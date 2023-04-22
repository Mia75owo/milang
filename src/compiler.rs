use crate::prelude::*;

use cranelift::{
    codegen::verify_function,
    prelude::{settings::FlagsOrIsa, *},
};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};

pub struct Compiler {
    pub builder_context: FunctionBuilderContext,
    pub ctx: codegen::Context,
    pub module: ObjectModule,
    pub scope: ScopeRoot,

    pub variable_index: usize,
}

impl Default for Compiler {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("Host machine is not supported: {msg}");
        });

        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder =
            ObjectBuilder::new(isa, "milang", cranelift_module::default_libcall_names()).unwrap();

        let module = ObjectModule::new(builder);

        let builder_context = FunctionBuilderContext::new();
        let ctx = module.make_context();
        let scope = ScopeRoot::default();

        Self {
            builder_context,
            ctx,
            module,
            scope,
            variable_index: 0,
        }
    }
}

impl Compiler {
    pub fn compile(mut self, input: &str) -> Result<ObjectProduct, String> {
        let file = parser::file(input).map_err(|e| e.to_string())?;

        /*
        for expr in &file {
            println!("{expr:?}");
        }
        */

        // Generate a fake main function to encapsulate the code
        let main_func = FunctionExpr {
            name: "main".to_owned(),
            params: vec![],
            return_type: TypeExpr::Ident("i32".to_owned()),
            stmts: file,
        };
        declare_function(&mut self.module, &mut self.scope, ROOT_PATH, &main_func);
        self.compile_function(main_func, ROOT_PATH)?;

        let object = self.module.finish();
        Ok(object)
    }

    pub fn compile_function(&mut self, func: FunctionExpr, scope: &str) -> Result<(), String> {
        let FunctionExpr {
            name,
            params,
            return_type,
            stmts,
        } = func;

        // Declare the param types
        for (_name, type_name) in &params {
            let ptype =
                LType::parse_basic_ident(type_name).expect("Failed to parse type: '{type_name}'!");
            let ptype = ptype.to_type();

            self.ctx.func.signature.params.push(AbiParam::new(ptype));
        }

        let ret_type =
            LType::parse_basic_ident(&return_type).expect("Failed to parse type: '{type_name}'!");
        let ret_type = ret_type.to_type();
        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(ret_type));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);

        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let function_scope = self.scope.create_scope_for_variable_at(scope, &name);

        for expr in &stmts {
            match expr {
                Expr::Function(func) => {
                    declare_function(&mut self.module, &mut self.scope, scope, func)
                }
                Expr::DefFunc(def) => {
                    declare_function_from_func_def(&mut self.module, &mut self.scope, scope, def)
                }
                _ => (),
            }
        }

        let mut func_compiler = FunctionCompiler::new(
            &mut self.module,
            &mut self.scope,
            &mut self.variable_index,
            builder,
            &function_scope,
        );

        func_compiler.declare_parameter_variables(entry_block, &params);

        for expr in stmts {
            func_compiler.translate_expr(expr);
        }

        let functions_to_compile = func_compiler.destroy();

        // TODO: use name convention for scoping
        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        let flags = settings::Flags::new(settings::builder());
        let flags_or_isa = FlagsOrIsa {
            flags: &flags,
            isa: None,
        };

        println!("{}", self.ctx.func.display());

        verify_function(&self.ctx.func, flags_or_isa).map_err(|e| e.to_string())?;

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        self.module.clear_context(&mut self.ctx);
        for (path, func) in functions_to_compile {
            self.compile_function(func, &path)?;
        }

        Ok(())
    }
}

pub struct FunctionCompiler<'a> {
    pub builder: FunctionBuilder<'a>,
    pub module: &'a mut ObjectModule,
    pub scope: &'a mut ScopeRoot,
    pub variable_index: &'a mut usize,
    pub current_scope: String,

    pub functions_to_compile: Vec<(String, FunctionExpr)>,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(
        module: &'a mut ObjectModule,
        scope: &'a mut ScopeRoot,
        variable_index: &'a mut usize,
        builder: FunctionBuilder<'a>,
        current_scope: &str,
    ) -> Self {
        Self {
            builder,
            module,
            scope,
            variable_index,
            current_scope: current_scope.to_owned(),
            functions_to_compile: vec![],
        }
    }

    pub fn destroy(self) -> Vec<(String, FunctionExpr)> {
        self.builder.finalize();
        self.functions_to_compile
    }

    pub fn translate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Literal(literal) => {
                let imm: i64 = literal.parse().unwrap();
                self.builder.ins().iconst(types::I64, imm)
            }
            Expr::Char(c) => {
                let c = if c.len() == 1 {
                    c.chars().next().unwrap()
                } else {
                    panic!("Failed to parse char: '{c}'");
                };

                let imm = c as u8;
                self.builder.ins().iconst(types::I8, imm as i64)
            }
            Expr::Array(ty, values) => {
                let ltype = LType::parse_type(&ty).unwrap();
                let type_size = LType::byte_size(&ltype);
                let bytes = type_size * values.len();

                let ss = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: bytes as u32,
                });

                for (i, value) in values.into_iter().enumerate() {
                    let val = self.translate_expr(value);
                    let val = cast_value(val, ltype.to_type(), true, &mut self.builder);

                    let offset = (i * type_size) as i32;
                    self.builder.ins().stack_store(val, ss, offset);
                }

                self.builder.ins().stack_addr(types::I64, ss, 0)
            }
            Expr::ArrayAccess(val, idx, ty) => {
                let val = self.translate_expr(*val);
                let idx = self.translate_expr(*idx);
                let idx = cast_value(idx, types::I64, false, &mut self.builder);

                let addr = self.builder.ins().iadd(val, idx);

                let load_type = if let Some(ty) = ty {
                    LType::parse_type(&ty).unwrap().to_type()
                } else {
                    types::I64
                };

                self.builder
                    .ins()
                    .load(load_type, MemFlags::new(), addr, 0)
            }
            Expr::String(s) => {
                let mut bytes = s.as_bytes().to_vec();
                bytes.push(b'\0');
                let bytes = bytes.into_boxed_slice();

                let mut data_ctx = DataContext::new();

                data_ctx.define(bytes);
                let msg_id = self.module.declare_anonymous_data(false, false).unwrap();
                let _ = self.module.define_data(msg_id, &data_ctx);
                let local_msg_id = self.module.declare_data_in_func(msg_id, self.builder.func);
                self.builder.ins().global_value(types::I64, local_msg_id)
            }
            Expr::Add(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

                self.builder.ins().iadd(lhs, rhs)
            }
            Expr::Sub(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

                self.builder.ins().isub(lhs, rhs)
            }
            Expr::Mul(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

                self.builder.ins().imul(lhs, rhs)
            }
            Expr::Div(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

                self.builder.ins().udiv(lhs, rhs)
            }
            Expr::Eq(lhs, rhs) => self.translate_icmp(IntCC::Equal, *lhs, *rhs),
            Expr::Ne(lhs, rhs) => self.translate_icmp(IntCC::NotEqual, *lhs, *rhs),
            Expr::Lt(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThan, *lhs, *rhs),
            Expr::Le(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs),
            Expr::Gt(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThan, *lhs, *rhs),
            Expr::Ge(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *lhs, *rhs),
            Expr::And(lhs, rhs) => self.translate_and(*lhs, *rhs),
            Expr::Or(lhs, rhs) => self.translate_or(*lhs, *rhs),
            Expr::Call(name, args) => self.translate_call(&name, args),
            Expr::GlobalDataAddr(_name) => {
                // NOTE: this is just used to test new features at the moment
                let ss = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: 4,
                });

                let val = self.builder.ins().iconst(types::I8, 1);
                self.builder.ins().stack_store(val, ss, 0);
                self.builder.ins().stack_store(val, ss, 1);
                self.builder.ins().stack_store(val, ss, 2);
                self.builder.ins().stack_store(val, ss, 3);

                let new_val = self.builder.ins().stack_load(types::I64, ss, 0);

                new_val
            }
            Expr::Identifier(name) => {
                let var = self
                    .scope
                    .find_variable_at(&self.current_scope, &name)
                    .unwrap_or_else(|| panic!("Variable '{name}' not defined!"));

                let val = var
                    .get_cl_int_var()
                    .unwrap_or_else(|| panic!("Failed to get value from variable '{name}'"));

                self.builder.use_var(val)
            }
            Expr::Assign(var, expr) => match *var {
                Expr::Identifier(name) => self.translate_assign(&name, *expr),
                Expr::ArrayAccess(arr, idx, ty) => self.translate_assign_array(*arr, *idx, *expr, ty),
                _ => panic!("Can not assign to value!"),
            },
            Expr::DefineVar(name_and_type, expr) => {
                self.translate_variable_declaration(&name_and_type, *expr)
            }
            Expr::IfElse(condition, then_body, else_body) => {
                self.translate_if_else(*condition, then_body, else_body)
            }
            Expr::WhileLoop(condition, loop_body) => {
                self.translate_while_loop(*condition, loop_body)
            }
            Expr::DefFunc(func) => {
                let func_type = LType::parse_function(&func).unwrap();
                let func_value = LValue::Function(
                    LFunctionValue::gen_from_function_type(func_type.clone(), self.module).unwrap(),
                );

                let variable = LVariable {
                    ltype: func_type,
                    lvalue: func_value,
                };

                self.scope
                    .insert_variable_at(&self.current_scope, &func.name, variable);

                self.builder.ins().iconst(types::I64, 0)
            }
            Expr::Return(expr) => {
                let ret_type = self
                    .builder
                    .func
                    .signature
                    .returns
                    .first()
                    .unwrap()
                    .value_type;

                let ret = self.translate_expr(*expr);
                let ret = cast_value(ret, ret_type, true, &mut self.builder);
                self.builder.ins().return_(&[ret]);

                ret
            }

            Expr::Function(func) => {
                declare_function(self.module, self.scope, &self.current_scope, &func);
                self.functions_to_compile
                    .push((self.current_scope.clone(), func));

                self.builder.ins().iconst(types::I64, 0)
            }
        }
    }

    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);

        let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

        self.builder.ins().icmp(cmp, lhs, rhs)
    }
    fn translate_and(&mut self, lhs: Expr, rhs: Expr) -> Value {
        // (x && y)
        // (x == 1 && y == 1)
        // ((x + y) == 2)

        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);

        let lhs = self.builder.ins().icmp_imm(IntCC::Equal, lhs, 1);
        let rhs = self.builder.ins().icmp_imm(IntCC::Equal, rhs, 1);

        let combined = self.builder.ins().iadd(lhs, rhs);
        let res = self.builder.ins().icmp_imm(IntCC::Equal, combined, 2);

        res
    }
    fn translate_or(&mut self, lhs: Expr, rhs: Expr) -> Value {
        // (x || y)
        // (x == 1 || y == 1)
        // ((x + y) > 0)

        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);

        let lhs = self.builder.ins().icmp_imm(IntCC::Equal, lhs, 1);
        let rhs = self.builder.ins().icmp_imm(IntCC::Equal, rhs, 1);

        let combined = self.builder.ins().iadd(lhs, rhs);
        let res = self
            .builder
            .ins()
            .icmp_imm(IntCC::SignedGreaterThan, combined, 0);

        res
    }

    // ===================
    // Translate Variables
    // ===================

    fn translate_variable_declaration(&mut self, variable: &NameType, expr: Expr) -> Value {
        let value = self.translate_expr(expr);
        let variable = self.declare_variable(variable);

        let value = cast_value(value, variable.ltype.to_type(), true, &mut self.builder);

        self.builder.def_var(
            variable
                .get_cl_int_var()
                .expect("Variable is not of type int!"),
            value,
        );
        value
    }

    fn translate_assign(&mut self, variable: &str, expr: Expr) -> Value {
        let variable = self
            .scope
            .find_variable_at(&self.current_scope, variable)
            .unwrap_or_else(|| panic!("Did not find variable in scope: '{}'", &variable));

        let value = self.translate_expr(expr);
        let value = cast_value(value, variable.ltype.to_type(), true, &mut self.builder);

        self.builder.def_var(
            variable
                .get_cl_int_var()
                .expect("Variable is not of type int!"),
            value,
        );
        value
    }
    fn translate_assign_array(&mut self, arr_val: Expr, arr_idx: Expr, expr: Expr, ty: Option<TypeExpr>) -> Value {
        let arr_val = self.translate_expr(arr_val);
        let arr_idx = self.translate_expr(arr_idx);
        let addr = self.builder.ins().iadd(arr_val, arr_idx);

        let mut val = self.translate_expr(expr);
        if let Some(ty) = ty {
            let cl_type = LType::parse_type(&ty).unwrap().to_type();
            val = cast_value(val, cl_type, true, &mut self.builder);
        }


        self.builder.ins().store(MemFlags::new(), val, addr, 0);
        self.builder.ins().iconst(types::I64, 0)
    }

    // ======================
    // Translate Control flow
    // ======================

    fn translate_if_else(
        &mut self,
        condition: Expr,
        then_body: Vec<Expr>,
        else_body: Vec<Expr>,
    ) -> Value {
        let condition_value = self.translate_expr(condition);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        let int_type = self.module.target_config().pointer_type();
        /*
        self.builder.append_block_param(merge_block, int_type);
        */

        self.builder
            .ins()
            .brif(condition_value, then_block, &[], else_block, &[]);

        // Then
        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        /*
        let mut then_return = self.builder.ins().iconst(int_type, 0);
        for expr in then_body {
            then_return = self.translate_expr(expr);
        }

        self.builder.ins().jump(merge_block, &[then_return]);
        */
        for expr in then_body {
            self.translate_expr(expr);
        }
        self.builder.ins().jump(merge_block, &[]);

        // Else
        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        /*
        let mut else_return = self.builder.ins().iconst(int_type, 0);
        for expr in else_body {
            else_return = self.translate_expr(expr);
        }

        self.builder.ins().jump(merge_block, &[else_return]);
        */
        for expr in else_body {
            self.translate_expr(expr);
        }
        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        /*
        let phi = self.builder.block_params(merge_block)[0];
        phi
        */
        self.builder.ins().iconst(int_type, 0)
    }

    fn translate_while_loop(&mut self, condition: Expr, loop_body: Vec<Expr>) -> Value {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        // Header
        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.translate_expr(condition);
        self.builder
            .ins()
            .brif(condition_value, body_block, &[], exit_block, &[]);

        // Body
        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);
        for expr in loop_body {
            self.translate_expr(expr);
        }

        self.builder.ins().jump(header_block, &[]);

        // Exit
        self.builder.switch_to_block(exit_block);

        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);

        let int_type = self.module.target_config().pointer_type();
        self.builder.ins().iconst(int_type, 0)
    }

    fn translate_call(&mut self, name: &str, args: Vec<Expr>) -> Value {
        let func = self
            .scope
            .find_variable_at(&self.current_scope, name)
            .unwrap_or_else(|| panic!("Did not find function '{name}' in scope!"));

        let func = match func.lvalue {
            LValue::Function(f) => f,
            _ => panic!("'{name}' is not a function!"),
        };

        assert!(args.len() == func.signature.params.len());

        let local_callee = func.declare_in_func(self.module, &mut self.builder);

        let mut arg_values = Vec::new();
        for (arg, param) in args.into_iter().zip(func.signature.params.iter()) {
            let val = self.translate_expr(arg);
            let val = cast_value(val, param.value_type, true, &mut self.builder);
            arg_values.push(val);
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call)[0]
    }

    fn declare_variable(&mut self, variable: &NameType) -> LVariable {
        let (str_var_name, str_var_type) = variable;

        let var = Variable::new(*self.variable_index);
        *self.variable_index += 1;

        let var_type = LType::parse_type(str_var_type)
            .unwrap_or_else(|| panic!("Failed to parse type: '{:?}'", &str_var_type));

        let value = LValue::Int(LInt {
            //raw: 0,
            cl_repr: var,
        });
        let variable = LVariable {
            ltype: var_type.clone(),
            lvalue: value,
        };

        self.scope
            .insert_variable_at(&self.current_scope, str_var_name, variable.clone());
        self.builder.declare_var(var, var_type.to_type());

        variable
    }
    fn declare_parameter_variables(&mut self, entry_block: Block, params: &[NameType]) {
        for (i, param) in params.iter().enumerate() {
            let var = self.declare_variable(param);
            self.builder.def_var(
                var.get_cl_int_var().unwrap(),
                self.builder.block_params(entry_block)[i],
            );
        }
    }
}
fn declare_function(
    module: &mut ObjectModule,
    scope_root: &mut ScopeRoot,
    current_scope: &str,
    func: &FunctionExpr,
) {
    let new_func = DefFuncExpr {
        name: func.name.to_owned(),
        params: func.params.clone(),
        return_type: func.return_type.to_owned(),
    };

    let func_type = LType::parse_function(&new_func).unwrap();
    let func_value = LValue::Function(
        LFunctionValue::gen_from_function_type(func_type.clone(), module).unwrap(),
    );

    let variable = LVariable {
        ltype: func_type,
        lvalue: func_value,
    };

    scope_root.insert_variable_at(current_scope, &func.name, variable);
}
fn declare_function_from_func_def(
    module: &mut ObjectModule,
    scope_root: &mut ScopeRoot,
    current_scope: &str,
    func: &DefFuncExpr,
) {
    let new_func = DefFuncExpr {
        name: func.name.to_owned(),
        params: func.params.clone(),
        return_type: func.return_type.to_owned(),
    };

    let func_type = LType::parse_function(&new_func).unwrap();
    let func_value = LValue::Function(
        LFunctionValue::gen_from_function_type(func_type.clone(), module).unwrap(),
    );

    let variable = LVariable {
        ltype: func_type,
        lvalue: func_value,
    };

    scope_root.insert_variable_at(current_scope, &func.name, variable);
}
