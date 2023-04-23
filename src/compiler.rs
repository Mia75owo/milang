use crate::prelude::*;

use std::collections::{HashMap, HashSet};

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
            modifiers: HashSet::from([FunctionModifier::NoMangle]),
        };
        // Declare and compile the function
        declare_function(&mut self.module, &mut self.scope, ROOT_PATH, &main_func);
        self.compile_function(main_func, ROOT_PATH)?;

        let object = self.module.finish();
        Ok(object)
    }

    /// Compile a function from a FunctionExpr and the current scope
    pub fn compile_function(&mut self, func: FunctionExpr, scope: &str) -> Result<(), String> {
        let FunctionExpr {
            name,
            params,
            return_type,
            stmts,
            modifiers,
        } = func;

        // Declare the param types
        for (_name, type_name) in &params {
            let ptype =
                LType::parse_basic_ident(type_name).expect("Failed to parse type: '{type_name}'!");
            let ptype = ptype.to_type();

            self.ctx.func.signature.params.push(AbiParam::new(ptype));
        }

        // Define the function return type
        let ret_type =
            LType::parse_basic_ident(&return_type).expect("Failed to parse type: '{type_name}'!");
        let ret_type = ret_type.try_to_type();
        if let Some(ret_type) = ret_type {
            self.ctx
                .func
                .signature
                .returns
                .push(AbiParam::new(ret_type));
        }

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block with the function params
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let function_scope = self.scope.create_scope_for_variable_at(scope, &name);

        // Pre-define functions in scope
        for expr in &stmts {
            match expr {
                Expr::Function(func) => {
                    declare_function(&mut self.module, &mut self.scope, &function_scope, func)
                }
                Expr::DefFunc(def) => declare_function_from_func_def(
                    &mut self.module,
                    &mut self.scope,
                    &function_scope,
                    def,
                ),
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

        // Add the function parameters to scope
        func_compiler.declare_parameter_variables(entry_block, &params);

        // Translate the function expressions
        for expr in stmts {
            func_compiler.translate_statement(expr);
        }

        // Get a list of functions in the function to translate later
        let functions_to_compile = func_compiler.destroy();

        let path_in_module = if modifiers.contains(&FunctionModifier::NoMangle) {
            name
        } else {
            ScopeRoot::path_convention(&function_scope)
        };

        let id = self
            .module
            .declare_function(&path_in_module, Linkage::Export, &self.ctx.func.signature)
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

        // Translate all functions, defined in the function
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

    pub importet_functions: HashMap<String, codegen::ir::FuncRef>,
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
            importet_functions: HashMap::new(),
            functions_to_compile: vec![],
        }
    }

    /// Destroy the function builder and return a list of "sub-functions"
    pub fn destroy(self) -> Vec<(String, FunctionExpr)> {
        self.builder.finalize();
        self.functions_to_compile
    }

    /// Translates top-level statements
    pub fn translate_statement(&mut self, expr: Expr) {
        match expr {
            // Function calls 'foo()'
            Expr::Call(name, args) => _ = self.translate_call(&name, args),
            // Variable assign 'a = 123'
            Expr::Assign(var, expr) => match *var {
                Expr::Identifier(name) => self.translate_assign(&name, *expr),
                Expr::ArrayAccess(arr, idx, ty) => {
                    self.translate_assign_array(*arr, *idx, *expr, ty)
                }
                _ => panic!("Can not assign to value!"),
            },
            // Variable definitions 'a: i32 = 123'
            Expr::DefVar(name_and_type, expr) => {
                self.translate_variable_declaration(&name_and_type, *expr)
            }
            // If-Else 'if 1 {} else {}'
            Expr::IfElse(condition, then_body, else_body) => {
                self.translate_if_else(*condition, then_body, else_body)
            }
            // While Loops 'while 1 {}'
            Expr::WhileLoop(condition, loop_body) => {
                self.translate_while_loop(*condition, loop_body)
            }
            // Function definitions 'fn foo() -> (i32)'
            Expr::DefFunc(_func) => {
                // This is unneeded because it gets pre-declared

                //declare_function_from_func_def(self.module, self.scope, &self.current_scope, &func);
            }
            // Return statements 'return 123'
            Expr::Return(expr) => {
                // Check if function and return-expr both return void or a value
                if self.builder.func.signature.returns.first().is_some() != expr.is_some() {
                    if expr.is_some() {
                        panic!("Can not return a value from a function with void return type!");
                    } else {
                        panic!("Can not return void from a function with a value return type!");
                    }
                }

                if let Some(ret_type) = self.builder.func.signature.returns.first() {
                    // Return a value
                    let ret_type = ret_type.value_type;
                    let ret = self.translate_value(*expr.unwrap(), Some(ret_type));
                    let ret = cast_value(ret, ret_type, true, &mut self.builder);
                    self.builder.ins().return_(&[ret]);
                } else {
                    // Return void
                    self.builder.ins().return_(&[]);
                }
            }
            // Functions 'fn foo() -> (i32) {}'
            Expr::Function(func) => {
                // This is unneeded because it gets pre-declared

                //declare_function(self.module, self.scope, &self.current_scope, &func);
                self.functions_to_compile
                    .push((self.current_scope.clone(), func));
            }
            _ => panic!("Expression is not a statement!"),
        }
    }
    pub fn expr_value_type(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Literal(_) => types::I64,
            Expr::Char(_) => types::I8,
            Expr::Array(_, _) => types::I64,
            Expr::ArrayAccess(_, _, ty) => {
                if let Some(ty) = ty {
                    LType::parse_type(ty).unwrap().to_type()
                } else {
                    types::I64
                }
            }
            Expr::String(_) => types::I64,
            Expr::Add(lhs, rhs) => {
                let lhs = self.expr_value_type(lhs);
                let rhs = self.expr_value_type(rhs);
                choose_type(lhs, rhs)
            }
            Expr::Sub(lhs, rhs) => {
                let lhs = self.expr_value_type(lhs);
                let rhs = self.expr_value_type(rhs);
                choose_type(lhs, rhs)
            }
            Expr::Mul(lhs, rhs) => {
                let lhs = self.expr_value_type(lhs);
                let rhs = self.expr_value_type(rhs);
                choose_type(lhs, rhs)
            }
            Expr::Div(lhs, rhs) => {
                let lhs = self.expr_value_type(lhs);
                let rhs = self.expr_value_type(rhs);
                choose_type(lhs, rhs)
            }
            Expr::Eq(_, _) => types::I8,
            Expr::Ne(_, _) => types::I8,
            Expr::Lt(_, _) => types::I8,
            Expr::Le(_, _) => types::I8,
            Expr::Gt(_, _) => types::I8,
            Expr::Ge(_, _) => types::I8,
            Expr::And(_, _) => types::I8,
            Expr::Or(_, _) => types::I8,
            Expr::Call(name, _) => {
                let func = self
                    .scope
                    .find_variable_at(&self.current_scope, name)
                    .unwrap_or_else(|| panic!("Did not find function '{name}' in scope!"));

                let func = match func.lvalue {
                    LValue::Function(f) => f,
                    _ => panic!("'{name}' is not a function!"),
                };

                if let Some(ty) = func.signature.returns.get(0) {
                    ty.value_type
                } else {
                    types::INVALID
                }
            }
            Expr::TestVal(_name) => types::I64,
            Expr::Identifier(name) => {
                let var = self
                    .scope
                    .find_variable_at(&self.current_scope, name)
                    .unwrap_or_else(|| panic!("Variable '{name}' not defined!"));
                var.ltype.to_type()
            }
            _ => panic!("Expression is not a value!"),
        }
    }

    pub fn translate_value(&mut self, expr: Expr, try_type: Option<Type>) -> Value {
        match expr {
            // Literals '123'
            Expr::Literal(literal) => {
                let imm: i64 = literal.parse().unwrap();
                if let Some(ty) = try_type {
                    self.builder.ins().iconst(ty, imm)
                } else {
                    self.builder.ins().iconst(types::I64, imm)
                }
            }
            // Chars "'a'"
            Expr::Char(c) => {
                let c = if c.len() == 1 {
                    c.chars().next().unwrap()
                } else {
                    panic!("Failed to parse char: '{c}'");
                };

                let imm = c as u8;

                if let Some(ty) = try_type {
                    self.builder.ins().iconst(ty, imm as i64)
                } else {
                    self.builder.ins().iconst(types::I8, imm as i64)
                }
            }
            // Arrays '@i64[1, 2, 3]'
            Expr::Array(ty, values) => {
                let ltype = LType::parse_type(&ty).unwrap();
                let type_size = LType::byte_size(&ltype);
                let bytes = type_size * values.len();

                let ss = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: bytes as u32,
                });

                for (i, value) in values.into_iter().enumerate() {
                    let val = self.translate_value(value, Some(ltype.to_type()));
                    let val = cast_value(val, ltype.to_type(), true, &mut self.builder);

                    let offset = (i * type_size) as i32;
                    self.builder.ins().stack_store(val, ss, offset);
                }

                self.builder.ins().stack_addr(types::I64, ss, 0)
            }
            // Array accesses 'foo@i8[0]'
            Expr::ArrayAccess(val, idx, ty) => {
                let val = self.translate_value(*val, Some(types::I64));
                let idx = self.translate_value(*idx, Some(types::I64));
                let idx = cast_value(idx, types::I64, false, &mut self.builder);

                let addr = self.builder.ins().iadd(val, idx);

                let load_type = if let Some(ty) = ty {
                    LType::parse_type(&ty).unwrap().to_type()
                } else {
                    types::I64
                };

                self.builder.ins().load(load_type, MemFlags::new(), addr, 0)
            }
            // Strings '"foo"'
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
            // Binary add '1+1'
            Expr::Add(lhs, rhs) => {
                let ty = choose_type(self.expr_value_type(&lhs), self.expr_value_type(&rhs));

                let lhs = self.translate_value(*lhs, Some(ty));
                let rhs = self.translate_value(*rhs, Some(ty));

                let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

                self.builder.ins().iadd(lhs, rhs)
            }
            // Binary subtract '1-1'
            Expr::Sub(lhs, rhs) => {
                let ty = choose_type(self.expr_value_type(&lhs), self.expr_value_type(&rhs));

                let lhs = self.translate_value(*lhs, Some(ty));
                let rhs = self.translate_value(*rhs, Some(ty));

                let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

                self.builder.ins().isub(lhs, rhs)
            }
            // Binary multiplication '1*1'
            Expr::Mul(lhs, rhs) => {
                let ty = choose_type(self.expr_value_type(&lhs), self.expr_value_type(&rhs));

                let lhs = self.translate_value(*lhs, Some(ty));
                let rhs = self.translate_value(*rhs, Some(ty));

                let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

                self.builder.ins().imul(lhs, rhs)
            }
            // Binary division '1/1'
            Expr::Div(lhs, rhs) => {
                let ty = choose_type(self.expr_value_type(&lhs), self.expr_value_type(&rhs));

                let lhs = self.translate_value(*lhs, Some(ty));
                let rhs = self.translate_value(*rhs, Some(ty));

                let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

                self.builder.ins().udiv(lhs, rhs)
            }
            // Binary comparators '==' '!=' '<' '<=' '>' '>=' '&&' '||'
            Expr::Eq(lhs, rhs) => self.translate_icmp(IntCC::Equal, *lhs, *rhs),
            Expr::Ne(lhs, rhs) => self.translate_icmp(IntCC::NotEqual, *lhs, *rhs),
            Expr::Lt(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThan, *lhs, *rhs),
            Expr::Le(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs),
            Expr::Gt(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThan, *lhs, *rhs),
            Expr::Ge(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *lhs, *rhs),
            Expr::And(lhs, rhs) => self.translate_and(*lhs, *rhs),
            Expr::Or(lhs, rhs) => self.translate_or(*lhs, *rhs),
            // Function calls 'foo()'
            Expr::Call(name, args) => self
                .translate_call(&name, args)
                .expect("Trying to take value of void function!"),
            // For testing
            Expr::TestVal(_name) => {
                // NOTE: this is just used to test new features at the moment
                self.builder.ins().iconst(types::I64, 0)
            }
            // Identifiers 'foo'
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
            _ => panic!("Expression is not a value!"),
        }
    }

    // Translate variable compares
    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
        // Get a type for both values
        let ty = choose_type(self.expr_value_type(&lhs), self.expr_value_type(&rhs));

        let lhs = self.translate_value(lhs, Some(ty));
        let rhs = self.translate_value(rhs, Some(ty));

        let (lhs, rhs) = cast_types((lhs, rhs), &mut self.builder);

        self.builder.ins().icmp(cmp, lhs, rhs)
    }
    /// Translate '&&' operator
    fn translate_and(&mut self, lhs: Expr, rhs: Expr) -> Value {
        // (x && y)
        // (x == 1 && y == 1)
        // ((x + y) == 2)

        // Get a type for both values
        let ty = choose_type(self.expr_value_type(&lhs), self.expr_value_type(&rhs));

        let lhs = self.translate_value(lhs, Some(ty));
        let rhs = self.translate_value(rhs, Some(ty));

        /*
        let lhs = self.builder.ins().icmp_imm(IntCC::Equal, lhs, 1);
        let rhs = self.builder.ins().icmp_imm(IntCC::Equal, rhs, 1);

        let combined = self.builder.ins().iadd(lhs, rhs);
        self.builder.ins().icmp_imm(IntCC::Equal, combined, 2)
        */

        self.builder.ins().band(lhs, rhs)
    }
    /// Translate '||' operator
    fn translate_or(&mut self, lhs: Expr, rhs: Expr) -> Value {
        // (x || y)
        // (x == 1 || y == 1)
        // ((x + y) > 0)

        // Get a type for both values
        let ty = choose_type(self.expr_value_type(&lhs), self.expr_value_type(&rhs));

        let lhs = self.translate_value(lhs, Some(ty));
        let rhs = self.translate_value(rhs, Some(ty));

        let lhs = self.builder.ins().icmp_imm(IntCC::Equal, lhs, 1);
        let rhs = self.builder.ins().icmp_imm(IntCC::Equal, rhs, 1);

        /*
        let combined = self.builder.ins().iadd(lhs, rhs);
        self.builder
            .ins()
            .icmp_imm(IntCC::SignedGreaterThan, combined, 0)
        */

        self.builder.ins().bor(lhs, rhs)
    }

    // ===================
    // Translate Variables
    // ===================

    /// Translate a variable declaration
    fn translate_variable_declaration(&mut self, variable: &NameType, expr: Expr) {
        let variable = self.declare_variable(variable);

        let value = self.translate_value(expr, Some(variable.ltype.to_type()));
        let value = cast_value(value, variable.ltype.to_type(), true, &mut self.builder);

        self.builder.def_var(
            variable
                .get_cl_int_var()
                .expect("Variable is not of type int!"),
            value,
        );
    }

    /// Translate a variable assign
    fn translate_assign(&mut self, variable: &str, expr: Expr) {
        // Get the variable from scope
        let variable = self
            .scope
            .find_variable_at(&self.current_scope, variable)
            .unwrap_or_else(|| panic!("Did not find variable in scope: '{}'", &variable));

        let value = self.translate_value(expr, Some(variable.ltype.to_type()));
        let value = cast_value(value, variable.ltype.to_type(), true, &mut self.builder);

        self.builder.def_var(
            variable
                .get_cl_int_var()
                .expect("Variable is not of type int!"),
            value,
        );
    }

    /// Translate assign to arrays
    fn translate_assign_array(
        &mut self,
        arr_val: Expr,
        arr_idx: Expr,
        expr: Expr,
        ty: Option<TypeExpr>,
    ) {
        // Translate the array pointer
        let arr_val = self.translate_value(arr_val, Some(types::I64));
        // Translate the array index and compute the offset
        let arr_idx = self.translate_value(arr_idx, Some(types::I64));
        let addr = self.builder.ins().iadd(arr_val, arr_idx);

        // Get the type to store
        let val = if let Some(ty) = ty {
            let cl_type = LType::parse_type(&ty).unwrap().to_type();
            let val = self.translate_value(expr, Some(cl_type));
            cast_value(val, cl_type, true, &mut self.builder)
        } else {
            let val = self.translate_value(expr, Some(types::I64));
            cast_value(val, types::I64, true, &mut self.builder)
        };

        self.builder.ins().store(MemFlags::new(), val, addr, 0);
    }

    // ======================
    // Translate Control flow
    // ======================

    // Translate if-else statements
    fn translate_if_else(&mut self, condition: Expr, then_body: Vec<Expr>, else_body: Vec<Expr>) {
        let condition_value = self.translate_value(condition, Some(types::I8));

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

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
            self.translate_statement(expr);
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
            self.translate_statement(expr);
        }
        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        /*
        let phi = self.builder.block_params(merge_block)[0];
        phi
        */
    }

    // Translate while-loop statement
    fn translate_while_loop(&mut self, condition: Expr, loop_body: Vec<Expr>) {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        // Header
        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.translate_value(condition, Some(types::I8));
        self.builder
            .ins()
            .brif(condition_value, body_block, &[], exit_block, &[]);

        // Body
        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);
        for expr in loop_body {
            self.translate_statement(expr);
        }

        self.builder.ins().jump(header_block, &[]);

        // Exit
        self.builder.switch_to_block(exit_block);

        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);
    }

    /// Translate a function call
    fn translate_call(&mut self, name: &str, args: Vec<Expr>) -> Option<Value> {
        // Get the function from scope
        let func_path = self
            .scope
            .path_find_variable_at(&self.current_scope, name)
            .unwrap_or_else(|| panic!("Did not find function '{name}' in scope!"));
        let func = self.scope.get_variable(&func_path).unwrap();

        let func_lvalue = match func.lvalue {
            LValue::Function(f) => f,
            _ => panic!("'{name}' is not a function!"),
        };
        let func_ltype = match func.ltype {
            LType::LFunction(f) => f,
            _ => panic!("'{name}' is not a function!"),
        };

        assert!(args.len() == func_lvalue.signature.params.len());

        // Get the function signature or generate it
        let local_callee = if let Some(callee) = self.importet_functions.get(name) {
            *callee
        } else {
            let path_in_module = if func_ltype.modifiers.contains(&FunctionModifier::NoMangle) {
                name.to_owned()
            } else {
                ScopeRoot::path_convention(&func_path)
            };

            let callee = self
                .module
                .declare_function(&path_in_module, Linkage::Import, &func_lvalue.signature)
                .expect("Problem declaring function");
            let callee = self.module.declare_func_in_func(callee, self.builder.func);
            self.importet_functions.insert(name.to_owned(), callee);

            callee
        };

        // Translate the function params
        let mut arg_values = Vec::new();
        for (arg, param) in args.into_iter().zip(func_lvalue.signature.params.iter()) {
            let val = self.translate_value(arg, Some(param.value_type));
            let val = cast_value(val, param.value_type, true, &mut self.builder);
            arg_values.push(val);
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call).get(0).copied()
    }

    /// Declare a variable in scope
    fn declare_variable(&mut self, variable: &NameType) -> LVariable {
        let (var_name, str_var_type) = variable;

        // Create a new cranelift variable
        let var = Variable::new(*self.variable_index);
        *self.variable_index += 1;

        let var_type = LType::parse_type(str_var_type)
            .unwrap_or_else(|| panic!("Failed to parse type: '{:?}'", &str_var_type));

        let value = LValue::Int(LInt { cl_repr: var });
        let variable = LVariable {
            ltype: var_type.clone(),
            lvalue: value,
        };

        self.scope
            .insert_variable_at(&self.current_scope, var_name, variable.clone());
        self.builder.declare_var(var, var_type.to_type());

        variable
    }
    /// Declare parameters for a function in scope
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
/// Declare a function in scope
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
        modifiers: func.modifiers.clone(),
    };

    let func_type = LType::parse_function_def(&new_func).unwrap();
    let func_value = LValue::Function(
        LFunctionValue::gen_from_function_type(func_type.clone(), module).unwrap(),
    );

    let variable = LVariable {
        ltype: func_type,
        lvalue: func_value,
    };

    scope_root.insert_variable_at(current_scope, &func.name, variable);
}
/// Declare a function from a function definition in scope
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
        modifiers: func.modifiers.clone(),
    };

    let func_type = LType::parse_function_def(&new_func).unwrap();
    let func_value = LValue::Function(
        LFunctionValue::gen_from_function_type(func_type.clone(), module).unwrap(),
    );

    let variable = LVariable {
        ltype: func_type,
        lvalue: func_value,
    };

    scope_root.insert_variable_at(current_scope, &func.name, variable);
}
