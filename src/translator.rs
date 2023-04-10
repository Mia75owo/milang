use cranelift::prelude::*;
use cranelift_module::{/*DataContext, */ Linkage, Module};
use cranelift_object::ObjectModule;

use crate::{
    cl_type::{cast_types, cast_value},
    ltype::LType,
    lvalue::{LFunctionValue, LInt, LValue, LVariable},
    parser::Expr,
    scope::Scope,
};

pub struct Translator<'a> {
    pub builder: FunctionBuilder<'a>,
    pub module: &'a mut ObjectModule,
    pub scope: &'a mut Scope,
    pub variable_index: usize,
}

impl<'a> Translator<'a> {
    pub fn translate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Literal(literal) => {
                let imm: i64 = literal.parse().unwrap();
                self.builder.ins().iconst(types::I64, imm)
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
            Expr::Call(name, args) => self.translate_call(&name, args),
            Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            Expr::Identifier(name) => {
                let var = self
                    .scope
                    .get_value_from_scope(&name)
                    .unwrap_or_else(|| panic!("Variable '{name}' not defined!"));

                let val = var
                    .get_cl_int_var()
                    .expect("Failed to get value from variable '{name}'");

                self.builder.use_var(val)
            }
            Expr::Assign(name, expr) => self.translate_assign(&name, *expr),
            Expr::DefineVar(name_and_type, expr) => self.translate_variable_declaration(&name_and_type, *expr),
            Expr::IfElse(condition, then_body, else_body) => {
                self.translate_if_else(*condition, then_body, else_body)
            }
            Expr::WhileLoop(condition, loop_body) => {
                self.translate_while_loop(*condition, loop_body)
            }
            Expr::DefFunc {
                name,
                params,
                return_type,
            } => {
                let func = Expr::DefFunc {
                    name: name.clone(),
                    params,
                    return_type,
                };

                let func_type = LType::parse_function(func).unwrap();
                let func_value = LValue::Function(
                    LFunctionValue::gen_from_function_type(func_type.clone(), self.module).unwrap(),
                );

                let variable = LVariable {
                    ltype: func_type,
                    lvalue: func_value,
                };

                self.scope.insert_variable(&name, variable);

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
        }
    }

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
        self.builder.append_block_param(merge_block, int_type);

        self.builder
            .ins()
            .brif(condition_value, then_block, &[], else_block, &[]);

        // Then
        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let mut then_return = self.builder.ins().iconst(int_type, 0);
        for expr in then_body {
            then_return = self.translate_expr(expr);
        }

        self.builder.ins().jump(merge_block, &[then_return]);

        // Else
        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let mut else_return = self.builder.ins().iconst(int_type, 0);
        for expr in else_body {
            else_return = self.translate_expr(expr);
        }

        self.builder.ins().jump(merge_block, &[else_return]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        let phi = self.builder.block_params(merge_block)[0];

        phi
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

    fn translate_variable_declaration(&mut self, variable: &(String, String), expr: Expr) -> Value {
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
        let value = self.translate_expr(expr);
        let variable = self
            .scope
            .get_value_from_scope(variable)
            .unwrap_or_else(|| panic!("Did not find variable in scope: '{}'", &variable));

        let value = cast_value(value, variable.ltype.to_type(), true, &mut self.builder);

        self.builder.def_var(
            variable
                .get_cl_int_var()
                .expect("Variable is not of type int!"),
            value,
        );
        value
    }

    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);
        self.builder.ins().icmp(cmp, lhs, rhs)
    }

    fn translate_call(&mut self, name: &str, args: Vec<Expr>) -> Value {
        let func = self
            .scope
            .get_value_from_scope(name)
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

    fn translate_global_data_addr(&mut self, name: String) -> Value {
        let sym = self
            .module
            .declare_data(&name, Linkage::Export, true, false)
            .expect("problem declaring data object");

        let local_id = self.module.declare_data_in_func(sym, self.builder.func);

        let pointer = self.module.target_config().pointer_type();
        self.builder.ins().symbol_value(pointer, local_id)
    }

    pub fn declare_variables(
        &mut self,
        params: &[(String, String)],
        return_val: (String, String),
        stmts: &[Expr],
        entry_block: Block,
    ) {
        for (i, variable) in params.iter().enumerate() {
            let variable = self.declare_variable(variable);

            let val = self.builder.block_params(entry_block)[i];
            self.builder
                .declare_var(variable.get_cl_int_var().unwrap(), variable.ltype.to_type());
            self.builder
                .def_var(variable.get_cl_int_var().unwrap(), val);

            self.variable_index += 1;
        }

        let variable = self.declare_variable(&return_val);
        let zero = self.builder.ins().iconst(variable.ltype.to_type(), 0);
        self.builder
            .def_var(variable.get_cl_int_var().unwrap(), zero);
        self.variable_index += 1;

        for expr in stmts {
            self.declare_variables_in_stmt(expr);
        }
    }

    fn declare_variable(&mut self, variable: &(String, String)) -> LVariable {
        let (str_var_name, str_var_type) = variable;

        let var = Variable::new(self.variable_index);
        self.variable_index += 1;

        let var_type = LType::parse_basic(str_var_type)
            .unwrap_or_else(|| panic!("Failed to parse type: '{}'", &str_var_type));

        let value = LValue::Int(LInt {
            //raw: 0,
            cl_repr: var,
        });
        let variable = LVariable {
            ltype: var_type.clone(),
            lvalue: value,
        };

        self.scope.insert_variable(str_var_name, variable.clone());
        self.builder.declare_var(var, var_type.to_type());

        variable
    }

    fn declare_variables_in_stmt(&mut self, expr: &Expr) {
        match expr {
            Expr::Assign(variable, expr) => {
                self.translate_assign(variable, *expr.clone());
            }
            Expr::IfElse(ref _condition, ref _then_body, ref _else_body) => {
                //for stmt in then_body {
                //declare_variables_in_stmt(int, builder, variables, index, stmt);
                //}
                //for stmt in else_body {
                //declare_variables_in_stmt(int, builder, variables, index, stmt);
                //}
                todo!()
            }
            Expr::WhileLoop(ref _condition, ref _loop_body) => {
                //for stmt in loop_body {
                //declare_variables_in_stmt(int, builder, variables, index, stmt);
                //}
                todo!()
            }
            _ => (),
        }
    }

    //fn val_type(&self, val: Value) -> Type {
    //self.builder.func.dfg.value_type(val)
    //}
}
