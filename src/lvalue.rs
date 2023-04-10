use crate::prelude::*;

use cranelift::{
    codegen::ir::FuncRef,
    prelude::{AbiParam, FunctionBuilder, Signature, Variable},
};
use cranelift_module::{Linkage, Module};
use cranelift_object::ObjectModule;

#[derive(Clone)]
pub struct LInt {
    //pub raw: i64,
    pub cl_repr: Variable,
}
#[derive(Clone)]
pub struct LStructure {}
#[derive(Clone)]
pub struct LFunctionValue {
    pub name: String,
    pub signature: Signature,
}
impl LFunctionValue {
    pub fn gen_from_function_type(func_type: LType, module: &mut ObjectModule) -> Option<Self> {
        let mut sig = module.make_signature();

        let func_type = match func_type {
            LType::LFunction(func) => func,
            _ => return None,
        };

        for arg in &func_type.params {
            let ltype = arg.1.to_type();
            sig.params.push(AbiParam::new(ltype));
        }

        let ret_type = func_type.return_type.to_type();
        sig.returns.push(AbiParam::new(ret_type));

        let ret = Self {
            name: func_type.name,
            signature: sig,
        };
        Some(ret)
    }
    pub fn declare_in_func(
        &self,
        module: &mut ObjectModule,
        builder: &mut FunctionBuilder,
    ) -> FuncRef {
        let callee = module
            .declare_function(&self.name, Linkage::Import, &self.signature)
            .expect("Problem declaring function");
        
        module.declare_func_in_func(callee, builder.func)
    }
}

#[derive(Clone)]
pub enum LValue {
    Int(LInt),
    Structure(LStructure),
    Function(LFunctionValue),
}

#[derive(Clone)]
pub struct LVariable {
    pub ltype: LType,
    pub lvalue: LValue,
}
impl LVariable {
    pub fn get_cl_int_var(&self) -> Option<Variable> {
        match self.lvalue {
            LValue::Int(ref v) => Some(v.clone().cl_repr),
            _ => None,
        }
    }
}
