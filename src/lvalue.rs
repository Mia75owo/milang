use crate::prelude::*;

use cranelift::prelude::{AbiParam, Signature, Variable};
use cranelift_module::Module;
use cranelift_object::ObjectModule;

#[derive(Debug, Clone, PartialEq)]
pub struct LInt {
    pub cl_repr: Variable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LStructure {}

#[derive(Debug, Clone, PartialEq)]
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

        let ret_type = func_type.return_type.try_to_type();
        if let Some(ret_type) = ret_type {
            sig.returns.push(AbiParam::new(ret_type));
        }

        let ret = Self {
            name: func_type.name,
            signature: sig,
        };
        Some(ret)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    Int(LInt),
    Structure(LStructure),
    Function(LFunctionValue),
    Dummy,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LVariable {
    pub ltype: LType,
    pub lvalue: LValue,
}
impl LVariable {
    // Return the cranelift int variable
    pub fn get_cl_int_var(&self) -> Option<Variable> {
        match self.lvalue {
            LValue::Int(ref v) => Some(v.clone().cl_repr),
            _ => None,
        }
    }
}
