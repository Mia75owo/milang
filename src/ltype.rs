use crate::prelude::*;
use cranelift::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub struct LFunctionType {
    pub name: String,
    pub params: Vec<(String, LType)>,
    pub return_type: Box<LType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LArrayType {
    pub ltype: Box<LType>,
    pub size: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,

    LArray(LArrayType),
    LStructure,
    LFunction(LFunctionType),
}

impl LType {
    pub fn parse_basic_ident(input: &TypeExpr) -> Option<LType> {
        match input {
            TypeExpr::Ident(ident) => match ident.as_str() {
                "i8" => Some(LType::I8),
                "i16" => Some(LType::I16),
                "i32" => Some(LType::I32),
                "i64" => Some(LType::I64),
                "u8" => Some(LType::U8),
                "u16" => Some(LType::U16),
                "u32" => Some(LType::U32),
                "u64" => Some(LType::U64),
                "f32" => Some(LType::F32),
                "f64" => Some(LType::F64),

                _ => None,
            },
            TypeExpr::Array(_, _) => None,
        }
    }
    pub fn parse_type(input: &TypeExpr) -> Option<LType> {
        match input {
            ident @ TypeExpr::Ident(_) => Self::parse_basic_ident(ident),
            TypeExpr::Array(ltype, size) => {
                let ltype = Self::parse_type(ltype).unwrap();
                let ltype = Box::new(ltype);

                let size: usize = match *size.clone() {
                    Expr::Literal(l) => l.parse().unwrap(),
                    _ => panic!("Failed to parse array size!"),
                };

                let ret = LType::LArray(LArrayType { ltype, size });

                Some(ret)
            }
        }
    }
    pub fn parse_function(input: &DefFuncExpr) -> Option<LType> {
        let DefFuncExpr {
            name,
            params,
            return_type,
        } = input;
        let func = LFunctionType {
            name: name.clone(),
            params: params
                .iter()
                .map(|e| {
                    (
                        e.0.clone(),
                        LType::parse_basic_ident(&e.1)
                            .unwrap_or_else(|| panic!("Failed to parse type '{:?}'!", &e.1)),
                    )
                })
                .collect(),
            return_type: Box::new(
                LType::parse_basic_ident(return_type)
                    .unwrap_or_else(|| panic!("Failed to parse type '{:?}'!", &return_type)),
            ),
        };
        Some(LType::LFunction(func))
    }
    pub fn to_type(&self) -> types::Type {
        match self {
            LType::I8 => types::I8,
            LType::I16 => types::I16,
            LType::I32 => types::I32,
            LType::I64 => types::I64,
            LType::U8 => types::I8,
            LType::U16 => types::I16,
            LType::U32 => types::I32,
            LType::U64 => types::I64,
            LType::F32 => types::F32,
            LType::F64 => types::F64,

            LType::LStructure => panic!("Structure type not implemented!"),
            LType::LFunction(_) => panic!("Function type not implemented!"),
            LType::LArray(_) => types::I64,
        }
    }
    pub fn byte_size(&self) -> usize {
        match self {
            LType::I8 => 1,
            LType::I16 => 2,
            LType::I32 => 4,
            LType::I64 => 8,
            LType::U8 => 1,
            LType::U16 => 2,
            LType::U32 => 4,
            LType::U64 => 8,
            LType::F32 => 4,
            LType::F64 => 8,

            LType::LStructure => panic!("Structure size not implemented!"),
            LType::LFunction(_) => panic!("Function size not implemented!"),
            LType::LArray(arr) => {
                let size = Self::byte_size(&arr.ltype);
                size * arr.size
            }
        }
    }
}
