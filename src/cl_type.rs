use cranelift::prelude::{types, FunctionBuilder, InstBuilder, Value};

pub fn choose_type(type1: types::Type, type2: types::Type) -> types::Type {
    use types::*;

    match (type1, type2) {
        // Same type
        t if type1 == type2 => t.0,

        // I64 > t
        (I64, I8 | I16 | I32) | (I8 | I16 | I32, I64) => I64,
        // I32 > t
        (I32, I8 | I16) | (I8 | I16, I32) => I32,
        // I16 > t
        (I16, I8) | (I8, I16) => I16,

        // F64 > t
        (F64, F32) | (F32, F64) => F64,

        (t1, t2) => panic!("Can't choose between '{t1}' and '{t2}'"),
    }
}
pub fn choose_type_from_values(
    values: (Value, Value),
    builder: &mut FunctionBuilder,
) -> types::Type {
    let values_types = (
        builder.func.dfg.value_type(values.0),
        builder.func.dfg.value_type(values.1),
    );
    choose_type(values_types.0, values_types.1)
}

pub fn cast_value(
    value: Value,
    to: types::Type,
    signed: bool,
    builder: &mut FunctionBuilder,
) -> Value {
    let from = builder.func.dfg.value_type(value);
    match (from, to) {
        (_, _) if from == to => value,
        (_, _) if to.wider_or_equal(from) => {
            if signed {
                builder.ins().sextend(to, value)
            } else {
                builder.ins().uextend(to, value)
            }
        }
        (_, _) => builder.ins().ireduce(to, value),
    }
}

pub fn cast_types(values: (Value, Value), builder: &mut FunctionBuilder) -> (Value, Value) {
    let wanted_type = choose_type_from_values(values, builder);
    (
        cast_value(values.0, wanted_type, true, builder),
        cast_value(values.1, wanted_type, true, builder),
    )
}
