use crate::compiler::Compiler;

fn compile(input: &str) -> Result<cranelift_object::ObjectProduct, String> {
    let translator = Compiler::default();
    translator.compile(input)
}

#[test]
fn compile_basic() {
    let code = r#"
fn main() -> (i32) {
    return 0
}
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_variable() {
    let code = r#"
fn main() -> (i32) {
    foo: i32 = 123
    return 0
}
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_variable_use() {
    let code = r#"
fn main() -> (i32) {
    foo: i32 = 0
    return foo
}
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_ops() {
    let code = r#"
fn main() -> (i32) {
    a: i32 = 5 + 5
    b: i32 = 5 - 5
    c: i32 = 5 * 5
    d: i32 = 5 / 5

    e: i32 = 5 == 5
    f: i32 = 5 != 5
    g: i32 = 5 < 5
    h: i32 = 5 <= 5
    i: i32 = 5 > 5
    j: i32 = 5 >= 5


    k: i32 = 5 + 5 * 10
    l: i32 = 5 * 5 + 10
    m: i32 = 5 >= 5 * 3

    return 0
}
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_op_paren() {
    let code = r#"
fn main() -> (i32) {
    a: i32 = (5)
    b: i32 = ((((5))))
    c: i32 = (5 * 5)
    d: i32 = (5 * 5) + 5
    e: i32 = (5 * 5) + (5)
    f: i32 = (((5 + 5)) + (2 * 3))

    return 0
}
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_define_extern_function() {
    let code = r#"
fn main() -> (i32) {
    fn putchar(char: i32) -> (i32);

    return 0
}
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_call_extern_function() {
    let code = r#"
fn main() -> (i32) {
    fn putchar(char: i32) -> (i32);

    putchar(1)
    putchar(2)
    putchar(3)
    putchar(4)

    return 0
}
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_typecheck_true() {
    let code = r#"
fn main() -> (i32) {
    a: u8 = 5
    b: u8 = a
    c: u16 = 5
    d: u16 = c
    e: u32 = 5
    f: u32 = e
    g: u64 = 5
    h: u64 = g

    i: i8 = 5
    j: i8 = i
    k: i16 = 5
    l: i16 = k
    m: i32 = 5
    n: i32 = m
    o: i64 = 5
    p: i64 = o

    return 0
}
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_typecheck_false() {
    // TODO: falsy typechecks
}
