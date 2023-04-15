use crate::compiler::Compiler;
use crate::prelude::*;
use crate::scope::*;

// ======
// SCOPES
// ======

#[test]
fn test_scope_create() {
    let mut root = ScopeRoot::default();
    let path = root.create_scope_at(ROOT_PATH, "foo");
    assert_eq!(path, "#@#foo");
}

#[test]
fn test_scope_create2() {
    let mut root = ScopeRoot::default();
    let foo_path = root.create_scope_at(ROOT_PATH, "foo");
    let baa_path = root.create_scope_at(&foo_path, "baa");
    assert_eq!(baa_path, "#@#foo#baa");
}

#[test]
fn test_scope_find() {
    let root_var = LVariable {
        ltype: LType::I8,
        lvalue: LValue::Dummy,
    };
    let foo_var = LVariable {
        ltype: LType::I16,
        lvalue: LValue::Dummy,
    };
    let baa_var = LVariable {
        ltype: LType::I32,
        lvalue: LValue::Dummy,
    };

    let mut root = ScopeRoot::default();
    let root_var_path = root.insert_variable_at(ROOT_PATH, "root_var", root_var.clone());
    let foo_path = root.create_scope_at(ROOT_PATH, "foo");
    let foo_var_path = root.insert_variable_at(&foo_path, "foo_var", foo_var.clone());
    let baa_path = root.create_scope_at(&foo_path, "baa");
    let baa_var_path = root.insert_variable_at(&baa_path, "baa_var", baa_var.clone());

    let got = root.get_variable(&root_var_path).unwrap();
    assert_eq!(root_var, got);
    let got = root.get_variable(&foo_var_path).unwrap();
    assert_eq!(foo_var, got);
    let got = root.get_variable(&baa_var_path).unwrap();
    assert_eq!(baa_var, got);
}

// ===========
// COMPILATION
// ===========

fn compile(input: &str) -> Result<cranelift_object::ObjectProduct, String> {
    let translator = Compiler::default();
    translator.compile(input)
}

#[test]
fn compile_basic() {
    let code = r#"
return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_variable() {
    let code = r#"
foo: i32 = 123
return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_variable_use() {
    let code = r#"
foo: i32 = 0
return foo
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_ops() {
    let code = r#"
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
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_op_paren() {
    let code = r#"
a: i32 = (5)
b: i32 = ((((5))))
c: i32 = (5 * 5)
d: i32 = (5 * 5) + 5
e: i32 = (5 * 5) + (5)
f: i32 = (((5 + 5)) + (2 * 3))

return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_define_extern_function() {
    let code = r#"
fn putchar(char: i32) -> (i32);

return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_call_extern_function() {
    let code = r#"
fn putchar(char: i32) -> (i32);

putchar(1)
putchar(2)
putchar(3)
putchar(4)

return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_typecheck_true() {
    let code = r#"
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
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_typecheck_false() {
    // TODO: falsy typechecks
}

#[test]
fn compile_call_function() {
    let code = r#"
fn foo() -> (i32) {
    return 0
}

foo()

return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_get_value_from_function_call() {
    let code = r#"
fn foo() -> (i32) {
    return 105
}

x: i32 = foo()

return 0
    "#;
    compile(code).unwrap();
}
