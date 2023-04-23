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

k: i32 = 5 == 5 && 6 == 6
l: i32 = 5 == 5 || 6 == 6

m: i32 = 5 + 5 * 10
n: i32 = 5 * 5 + 10
o: i32 = 5 >= 5 * 3

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
fn putchar(char: i32) -> (i32)

return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_call_extern_function() {
    let code = r#"
fn putchar(char: i32) -> (i32)

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
#[should_panic]
fn compile_typecheck_false() {
    let code = r#"
fn foo() -> (i32) {
    return 0
}

baa: i64 = foo

return 0
    "#;
    compile(code).unwrap();
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
fn compile_call_function_with_params() {
    let code = r#"
fn foo(a: i32) -> (i32) {
    return a
}

foo(123)

return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_call_void_function() {
    let code = r#"
fn foo() {
    return;
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

#[test]
fn compile_scoping_levels() {
    let code = r#"
fn a() -> (i32) {
    fn b() -> (i32) {
        fn c() -> (i32) {
            return 0
        }
        return c()
    }
    return b()
}

return a()
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_forward_declare() {
    let code = r#"
putchar('O')

fn putchar(char: i32) -> (i32)

return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_char() {
    let code = r#"
a: i8 = 'a'
b: i8 = 'b'
c: i8 = 'c'
nl: i8 = '\n'
cr: i8 = '\r'
bs: i8 = '\b'

return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_string() {
    let code = r#"
a: i64 = ""
b: i64 = "abc"
c: i64 = "abcdefghijklmnopqrstuvwxyz"
nl: i64 = "abc\ndef"
cr: i64 = "\"hi\""
bs: i64 = "\"\r\\foo\n\b\"\0"

return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_array_def() {
    let code = r#"
a: [i8] = @i8['a', 'b', 'c']
b: [i8; 3] = @i8['a', 'b', 'c']
return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_array_access() {
    let code = r#"
arr: [i8] = @i8['a', 'b', 'c']
a: i8 = arr[0]
b: i8 = arr[1]
c: i8 = arr[2]
return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_array_assign() {
    let code = r#"
arr: [i8] = @i8['a', 'b', 'c']
arr[0] = 'd'
arr[1] = 'e'
arr[2] = 'f'
return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_singleline_comment() {
    let code = r#"
// foo
return 0 // baa
// baz
// // / //// hi
    "#;
    compile(code).unwrap();
}
#[test]
fn compile_multiline_comment() {
    let code = r#"
/* foo */
return /*inbetween :>*/ 0 /* baa
baz
// // /* /* / / hi */
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_if_statement() {
    let code = r#"
fn putchar(c: i32) -> (i32)
if 1 {}
if 1 {
    putchar('y')
}
if 1 {
    putchar('y')
    putchar('y')
    putchar('y')
}
if 1 {
    if 1 {
        if 1 {
            putchar('y')
        }
        if 1 {
            putchar('y')
        }
    }
}
return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_if_else_statement() {
    let code = r#"
fn putchar(c: i32) -> (i32)
if 1 {} else {}
if 1 {
    putchar('y')
} else {
    putchar('n')
}
if 1 {
    putchar('y')
    putchar('y')
    putchar('y')
} else {
    putchar('n')
    putchar('n')
    putchar('n')
}
if 1 {
    if 1 {
        if 1 {
            putchar('y')
        } else {
            putchar('n')
        }
        if 1 {
            putchar('y')
        } else {
            putchar('n')
        }
    } else {
        if 1 {
            putchar('y')
        } else {
            putchar('n')
        }
        if 1 {
            putchar('y')
        } else {
            putchar('n')
        }
    }
} else {}
return 0
    "#;
    compile(code).unwrap();
}

#[test]
fn compile_while_statement() {
    let code = r#"
fn putchar(c: i32) -> (i32)
fn exit(c: i32) -> (i32)

while 0 {}
while 0 {
    putchar('n')
}
while 0 {
    putchar('n')
    putchar('n')
    putchar('n')
}
while 1 {
    while 1 {
        while 0 {
            putchar('n')
        }
        while 1 {
            exit(0)
        }
    }
}
return 0
    "#;
    compile(code).unwrap();
}
