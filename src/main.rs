use compiler::Compiler;

pub mod cl_type;
pub mod compiler;
pub mod ltype;
pub mod lvalue;
pub mod parser;
pub mod scope;
pub mod translator;

pub mod prelude {
    pub use crate::cl_type::*;
    pub use crate::ltype::*;
    pub use crate::lvalue::*;
    pub use crate::parser::*;
    pub use crate::scope::*;
}

fn main() {
    let code = std::fs::read_to_string("test.mil").expect("Failed to read file");

    let translator = Compiler::default();

    let object = translator.compile(&code).unwrap();
    let raw = object.emit().unwrap();
    std::fs::write("test.o", raw).expect("Failed to write to file");

    compile_output("./test");
}

fn compile_output(file: &str) {
    use std::process::Command;
    println!("==COMPILING==");
    Command::new("cc")
        .arg(format!("{file}.o"))
        .arg("-lc")
        .arg("-o")
        .arg(file)
        .spawn()
        .expect("Failed to compile file")
        .wait()
        .unwrap();
    println!("==RUNNING==");
    Command::new(file)
        .spawn()
        .expect("Failed to compile file")
        .wait()
        .unwrap();
}
