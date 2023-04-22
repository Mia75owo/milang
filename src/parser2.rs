pub type NameType = (String, TypeExpr);

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Ident(String),
    Array(Box<TypeExpr>, Box<Expr>),
}

/// The AST node for expressions.
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(String),
    Char(String),
    String(String),
    Array(TypeExpr, Vec<Expr>),
    ArrayAccess(Box<Expr>, Box<Expr>),
    Identifier(String),
    DefineVar(NameType, Box<Expr>),
    Assign(String, Box<Expr>),
    AssignArray(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    WhileLoop(Box<Expr>, Vec<Expr>),
    Call(String, Vec<Expr>),
    GlobalDataAddr(String),
    DefFunc(DefFuncExpr),
    Function(FunctionExpr),
    Return(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct DefFuncExpr {
    pub name: String,
    pub params: Vec<NameType>,
    pub return_type: TypeExpr,
}

#[derive(Debug, Clone)]
pub struct FunctionExpr {
    pub name: String,
    pub params: Vec<NameType>,
    pub return_type: TypeExpr,
    pub stmts: Vec<Expr>,
}

peg::parser!(pub grammar parser() for str {
    rule traced<T>(e: rule<T>) -> T =
        &(input:$([_]*) {
            #[cfg(feature = "trace")]
            println!("[PEG_INPUT_START]\n{}\n[PEG_TRACE_START]", input);
        })
        e:e()? {?
            #[cfg(feature = "trace")]
            println!("[PEG_TRACE_STOP]");
            e.ok_or("")
        }

    pub rule file() -> Vec<Expr>
        = stmts:traced(<_file()>) { stmts }
    pub rule _file() -> Vec<Expr>
        = stmts:statements() { stmts }

    rule function() -> Expr
        = "fn" _ name:identifier() _
        "(" params:((_ n:identifier() _ ":" _ t:var_type() _ {(n, t)}) ** ",") ")" _
        "->" _
        "(" return_type:var_type() ")" _
        "{" _
        stmts:statements()
        _ "}"
        { Expr::Function(FunctionExpr { name, params, return_type, stmts }) }

    rule def_func() -> Expr
        = "fn" _ name:identifier() _
        "(" params:((_ n:identifier() _ ":" _ t:var_type() _ {(n, t)}) ** ",") ")" _
        "->" _
        "(" returns:var_type() ")" ";"
        { Expr::DefFunc(DefFuncExpr { name, params, return_type: returns }) }

    rule statements() -> Vec<Expr>
        = s:(statement()*) { s }

    rule statement() -> Expr
        = _ e:function() _ { e }
        / _ e:return_expr() _ { e }
        / _ e:def_func() _ { e }
        / _ e:def_var() _ { e }
        / _ e:while_loop() _ { e }
        / _ e:if_else() _ { e }
        / _ e:assignment_array() _ { e }
        / _ e:assignment() _ { e }
        / _ e:function_call() _ { e }

    rule value() -> Expr
        = _ "(" _ v:value() _ ")" _ { v }
        / _ v:array_access() _ { v }
        / _value()
    rule _value() -> Expr
        = _ v:binary_op() _ { v }

    rule return_expr() -> Expr
        = "return" _ e:value() { Expr::Return(Box::new(e)) }

    rule if_else() -> Expr
        = "if" _ e:value() _ "{" _
        then_body:statements() _ "}" _ "else" _ "{" _
        else_body:statements() _ "}"
        { Expr::IfElse(Box::new(e), then_body, else_body) }

    rule while_loop() -> Expr
        = "while" _ e:value() _ "{" _
        loop_body:statements() _ "}"
        { Expr::WhileLoop(Box::new(e), loop_body) }

    rule assignment() -> Expr
        = i:identifier() _ "=" _ e:value() { Expr::Assign(i, Box::new(e)) }
    rule assignment_array() -> Expr
        = i:array_access() _ "=" _ e:value() { Expr::AssignArray(Box::new(i), Box::new(e)) }

    rule def_var() -> Expr
        = i:identifier() ":" _ t:var_type() _ "=" _ e:value() { Expr::DefineVar((i, t), Box::new(e)) }

    #[cache_left_rec]
    rule binary_op() -> Expr = precedence!{
        a:@ _ "&&" _ b:(@) { Expr::And(Box::new(a), Box::new(b)) }
        a:@ _ "||" _ b:(@) { Expr::Or(Box::new(a), Box::new(b)) }
        --
        a:@ _ "==" _ b:(@) { Expr::Eq(Box::new(a), Box::new(b)) }
        a:@ _ "!=" _ b:(@) { Expr::Ne(Box::new(a), Box::new(b)) }
        a:@ _ "<"  _ b:(@) { Expr::Lt(Box::new(a), Box::new(b)) }
        a:@ _ "<=" _ b:(@) { Expr::Le(Box::new(a), Box::new(b)) }
        a:@ _ ">"  _ b:(@) { Expr::Gt(Box::new(a), Box::new(b)) }
        a:@ _ ">=" _ b:(@) { Expr::Ge(Box::new(a), Box::new(b)) }
        --
        a:@ _ "+" _ b:(@) { Expr::Add(Box::new(a), Box::new(b)) }
        a:@ _ "-" _ b:(@) { Expr::Sub(Box::new(a), Box::new(b)) }
        --
        a:@ _ "*" _ b:(@) { Expr::Mul(Box::new(a), Box::new(b)) }
        a:@ _ "/" _ b:(@) { Expr::Div(Box::new(a), Box::new(b)) }
        --
        c:char() { c }
        s:string() { s }
        f:function_call() { f }
        i:identifier() { Expr::Identifier(i) }
        i:array_value() { i }
        l:literal() { l }
    }

    rule function_call() -> Expr
        = i:identifier() _ "(" args:((_ e:value() _ {e}) ** ",") ")" { Expr::Call(i, args) }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule literal() -> Expr
        = n:$(['0'..='9']+) { Expr::Literal(n.to_owned()) }
        / "&" i:identifier() { Expr::GlobalDataAddr(i) }

    rule array_value() -> Expr
        = "@" ty:var_type() "[" values:((_ v:value() _ { v }) ** ",") "]" { Expr::Array(ty, values) }

    #[cache_left_rec]
    rule array_access() -> Expr
        = i:array_access() "[" _ index:value() _ "]" { Expr::ArrayAccess(Box::new(i), Box::new(index)) }
        / i:_value() { i }

    rule string_escape() -> String
        = s:$(quiet!{"\\n"}) { "\n".to_string() }
        / s:$(quiet!{"\\r"}) { "\r".to_string() }
        / s:$(quiet!{"\\t"}) { "\t".to_string() }
        / s:$(quiet!{"\\0"}) { "\0".to_string() }
        / quiet!{"\\"} c:$([_]) { c.to_string() }
    rule string_literal_char() -> String
        = string_escape()
        / s:$([^'\"']) { s.to_owned() }
    rule string() -> Expr
        = quiet!{"\"" s:string_literal_char()* "\"" { Expr::String(s.join("")) }}

    rule char() -> Expr
        = "'" c:string_literal_char() "'" { Expr::Char(c.to_owned()) }

    rule var_type() -> TypeExpr
        //= "[" _ ty:var_type() _ ";" _ len:literal() "]" { TypeExpr::Array(Box::new(ty), Box::new(len)) }
        = "[" _ ty:var_type() _ ";" _ len:literal() "]" { TypeExpr::Array(Box::new(ty), Box::new(len)) }
        / "[" _ ty:var_type() _ "]" { TypeExpr::Array(Box::new(ty), Box::new(Expr::Literal("0".to_owned()))) }
        / i:identifier() { TypeExpr::Ident(i) }

    rule _() = ignore()*

    rule ignore()
        = whitespace()
        / single_comment()
        / multi_comment()
    rule whitespace()
        = quiet!{[' ' | '\t' | '\n']}
    rule single_comment()
        = "//" ([^'\n']*)
    rule multi_comment()
        = "/*" ((!"*/"[_])*) "*/"
});
