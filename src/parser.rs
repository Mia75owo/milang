pub type NameType = (String, String);

/// The AST node for expressions.
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(String),
    Char(String),
    Identifier(String),
    DefineVar(NameType, Box<Expr>),
    Assign(String, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    WhileLoop(Box<Expr>, Vec<Expr>),
    Call(String, Vec<Expr>),
    GlobalDataAddr(String),
    DefFunc {
        name: String,
        params: Vec<NameType>,
        return_type: String,
    },
    Function(FunctionExpr),
    Return(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct FunctionExpr {
    pub name: String,
    pub params: Vec<NameType>,
    pub return_type: String,
    pub stmts: Vec<Expr>,
}

peg::parser!(pub grammar parser() for str {
    pub rule file() -> Vec<Expr>
        = stmts:statements() { stmts }

    pub rule function() -> Expr
        = "fn" _ name:identifier() _
        "(" params:((_ n:identifier() _ ":" _ t:identifier() _ {(n, t)}) ** ",") ")" _
        "->" _
        "(" return_type:identifier() ")" _
        "{" _
        stmts:statements()
        _ "}"
        { Expr::Function(FunctionExpr { name, params, return_type, stmts }) }

    rule def_func() -> Expr
        = "fn" _ name:identifier() _
        "(" params:((_ n:identifier() _ ":" _ t:identifier() _ {(n, t)}) ** ",") ")" _
        "->" _
        "(" returns:identifier() ")" ";"
        { Expr::DefFunc {name, params, return_type: returns } }

    rule statements() -> Vec<Expr>
        = s:(statement()*) { s }

    rule statement() -> Expr
        = _ e:function() _ { e }
        / _ e:return_expr() _ { e }
        / _ e:def_func() _ { e }
        / _ e:def_var() _ { e }
        / _ e:expression() _ { e }

    rule expression() -> Expr
        = if_else()
        / while_loop()
        / assignment()
        / binary_op()

    rule return_expr() -> Expr
        = "return" _ e:expression() { Expr::Return(Box::new(e)) }

    rule if_else() -> Expr
        = "if" _ e:expression() _ "{" _
        then_body:statements() _ "}" _ "else" _ "{" _
        else_body:statements() _ "}"
        { Expr::IfElse(Box::new(e), then_body, else_body) }

    rule while_loop() -> Expr
        = "while" _ e:expression() _ "{" _
        loop_body:statements() _ "}"
        { Expr::WhileLoop(Box::new(e), loop_body) }

    rule assignment() -> Expr
        = i:identifier() _ "=" _ e:expression() { Expr::Assign(i, Box::new(e)) }

    rule def_var() -> Expr
        = i:identifier() ":" _ t:identifier() _ "=" _ e:expression() { Expr::DefineVar((i, t), Box::new(e)) }

    rule binary_op() -> Expr = precedence!{
        "(" _ a:binary_op() _ ")" { a }
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
        i:identifier() _ "(" args:((_ e:expression() _ {e}) ** ",") ")" { Expr::Call(i, args) }
        i:identifier() { Expr::Identifier(i) }
        l:literal() { l }
    }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule literal() -> Expr
        = n:$(['0'..='9']+) { Expr::Literal(n.to_owned()) }
        / "&" i:identifier() { Expr::GlobalDataAddr(i) }

    rule char() -> Expr
        = "'" c:$("\\"? [_]) "'" { Expr::Char(c.to_owned()) }

    rule _() =  quiet!{[' ' | '\t' | '\n']*}
});
