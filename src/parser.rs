use lexer::*;

/// a program is a vector of ASTNode, an AST node is a declaration or
/// a definition, 
pub enum ASTNode {
    Declaration,
    Definition
}

pub enum Declaration {
    Prototype(Prototype)
}

impl Declaration {
    const TOKEN: Token = Token::Extern;
}

pub struct Prototype {
    name: String,
    args: String
}

pub struct Definition {
    prototype: Prototype,
    expression: Expression
}

impl Definition {
    const TOKEN: Token = Token::Def;
}

pub enum Expression {
    LiteralExpr(f64),
    VariableExpr(String),
    BinaryExpr(BinaryExpr),
    CallExpr(CallExpr)
}

pub struct CallExpr {
    callee: String,
    args: Vec<Expression>
}

pub struct BinaryExpr {
    lhs: Box<Expression>,
    op: Operator,
    rhs: Box<Expression>
}