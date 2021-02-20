use lexer::*;

/// a program is a vector of ASTNode, an AST node is a declaration or
/// a definition
#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    Declaration,
    Definition
}

/// a declaration gives us the prototype of a function, which consists of
/// function name and parameter list
#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Prototype(Prototype)
}


/// defines a prototype
#[derive(Debug, PartialEq, Clone)]
pub struct Prototype {
    name: String,
    args: String
}

/// definition consists of a prototype and an expression</br>
/// note: in this language, we consider a bare expression in the top scope as
/// a anonymous function.
#[derive(Debug, PartialEq, Clone)]
pub struct Definition {
    prototype: Prototype,
    expression: Expression
}

/// an expression can be a literal, a variable, a call-sentence or a simple binary expression
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    LiteralExpr(f64),
    VariableExpr(String),
    BinaryExpr(BinaryExpr),
    CallExpr(CallExpr)
}

/// a CallExpr structure decriables the callee name and the arguments
#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr {
    callee: String,
    args: Vec<Expression>
}

/// this is a recursive definition of Binary Expression
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    lhs: Box<Expression>,
    op: Operator,
    rhs: Box<Expression>
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<ASTNode>, String> {
    let mut token_stack = tokens.clone();
    let mut ast_result: Vec<ASTNode> = Vec::new();

    // use pop and push from the start of tokens
    token_stack.reverse();

    // we will start with the following terminators
    // Extern, Def, Ident, OpeningParenthesis, Delimiter
    loop {
        let cur_token = match token_stack.last() {
            Some(x) => x.clone(),
            None => break
        };
        let cur_node = match cur_token {
            Token::Extern => parse_declare(&mut token_stack)?,
            Token::Def => parse_def(&mut token_stack)?,
            Token::Ident(_) | Token::OpeningParenthesis => parse_expr(&mut token_stack)?,
            Token::Delimiter => {token_stack.pop(); continue},
            _ => return Err(String::from("unexpected token.")),
        };
        ast_result.push(cur_node);
    }

    Ok(ast_result)
}

pub fn parse_def(tokens: &mut Vec<Token>) -> Result<ASTNode, String> {

    Err(String::from("unexpected token."))
}

macro_rules! ret_err_msg {
    ($err_msg:expr) => {
        return Err(String::from($err_msg))
    };
}

macro_rules! expect_token {
    ($pattern:pat in $tokens:expr, $idt:expr, $err_msg:expr) => {
        expect_token!(@expand $pattern in $tokens, $idt, {ret_err_msg!($err_msg)})
    };

    ($pattern:pat in $tokens:expr, $err_msg:expr) => {
        expect_token!(@expand $pattern in $tokens, {}, {ret_err_msg!($err_msg)})
    };

    (@expand $pattern:pat in $tokens:tt, $ret:tt, $err:block) => {{
        if let Some(x) = $tokens.pop() {
            match x {
                $pattern => $ret,
                _ => $err
            }
        } else {
            return Err(String::from("incomplete sentence."))
        }
    }};

    
}

/// a declaration obeies this form:</br>
/// extern name (arg1, arg2, ...)
pub fn parse_declare(tokens: &mut Vec<Token>) -> Result<ASTNode, String> {
    // pop the Extern token
    tokens.pop();

    let cur_token = match tokens.last() {
        Some(x) => x,
        _ => return Err(String::from("declaration statement not complete."))
    };

    let name = expect_token!(Token::Ident(t) in tokens, t, "expecting ident");
    expect_token!(Token::OpeningParenthesis in tokens, "missing '('");






    Err(String::from("unexpected token."))
}

pub fn parse_expr(tokens: &mut Vec<Token>) -> Result<ASTNode, String> {
    Err(String::from("unexpected token."))
}

#[cfg(test)]
mod tests {
    use crate::lexer::{tokenize, Token};
    use super::parse;

    #[test]
    fn all_delimiter() {
        let delimiters = tokenize(";;;;");
        assert_eq!(delimiters, vec![Token::Delimiter, Token::Delimiter, Token::Delimiter, Token::Delimiter]);
        let res = parse(delimiters).unwrap();
        assert_eq!(res, Vec::new());
    }
}