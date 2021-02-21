use lexer::*;

/// a program is a vector of ASTNode, an AST node is a declaration or
/// a definition
#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    Declaration(Declaration),
    Definition(Definition)
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
    args: Vec<String>
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

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<ASTNode>, String> {
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
            Token::Extern => parse_declaration(&mut token_stack)?,
            Token::Def => parse_definition(&mut token_stack)?,
            Token::Ident(_) | Token::OpeningParenthesis => parse_expr(&mut token_stack)?,
            Token::Delimiter => {token_stack.pop(); continue},
            _ => return Err(String::from("unexpected token.")),
        };
        ast_result.push(cur_node);
    }

    Ok(ast_result)
}

pub fn parse_definition(tokens: &mut Vec<Token>) -> Result<ASTNode, String> {
    // eat Def keyword
    tokens.pop();

    // parse the prototype
    let prototype = parse_prototype(tokens)?;

    

    Err(String::from("unexpected token."))
}

macro_rules! ret_err_msg {
    ($err_msg:expr) => {
        return Err(String::from($err_msg))
    };
}

macro_rules! expect_token {
    ($pattern:pat in $tokens:expr, do $idt:block, except $err:block) => {
        expect_token!(@expand ($pattern) in $tokens, $idt, $err)
    };

    ($pattern:pat in $tokens:expr, except $err:block) => {
        expect_token!(@expand ($pattern) in $tokens, {}, $err)
    };

    ($pattern:pat in $tokens:expr, do $idt:block, throw $err_msg:expr) => {
        expect_token!(@expand ($pattern) in $tokens, $idt, {ret_err_msg!($err_msg)})
    };

    ($pattern:pat in $tokens:expr, throw $err_msg:expr) => {
        expect_token!(@expand ($pattern) in $tokens, {}, {ret_err_msg!($err_msg)})
    };

    (@expand ($($pattern:pat),+) in $tokens:expr, $do:block, $err:block) => {{
        if let Some(x) = $tokens.last() {
            let x = x.clone();
            match x {
                $($pattern)|+ => $do,
                _ => $err
            }
        } else {
            return Err(String::from("unexpected end of program."))
        }
    }};
}

pub fn parse_prototype(tokens: &mut Vec<Token>) -> Result<Prototype, String> {
    // eat name
    let name = expect_token!(Token::Ident(t) in tokens, 
        do {tokens.pop();t}, 
        throw "expect function name in declaration.");
    // eat '('
    expect_token!(Token::OpeningParenthesis in tokens, 
        do {tokens.pop()},
        throw "missing '('");

    // eat all args into a string vector
    let mut args: Vec<String> = vec![];
    loop {
        args.push(expect_token!(Token::Ident(t) in tokens, 
            do {tokens.pop(); t}, 
            except {break}));
        expect_token!(Token::Comma in tokens, 
            do {tokens.pop();}, 
            except {});
    }

    // eat ')'
    expect_token!(Token::ClosingParenthesis in tokens, 
        do {tokens.pop();},
        throw "missing ')'");

    Ok(Prototype {
        name,
        args
    })
}

/// parse a declaration and return the corresponding AST node
/// a declaration obeies this form:</br>
/// extern name (arg1, arg2, ...)
pub fn parse_declaration(tokens: &mut Vec<Token>) -> Result<ASTNode, String> {
    // pop the Extern token
    tokens.pop();

    // get prototype
    let prototype = parse_prototype(tokens)?;

    Ok(ASTNode::Declaration(Declaration::Prototype(prototype)))
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
        let res = parse(&delimiters).unwrap();
        assert_eq!(res, Vec::new());
    }

    #[test]
    fn test_declaration() {
        let declare1 = tokenize("extern my_first_func(arg1, arg2, arg3)");
        let declare2 = tokenize("extern myfunc(arg1)");
        let declare3 = tokenize("extern myfunc()");
        let declare4 = tokenize("extern myfunc(arg1 arg2");
        let declare5 = tokenize("extern myfunc(;a");
        let declare6 = tokenize("extern myfunc(arg1");

        let res1 = parse(&declare1).unwrap();
        let res2 = parse(&declare2).unwrap();
        let res3 = parse(&declare3).unwrap();
        println!("{:?}", res1);
        println!("{:?}", res2);
        println!("{:?}", res3);

        let res4 = parse(&declare4).unwrap_err();
        let res5 = parse(&declare5).unwrap_err();
        let res6 = parse(&declare6).unwrap_err();
        println!("{:?}", res4);
        println!("{:?}", res5);
        println!("{:?}", res6);
    }
}