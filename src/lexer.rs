use regex_macro::regex;

#[derive(PartialEq, Clone, Debug)]
pub enum Operator {
    Add,
    Mul,
    Sub,
    Div,
    Mod,
    Assign
}

/// all the tokens used in our language
#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Def,
    Extern,
    If,
    Then,
    Else,
    For,
    In,
    Binary,
    Unary,
    Var,
    Delimiter, //';' character
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Ident(String),
    Number(f64),
    Operator(Operator)
}

/// split the source code into vector of token
pub fn tokenize(input: &str) -> Vec<Token> {
    // comments start with #, end with the line end
    let comment_re: &regex::Regex = regex!(r"(?m)#.*\n");
    // remove all comments
    let preprocessed = comment_re.replace_all(input, "\n");

    let mut result = Vec::new();

    // regex for token, just union of straightforward regexes for different token types
    // operators are parsed the same way as identifier and separated later
    let token_re: &regex::Regex = regex!(concat!(
        r"(?P<ident>\p{Alphabetic}\w*)|",
        r"(?P<number>\d+\.?\d*)|",
        r"(?P<delimiter>;)|",
        r"(?P<oppar>\()|",
        r"(?P<clpar>\))|",
        r"(?P<comma>,)|",
        r"(?P<operator>\S)"));

    for cap in token_re.captures_iter(preprocessed.as_ref()) {
        let token = if cap.name("ident").is_some() {
            match cap.name("ident").unwrap().as_str() {
                "def" => Token::Def,
                "extern" => Token::Extern,
                "if" => Token::If,
                "then" => Token::Then,
                "else" => Token::Else,
                "for" => Token::For,
                "in" => Token::In,
                "binary" => Token::Binary,
                "unary" => Token::Unary,
                "var" => Token::Var,
                ident => Token::Ident(ident.to_string())
            }
        } else if cap.name("number").is_some() {
            match cap.name("number").unwrap().as_str().parse() {
                Ok(number) => Token::Number(number),
                Err(_) => panic!("Lexer failed trying to parse number")
            }
        } else if cap.name("delimiter").is_some() {
            Token::Delimiter
        } else if cap.name("oppar").is_some() {
            Token::OpeningParenthesis
        } else if cap.name("clpar").is_some() {
            Token::ClosingParenthesis
        } else if cap.name("comma").is_some() {
            Token::Comma
        } else if cap.name("operator").is_some() {
            match cap.name("operator").unwrap().as_str() {
                "+" => Token::Operator(Operator::Add),
                "-" => Token::Operator(Operator::Sub),
                "*" => Token::Operator(Operator::Mul),
                "/" => Token::Operator(Operator::Div),
                "%" => Token::Operator(Operator::Mod),
                "=" => Token::Operator(Operator::Assign),
                _ => panic!("Unknown operator.")
            }
        } else {
            panic!("Impossible error.")
        };

        result.push(token)
    }

    result
}

#[cfg(test)]
mod tests {

    use super::tokenize;
    use super::{Token, Operator};

    #[test]
    fn test_lexer() {
        let tokens = tokenize("extern a = 2 / 3");
        assert_eq!(tokens[0], Token::Extern);
        assert_eq!(tokens[1], Token::Ident(String::from("a")));
        assert_eq!(tokens[2], Token::Operator(Operator::Assign));
        assert_eq!(tokens[3], Token::Number(2.0));
        assert_eq!(tokens[4], Token::Operator(Operator::Div));
        assert_eq!(tokens[5], Token::Number(3.0));
    }
}