use iron_kaleidoscope::{lexer::tokenize, parser::{default_parser_settings, parse}};

//< ch-0 ch-1 ch-2 ch-3

extern crate iron_kaleidoscope;

fn main() {
    let src = "a = 0;";
    let tokens = tokenize(src);
    for token in tokens.clone() {
        println!("{:?}", token);
    }
    let mut settings = default_parser_settings();
    let res = parse(tokens.as_slice(), &[], &mut settings).unwrap();
    println!("{:?}", res);
}
//> ch-0 ch-1 ch-2 ch-3 parser-main
