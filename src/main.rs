use iron_kaleidoscope::{lexer::tokenize};

//< ch-0 ch-1 ch-2 ch-3

extern crate iron_kaleidoscope;

fn main() {
    let src = "extern a = 2 / 3;";
    let tokens = tokenize(src);
    for token in tokens.clone() {
        println!("{:?}", token);
    }
}
//> ch-0 ch-1 ch-2 ch-3 parser-main
