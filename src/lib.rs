//< ch-1 ch-2 ch-3
#![feature(box_syntax)]
//< ch-0
#![feature(plugin)]

extern crate regex;
extern crate regex_macro;
//> ch-0 ch-1

//< ir-import
extern crate llvm_sys;
//> ir-import

//< ch-0 ch-1 ch-2
pub mod lexer;
//> ch-0
pub mod parser;