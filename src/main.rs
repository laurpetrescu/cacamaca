mod lexer;

use std::io::{Write};
use lexer::lexer as lx;

const PROMPT: &str = "kakamaca>> ";

/*
* Start REPL program.
*/
fn start() {
	loop {
		print!("{}", PROMPT);
		
		let mut line = String::new();
		
		std::io::stdout().flush().expect("error while flushing");
		std::io::stdin().read_line(&mut line).expect("error while getting string");
		
		if let Some('\n') = line.chars().next_back() {
			line.pop();
		}
		
		if let Some('\r') = line.chars().next_back() {
			line.pop();
		}
		
		let lex = lx::Lexer::new(line);
		let mut parser = lx::Parser::new(lex);
		
		parser.parse_program();
// 		loop {
// 			let tok = lex.next_token();
// 			match tok.token_type {
// 				lx::TokenType::Eof => break,
// 				_ => println!("{:?}", tok)
// 			}
// 		}
	}
}

fn main() {
	
// 	let mut l: lx::Lexer = lx::Lexer::new(String::from("let abc=,{}() 123"));
// 	let mut tok = l.next_token();

	println!("Hellow, type commands here!");
	start();
}
