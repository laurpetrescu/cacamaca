// use crate::lexer;
// mod lexer;
// use std::io::*;
// use std::io::Write;
// 
// 
// pub mod repl {
// 	const PROMPT: &str = "cacamaca >> ";
// 
// 	/*
// 	 * Start REPL program.
// 	 */
// 	pub fn start() {
// 		
// 		loop {
// 			print!("{}", PROMPT);
// 			println!("");
// 			let mut line = String::new();
// 			
// 			std::io::stdout().flush();
// 			std::io::stdin().read_line(&mut line).expect("error while getting string");
// 			
// 			if let Some('\n') = line.chars().next_back() {
// 				line.pop();
// 			}
// 			
// 			if let Some('\r') = line.chars().next_back() {
// 				line.pop();
// 			}
// 			
// 			println!("Feed: {}", line);
//  			let lex = lexer::lexer::Lexer::new(line);
// 			while let tok = lex.next_token() != lx::Tokens::Eof {
// 				println!("{:?}", tok);
// 			}
// 		}
// 	}
// }
