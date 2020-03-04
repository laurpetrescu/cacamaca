mod lexer;

use lexer::lexer as lx;

fn main() {
	
	let mut l: lx::Lexer = lx::Lexer::new(String::from("let abc=,{}() 123"));
	let mut tok = l.next_token();
	
	loop {
		match tok.token_type {
			lx::Tokens::Integer(s) => {
				println!("Integer: {}", s);
				tok = l.next_token();
			},
			lx::Tokens::Invalid(s) => {
				println!("Invalid {} ", s);
				break;
			},
			_ => {
				println!("{}", tok.literal);
				tok = l.next_token();
			}
		}
	}
}
