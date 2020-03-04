//use lazy_static::lazy_static;
//use std::collections::HashMap;

pub mod lexer {
	const ILLEGAL: &str		= "ILLEGAL";
	const EOF: &str			= "EOF";
	const IDENTIFIER: &str 	= "IDENTIFIER";
	const INTEGER: &str		= "INTEGER";
	
	const FUNCTION: &str	= "fn";
	const LET: &str 		= "let";
	const TRUE: &str 		= "true";
	const FALSE: &str 		= "false";
	const IF: &str	 		= "if";
	const ELSE: &str 		= "else";
	const RETURN: &str 		= "return";
	
	const EQUAL: &str		= "==";
	const NOT_EQUAL: &str	= "!=";
	
	const ASSIGN: char 		= '=';
	const PLUS: char 		= '+';
	const MINUS: char 		= '-';
	const BANG: char 		= '!';
	const ASTERIX: char		= '*';
	const SLASH: char 		= '/';
	const LT: char 			= '<';
	const GT: char	 		= '>';
	
	const COMMA: char 		= ',';
	const SEMICOLON: char	= ';';
	const LPAREN: char 		= '(';
	const RPAREN: char 		= ')';
	const LBRACE: char 		= '{';
	const RBRACE: char 		= '}';
	
	
	pub struct Token {
		pub token_type: Tokens,
		pub literal: String
	}

	#[derive(Debug, Clone)]
	pub enum Tokens {
		Invalid(String),
		Eof(String),
		Identifier(String),
		Integer(String),
	
		Equal,
		NotEqual,
		Assign,
		Plus,
		Minus,
		Bang,
		Asterix,
		Slash,
		Lt,
		Gt,
		
		Comma,
		Semicolon,
		Lparen,
		Rparen,
		Lbrace,
		Rbrace,
	
		Function,
		Let,
		True,
		False,
		If,
		Else,
		Return
	}
	
	lazy_static::lazy_static! {
	static ref KEYWORDS: std::collections::HashMap<&'static str, Tokens> = {
		let mut map = std::collections::HashMap::new();
		map.insert(FUNCTION, Tokens::Function);
		map.insert(LET, Tokens::Let);
		
		map
	};
}
	
	fn lookup_keyword(keyword: &String) -> Tokens {
		match KEYWORDS.get(&keyword[..]) {
			Some(key) => {
				println!("deb key word: {}", keyword);
				key.clone()
			},
			None => {
				println!("deb indentifier: {}", keyword);
				Tokens::Identifier(keyword.to_string())
			}
		}
	}
	
	pub struct Lexer {
		input: String,
		position: usize,
		read_position: usize,
		pub ch: char
	}
	
	fn is_valid_identifier(ch: char) -> bool {
		ch.is_ascii_alphabetic() || ch == '_'

	}
	
	fn is_digit(ch: char) -> bool {
		'0' <= ch && ch <= '9'
	}
	
	fn is_whitespace(ch: char) -> bool {
		ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
	}
	
	fn skip_whitespace(lex: &mut Lexer) {
		while is_whitespace(lex.ch) {
			lex.read_char();
		}
	}
	
	impl Lexer {
		/*
		 * Constructor
		 */
		pub fn new(input: String) -> Lexer {
			let mut lex = Lexer{input: input, position: 0, read_position: 0, 
ch: '\0'};
			lex.read_char();
			return lex;
		}
		
		/* TODO: read/test for UTF-8 chars
		 * Read char
		 */
		pub fn read_char(&mut self) {
			if self.read_position >= self.input.len() {
				self.ch = '\0';
			} else {
				self.ch = self.input.chars().nth(self.read_position).unwrap();
			}
			
			self.position = self.read_position;
			self.read_position += 1;
		}
		
		/*
		 * Peek one char without moving the position marker
		 */
		pub fn peek_char(&self) -> char {
			if self.read_position >= self.input.len() {
				'\0'
			} else {
				self.input.chars().nth(self.read_position).unwrap()
			}
		}
		
		/* 
		 * Read identifier
		 */
		pub fn read_identifier(&mut self) -> String {
			let pos = self.position;
			while is_valid_identifier(self.ch) {
				self.read_char();
			}
			
			self.input[pos..self.position].to_string()
		}
		
		/*
		 * Read number
		 */
		pub fn read_number(&mut self) -> String {
			let pos = self.position;
			
			while is_digit(self.ch) {
				self.read_char();
			}
			
			self.input[pos..self.position].to_string()
		}
		
		/*
		 * Match next token
		 */
		pub fn next_token(&mut self) -> Token {
			skip_whitespace(self);
			let mut lit: String = self.ch.to_string();
			
			let tok = match self.ch {
				ASSIGN => {
					if self.peek_char() == ASSIGN {
						self.read_char();
						lit = String::from(EQUAL);
						Tokens::Equal
					} else {
						Tokens::Assign
					}
				},
				BANG => {
					if self.peek_char() == ASSIGN {
						self.read_char();
						lit = String::from(NOT_EQUAL);
						Tokens::NotEqual
					} else {
						Tokens::Bang
					}
				},
				PLUS => Tokens::Plus,
				MINUS => Tokens::Minus,
				SLASH => Tokens::Slash,
				ASTERIX => Tokens::Asterix,
				LT => Tokens::Lt,
				GT => Tokens::Gt,
				
				COMMA => Tokens::Comma,
				SEMICOLON => Tokens::Semicolon,
				LPAREN => Tokens::Lparen,
				RPAREN => Tokens::Rparen,
				LBRACE => Tokens::Lbrace,
				RBRACE => Tokens::Rbrace,
				_ => {
					println!("deb {}", self.ch);
					if is_valid_identifier(self.ch) {
						lit = self.read_identifier();
						lookup_keyword(&lit)
					} else if is_digit(self.ch) {
						lit = self.read_number();
						Tokens::Integer(lit.to_string())
					} else {
						Tokens::Invalid(self.ch.to_string())
					}
				}
			};
			
			let token = Token{token_type: tok, literal: lit};
			self.read_char();
			return token;
		}
	}
}

// #[cfg(test)]
// mod tests {
// 	#[test]
// 	fn it_works() {
// 		assert_eq!(2+2, 4);
// 	}
// 	
// 	#[test]
// 	fn call_lexer() {
// 		let mut l: lexer::lexer::Lexer = lexer::lexer::Lexer::new(String::from("let abc=,{}() 123"));
// 		let mut tok = l.next_token();
// 	
// 		match tok {
//  			lexer::lexer::Tokens::Identifier(s) => {
// 				println!("{}", s);
// 			},
// 			_ => {
// 				println!("somesome");
// 			}
// 		}
// 	}
// }
