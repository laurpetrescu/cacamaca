pub mod token {

	//pub type TokenType = String;
	
	pub const ILLEGAL: &str		= "ILLEGAL";
	pub const EOF: &str			= "EOF";
	pub const IDENTIFIER: &str 	= "IDENTIFIER";
	pub const INTEGER: &str		= "INTEGER";
	pub const ASSIGN: &str 		= "=";
	pub const PLUS: &str 		= "+";
	pub const COMMA: &str 		= ",";
	pub const SEMICOLON: &str	= ";";
	pub const LPAREN: &str 		= "(";
	pub const RPAREN: &str 		= ")";
	pub const LBRACE: &str 		= "{";
	pub const RBRACE: &str 		= "}";
	pub const FUNCTION: &str	= "FUNCTION";
	pub const LET: &str 		= "LET";
	
	pub struct Token {
		token_type: String,
		literal: String
	}
	
	pub enum Tokens {
		Illegal(String),
		Eof(String),
		Identifier(String),
		Integer(String),
		ASSIGN(String),
		PLUS(String),
		COMMA(String),
		SEMICOLON(String),
		LPAREN(String),
		RPAREN(String),
		LBRACE(String),
		RBRACE(String),
		FUNCTION(String),
		LET(String),
	}
	
	pub fn test() {
		println!("test");
	}
} 
