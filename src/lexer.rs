//use lazy_static::lazy_static;
//use std::collections::HashMap;

#[allow(dead_code)]
pub mod lexer {
// 	const ILLEGAL: &str		= "ILLEGAL";
// 	const IDENTIFIER: &str 	= "IDENTIFIER";
// 	const INTEGER: &str		= "INTEGER";
	
	const FUNCTION: &str	= "fn";
	const LET: &str 		= "let";
	const TRUE: &str 		= "true";
	const FALSE: &str 		= "false";
	const IF: &str	 		= "if";
	const ELSE: &str 		= "else";
	const RETURN: &str 		= "return";
	const KAKA: &str 		= "kaka";
	const MACA: &str 		= "maca";
	
	const EQUAL: &str		= "==";
	const NOT_EQUAL: &str	= "!=";
	
	const EOF: char			= '\0';
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
	
	#[derive(Debug, Clone)]
	pub struct Token {
		pub token_type: TokenType,
		pub literal: String
	}
	
	impl Token {
		fn new() -> Token {
			Token{token_type: TokenType::Eof, literal: String::new()}
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub enum TokenType {
		Invalid,
		Eof,
		Kaka,
		Maca,
		
		Identifier,
		Integer,
	
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
	static ref KEYWORDS: std::collections::HashMap<&'static str, TokenType> = {
		let mut map = std::collections::HashMap::new();
		map.insert(FUNCTION, TokenType::Function);
		map.insert(LET, TokenType::Let);
		map.insert(TRUE, TokenType::True);
		map.insert(FALSE, TokenType::False);
		map.insert(IF, TokenType::If);
		map.insert(ELSE, TokenType::Else);
		map.insert(RETURN, TokenType::Return);
		map.insert(KAKA, TokenType::Kaka);
		map.insert(MACA, TokenType::Maca);
		
		map
		};
	}
	
	pub enum Precedence {
		Lowest = 0,
		Equals = 1, 		// ==
		LessGreater = 2, 	// > or <
		Sum = 3,			// +
		Product = 4,		// *
		Prefix = 5,			// -x or !x
		Call = 6
	}
	
	fn lookup_keyword(keyword: &String) -> TokenType {
		match KEYWORDS.get(&keyword[..]) {
			Some(key) => {
				//println!("deb key word: {}", keyword);
				key.clone()
			},
			None => {
				//println!("deb indentifier: {}", keyword);
				TokenType::Identifier
			}
		}
	}
	
	#[allow(dead_code)]
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
	
	#[allow(dead_code)]
	impl Lexer {
		/*
		 * Constructor
		 */
		pub fn new(input: String) -> Lexer {
			let mut lex = Lexer{input: input, position: 0, read_position: 0, ch: '\0'};
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
			while is_valid_identifier(self.peek_char()) {
				self.read_char();
			}
			
			self.input[pos..self.position+1].to_string()
		}
		
		/*
		 * Read number
		 */
		pub fn read_number(&mut self) -> String {
			let pos = self.position;
			
			while is_digit(self.peek_char()) {
				self.read_char();
			}
			
			self.input[pos..self.position+1].to_string()
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
						TokenType::Equal
					} else {
						TokenType::Assign
					}
				},
				BANG => {
					if self.peek_char() == ASSIGN {
						self.read_char();
						lit = String::from(NOT_EQUAL);
						TokenType::NotEqual
					} else {
						TokenType::Bang
					}
				},
				PLUS => TokenType::Plus,
				MINUS => TokenType::Minus,
				SLASH => TokenType::Slash,
				ASTERIX => TokenType::Asterix,
				LT => TokenType::Lt,
				GT => TokenType::Gt,
				
				COMMA => TokenType::Comma,
				SEMICOLON => TokenType::Semicolon,
				LPAREN => TokenType::Lparen,
				RPAREN => TokenType::Rparen,
				LBRACE => TokenType::Lbrace,
				RBRACE => TokenType::Rbrace,
				EOF => TokenType::Eof,
				_ => {
					//println!("deb {}", self.ch);
					if is_valid_identifier(self.ch) {
						lit = self.read_identifier();
						lookup_keyword(&lit)
					} else if is_digit(self.ch) {
						lit = self.read_number();
						TokenType::Integer
					} else {
						TokenType::Invalid
					}
				}
			};
			
			let token = Token{token_type: tok, literal: lit};
			self.read_char();
			return token;
		}
	}

	
/////////////////////////////////////////////////////////////////////////////////

// 	trait NodeTrait {
// 		fn token_literal(&self) -> &str;
// 		fn to_string(&self) -> String;
// 	}
	
	trait StatementTrait {
		fn token_literal(&self) -> &str;
		fn to_string(&self) -> String;
	}
	
	trait ExpressionTrait {
		fn token_literal(&self) -> &str;
		fn to_string(&self) -> String;
	}
	
// 	struct Node;
	struct Expression {
		token: Token
	}
	
	impl Expression {
		fn new() -> Expression {
			Expression{token: Token{token_type: TokenType::Eof, literal: EOF.to_string()}}
		}
	}
	
	impl ExpressionTrait for Expression {
		fn token_literal(&self) -> &str {
			self.token.literal.as_str()
		}

		fn to_string(&self) -> String {
			self.token.literal.clone()
		}
	}
	
// 	impl StatementTrait for Expression {
// 		fn token_literal(&self) -> &str {
// 			self.token.literal.as_str()
// 		}
// 		
// 		fn to_string(&self) -> String {
// 			self.token.literal.clone()
// 		}
// 	}
	
	struct IdentifierExpression {
		token: Token,
		value: String
	}
	
	impl IdentifierExpression {
		fn new() -> IdentifierExpression {
			IdentifierExpression{token: Token::new(), value: String::new()}
		}
	}
	
	impl ExpressionTrait for IdentifierExpression {
		fn token_literal(&self) -> &str {
			self.token.literal.as_str()
		}
		
		fn to_string(&self) -> String {
			self.value.clone()
		}
	}
	
	struct IntegerExpression {
		token: Token,
		value: i64
	}
	
	impl IntegerExpression {
		fn new() -> IntegerExpression {
			IntegerExpression{token: Token::new(), value: 0}
		}
	}
	
	impl ExpressionTrait for IntegerExpression {
		fn token_literal(&self) -> &str {
			self.token.literal.as_str()
		}
		
		fn to_string(&self) -> String {
			self.token.literal.clone()
		}
	}
	
	struct LetStatement {
		token: Token,
		name: String,
		value: Expression
	}
	
	impl StatementTrait for LetStatement {
		fn token_literal(&self) -> &str {
			self.token.literal.as_str()
		}
		
		fn to_string(&self) -> String {
			format!("{} {} = {}", self.token.literal, self.name, self.value.to_string())
		}
	}
	
	impl LetStatement {
		pub fn new() -> Box<LetStatement> {
			Box::new(LetStatement{
				token: Token{token_type: TokenType::Let, literal: LET.to_string()},
				name: String::new(),
				value: Expression::new()
			})
		}
	}
	
	struct ReturnStatement {
		token: Token,
		return_value: Expression
	}
	
	impl StatementTrait for ReturnStatement {
		fn token_literal(&self) -> &str {
			&self.token.literal
		}
		
		fn to_string(&self) -> String {
			format!("{} {}", self.token.literal, self.return_value.to_string())
		}
	}
	
	impl ReturnStatement {
		pub fn new() -> Box<ReturnStatement> {
			Box::new(ReturnStatement{
				token: Token{token_type: TokenType::Return, literal: RETURN.to_string()},
				return_value: Expression::new()
			})
		}
	}
	
	struct ExpressionStatement {
		token: Token,
		expression: Box<dyn ExpressionTrait>
	}
	
	impl ExpressionStatement {
		pub fn new() -> Box<ExpressionStatement> {
			Box::new(ExpressionStatement{
				token: Token{token_type: TokenType::Identifier, literal: EOF.to_string()},
				expression: Box::new(Expression::new())
			})
		}
	}
	
	impl StatementTrait for ExpressionStatement {
		fn token_literal(&self) -> &str {
			&self.token.literal
		}
		
		fn to_string(&self) -> String {
			self.expression.to_string()
		}
	}
	
	struct Program {
		statements: Vec<Box<dyn StatementTrait>>
	}
	
	impl Program {
		fn new() -> Program {
			Program{statements: vec![]}
		}
	}
	
	impl StatementTrait for Program {
		fn token_literal(&self) -> &str {
			if self.statements.len() > 0 {
				self.statements[0].token_literal()
			} else {
				""
			}
		}
		
		fn to_string(&self) -> String {
			let mut program_string = String::new();
			for s in &self.statements {
				program_string.push_str(&s.to_string());
			}
			
			return program_string;
		}
	}
	
	type InfixParserFn = fn() -> Option<Box<dyn ExpressionTrait>>;
	type PrefixParserFn = fn(&Token) -> Option<Box<dyn ExpressionTrait>>;
		
	
	struct Parser {
		
		lexer: Lexer,
		errors: Vec<String>,
		current_token: Token,
		peek_token: Token,
		infix_parsers: std::collections::HashMap<TokenType, InfixParserFn>,
		prefix_parsers: std::collections::HashMap<TokenType, PrefixParserFn>
	}
	
	fn parse_identifier(_tok: &Token) -> Option<Box<dyn ExpressionTrait>> {
		None
	}
	
	fn parse_integer(tok: &Token) -> Option<Box<dyn ExpressionTrait>> {
		if let Ok(i) = tok.literal.parse::<i64>() {
			Some(Box::new(IntegerExpression{token: tok.clone(), value: i}))
		} else {
			None
		}
	}
		
	impl Parser {
		pub fn new(lex: Lexer) -> Parser {
			let mut parser = Parser{lexer: lex,
				errors: vec![],
				current_token: Token{token_type: TokenType::Eof, literal: String::new()},
				peek_token: Token{token_type: TokenType::Eof, literal: String::new()},
				infix_parsers: std::collections::HashMap::new(),
				prefix_parsers: std::collections::HashMap::new()
			};
			
			parser.prefix_parsers.insert(TokenType::Identifier, parse_identifier);
			parser.prefix_parsers.insert(TokenType::Integer, parse_integer);
			
			// populate current and peek TokenType
			parser.next_token();
			parser.next_token();
			
			return parser;
		}
		
		pub fn errors(&self) -> &Vec<String> {
			&self.errors
		}
		
		pub fn peek_error(&mut self, tok: TokenType) {
			self.errors.push(format!("expected next token to be {:?}, got {:?} instead",
				tok, self.peek_token.token_type));
		}
		
		pub fn next_token(&mut self) {
			self.current_token = self.peek_token.clone();
			self.peek_token = self.lexer.next_token();
		}
		
		pub fn parse_program(&mut self) -> Program {
			let mut program = Program::new();
			
			loop {
				match self.current_token.token_type {
					TokenType::Eof => break,
					_ => {
						if let Some(statement) = self.parse_statement() {
							program.statements.push(statement);
						}
						
						self.next_token();
					}
				}
			}
			
			return program;
		}
		
		pub fn parse_statement(&mut self) -> Option<Box<dyn StatementTrait>> {
			match self.current_token.token_type {
				TokenType::Let => self.parse_let_statement(),
				TokenType::Return => self.parse_return_statement(),
				_ => self.parse_expression_statement()
			}
		}
		
		pub fn parse_let_statement(&mut self) -> Option<Box<dyn StatementTrait>> {
			let stmt = LetStatement::new();
			
			if !self.expect_peek(TokenType::Identifier) {
				return None;
			}
			
			while !self.is_current_token(TokenType::Semicolon) {
				self.next_token();
			}
			
			return Some(stmt);
		}
		
		pub fn parse_return_statement(&mut self) -> Option<Box<dyn StatementTrait>> {
			let stmt = ReturnStatement::new();
			
			self.next_token();
			
			while !self.is_current_token(TokenType::Semicolon) {
				self.next_token();
			}
			
			return Some(stmt);
		}
		
		pub fn parse_expression_statement(&mut self) -> Option<Box<dyn StatementTrait>> {
			if let Some(expr) = self.parse_expression(Precedence::Lowest) {
				while !self.is_current_token(TokenType::Semicolon) {
					self.next_token();
				}
				
				Some(Box::new(ExpressionStatement{
					token: self.current_token.clone(),
					expression: expr}))
			} else {
				None
			}
		}
		
		fn parse_expression(&self, _prec: Precedence) -> Option<Box<dyn ExpressionTrait>> {
			if let Some(prefix) = self.prefix_parsers.get(&self.current_token.token_type) {
				prefix(&self.current_token)
			} else {
				None
			}
		}
		
		
		fn is_current_token(&self, tok: TokenType) -> bool {
			tok == self.current_token.token_type
		}
		
		fn is_peek_token(&self, tok: TokenType) -> bool {
			tok == self.peek_token.token_type
		}
		
		fn expect_peek(&mut self, tok: TokenType) -> bool {
			if self.is_peek_token(tok.clone()) {
				self.next_token();
				return true;
			} else {
				self.peek_error(tok);
				return false;
			}
		}
		
		pub fn register_infix(&mut self, tok: TokenType, infix: InfixParserFn) {
			self.infix_parsers.insert(tok, infix);
		}
		
		pub fn register_prefix(&mut self, tok: TokenType, prefix: PrefixParserFn) {
			self.prefix_parsers.insert(tok, prefix);
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
//  			lexer::lexer::TokenType::Identifier(s) => {
// 				println!("{}", s);
// 			},
// 			_ => {
// 				println!("somesome");
// 			}
// 		}
// 	}
// }
