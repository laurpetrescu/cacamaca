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
	
	#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
	pub enum Precedence {
		Lowest = 0,
		Equals = 1, 		// ==
		LessGreater = 2, 	// > or <
		Sum = 3,			// +
		Product = 4,		// *
		Prefix = 5,			// -x or !x
		Call = 6
	}
	
	lazy_static::lazy_static! {
	static ref PRECEDENCES: std::collections::HashMap<TokenType, Precedence> = {
		let mut map = std::collections::HashMap::new();
		map.insert(TokenType::Equal, Precedence::Equals);
		map.insert(TokenType::NotEqual, Precedence::Equals);
		map.insert(TokenType::Lt, Precedence::LessGreater);
		map.insert(TokenType::Gt, Precedence::LessGreater);
		map.insert(TokenType::Plus, Precedence::Sum);
		map.insert(TokenType::Minus, Precedence::Sum);
		map.insert(TokenType::Slash, Precedence::Product);
		map.insert(TokenType::Asterix, Precedence::Product);
		
		map
		};
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
			Expression{token: Token{token_type: TokenType::Eof, literal: String::new()}}
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
	
	struct Statement {
		token: Token
	}
	
	impl StatementTrait for Statement {
		fn token_literal(&self) -> &str {
			&self.token.literal
		}
		
		fn to_string(&self) -> String {
			format!("{}", self.token.literal)
		}
	}
	
	impl Statement {
		pub fn new() -> Statement {
			Statement{
				token: Token{token_type: TokenType::Eof, literal: EOF.to_string()}
			}
		}
	}
	
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
	
	struct BoolExpression {
		token: Token,
		value: bool
	}
	
	impl BoolExpression {
		fn new() -> BoolExpression {
			BoolExpression{token: Token::new(), value: false}
		}
	}
	
	impl ExpressionTrait for BoolExpression {
		fn token_literal(&self) -> &str {
			self.token.literal.as_str()
		}
		
		fn to_string(&self) -> String {
			self.token.literal.clone()
		}
	}
	
	struct IfExpression {
		token: Token,
		condition: Box<dyn ExpressionTrait>,
		consequence: Box<dyn StatementTrait>,
		alternative: Option<Box<dyn StatementTrait>>
	}
	
	impl IfExpression {
		fn new() -> IfExpression {
			IfExpression {token: Token::new(),
				condition: Box::new(Expression::new()),
				consequence: Box::new(Statement::new()),
				alternative: None
			}
		}
	}
	
	impl ExpressionTrait for IfExpression {
		fn token_literal(&self) -> &str {
			self.token.literal.as_str()
		}
		
		fn to_string(&self) -> String {
			format!("if {} {} {} {}",
				self.condition.to_string(),
				self.consequence.to_string(),
				match &self.alternative {
					Some(_) => String::from("else"),
					None => String::new()
				},
				match &self.alternative {
					Some(a) => a.to_string(),
					None => String::new()
				}
			)
		}
	}
	
	struct PrefixExpression {
		token: Token,
		operator: String,
		right: Option<Box<dyn ExpressionTrait>>
	}
	
	impl PrefixExpression {
		fn new() -> PrefixExpression {
			PrefixExpression{token: Token::new(), operator: String::new(), right: None}
		}
	}
	
	impl ExpressionTrait for PrefixExpression {
		fn token_literal(&self) -> &str {
			self.token.literal.as_str()
		}
		
		fn to_string(&self) -> String {
			format!("({}{})", self.operator, 
			match &self.right {
				Some(expr) => expr.to_string(),
				None => String::from("<empty>")
			})
		}
	}
	
	struct InfixExpression {
		token: Token,
		left: Option<Box<dyn ExpressionTrait>>,
		operator: String,
		right: Option<Box<dyn ExpressionTrait>>
	}
	
	impl InfixExpression {
		fn new() -> InfixExpression {
			InfixExpression{token: Token::new(), left: None, operator: String::new(), right: None}
		}
	}
	
	impl ExpressionTrait for InfixExpression {
		fn token_literal(&self) -> &str {
			self.token.literal.as_str()
		}
		
		fn to_string(&self) -> String {
			format!("({} {} {})",
				match &self.left {
					Some(expr) => expr.to_string(),
					None => String::from("<empty>")
				},
				self.operator, 
				match &self.right {
					Some(expr) => expr.to_string(),
					None => String::from("<empty>")
				})
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
		pub fn new() -> LetStatement {
			LetStatement{
				token: Token{token_type: TokenType::Let, literal: LET.to_string()},
				name: String::new(),
				value: Expression::new()
			}
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
		pub fn new() -> ReturnStatement {
			ReturnStatement{
				token: Token{token_type: TokenType::Return, literal: RETURN.to_string()},
				return_value: Expression::new()
			}
		}
	}
	
	struct ExpressionStatement {
		token: Token,
		expression: Box<dyn ExpressionTrait>
	}
	
	impl ExpressionStatement {
		pub fn new() -> ExpressionStatement {
			ExpressionStatement{
				token: Token{token_type: TokenType::Identifier, literal: String::new()},
				expression: Box::new(Expression::new())
			}
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
	
	struct BlockStatement {
		token: Token, // the { token
		statements: Vec<Box<dyn StatementTrait>>
	}
	
	impl BlockStatement {
		pub fn new() -> BlockStatement {
			BlockStatement{
				token: Token{token_type: TokenType::Lbrace, literal: LBRACE.to_string()},
				statements: vec![]
			}
		}
	}
	
	impl StatementTrait for BlockStatement {
		fn token_literal(&self) -> &str {
			&self.token.literal
		}
		
		fn to_string(&self) -> String {
			self.statements.iter().map(|i| i.to_string()).collect::<String>()
		}
	}
	
	struct FunctionExpression {
		token: Token, // fn token
		parameters: Vec<IdentifierExpression>,
		body: Option<BlockStatement>
	}
	
	impl FunctionExpression {
		pub fn new() -> FunctionExpression {
			FunctionExpression{
				token: Token{token_type: TokenType::Function, literal: FUNCTION.to_string()},
				parameters: vec![],
				body: None
			}
		}
	}
	
	impl ExpressionTrait for FunctionExpression {
		fn token_literal(&self) -> &str {
			&self.token.literal
		}
		
		fn to_string(&self) -> String {
			let params = self.parameters.iter().map(|i| i.to_string()).collect::<Vec<String>>().join(", ");
			format!("{}({}) {}", self.token_literal(), params, 
				match &self.body {
					Some(body) => body.to_string(),
					None => "<empty>".to_string()
				})
		}
	}
	
	struct CallExpression {
		token: Token, // ( token
		function: Option<Box<dyn ExpressionTrait>>, // identifier or function
		arguments: Vec<Box<dyn ExpressionTrait>>
	}
	
	impl CallExpression {
		pub fn new() -> CallExpression {
			CallExpression{
				token: Token{token_type: TokenType::Lparen, literal: LPAREN.to_string()},
				function: None,
				arguments: vec![]
			}
		}
	}
	
	impl ExpressionTrait for CallExpression {
		fn token_literal(&self) -> &str {
			&self.token.literal
		}
		
		fn to_string(&self) -> String {
			let args = self.arguments.iter().map(|i| i.to_string()).collect::<Vec<String>>().join(", ");
			format!("{}({})", self.function.to_string(), args)
		}
	}
	
	pub struct Program {
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
	
	pub struct Parser {
		lexer: Lexer,
		errors: Vec<String>,
		current_token: Token,
		peek_token: Token
	}
	
	
	
	impl Parser {
		pub fn new(lex: Lexer) -> Parser {
			let mut parser = Parser{lexer: lex,
				errors: vec![],
				current_token: Token{token_type: TokenType::Eof, literal: EOF.to_string()},
				peek_token: Token{token_type: TokenType::Eof, literal: EOF.to_string()}
			};
			
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
		
		fn next_token(&mut self) {
			self.current_token = self.peek_token.clone();
			self.peek_token = self.lexer.next_token();
			println!("next_token {:?}", self.current_token.clone());
		}
		
		pub fn parse_program(&mut self) -> Program {
			println!("parse_program");
			let mut program = Program::new();
			
			loop {
				println!("parse_program - parse next token");
				match self.current_token.token_type {
					TokenType::Eof => break,
					_ => {
						if let Some(statement) = self.parse_statement() {
							program.statements.push(statement);
						} else {
							break;
						}
						
						self.next_token();
					}
				}
			}
			
			return program;
		}
		
		fn parse_statement(&mut self) -> Option<Box<dyn StatementTrait>> {
			println!("parse_statement");
			match self.current_token.token_type {
				TokenType::Let => self.parse_let_statement(),
				TokenType::Return => self.parse_return_statement(),
				_ => self.parse_expression_statement()
			}
		}
		
		fn parse_let_statement(&mut self) -> Option<Box<dyn StatementTrait>> {
			println!("parse_let_statement");
			
			let stmt = Box::new(LetStatement::new());
			
			if !self.expect_peek(TokenType::Identifier) {
				None
			}
			
			while !self.is_current_token(TokenType::Semicolon) {
				self.next_token();
			}
			
			return Some(stmt);
		}
		
		fn parse_return_statement(&mut self) -> Option<Box<dyn StatementTrait>> {
			println!("parse_return_statement");
			
			let stmt = Box::new(ReturnStatement::new());
			
			self.next_token();
			
			while !self.is_current_token(TokenType::Semicolon) {
				self.next_token();
			}
			
			return Some(stmt);
		}
		
		fn parse_expression_statement(&mut self) -> Option<Box<dyn StatementTrait>> {
			println!("parse_expression_statement");
			
			if let Some(expr) = self.parse_expression(Precedence::Lowest) {
				if !self.is_current_token(TokenType::Semicolon) {
					self.next_token();
				}
				
				Some(Box::new(ExpressionStatement{
					token: self.current_token.clone(),
					expression: expr}))
			} else {
				None
			}
		}
		
		fn parse_identifier(&self) -> Option<Box<dyn ExpressionTrait>> {
			println!("parse_identifier");
			
			Some(Box::new(IdentifierExpression{token: self.current_token.clone(),
				value: self.current_token.literal.clone()}))
		}
		
		fn parse_integer(&self) -> Option<Box<dyn ExpressionTrait>> {
			println!("parse_integer");
			
			if let Ok(i) = self.current_token.literal.parse::<i64>() {
				Some(Box::new(IntegerExpression{token: self.current_token.clone(),
					value: i}))
			} else {
				None
			}
		}
		
		fn parse_boolean(&self) -> Option<Box<dyn ExpressionTrait>> {
			println!("parse_boolean");
			
			Some(Box::new(BoolExpression{token: self.current_token.clone(),
				value: self.is_current_token(TokenType::True)
			}))
		}
		
		fn parse_grouped_expression(&mut self) -> Option<Box<dyn ExpressionTrait>> {
			println!("parse_grouped_expression");
			
			if self.expect_peek(TokenType::Rparen) {
				self.parse_expression(Precedence::Lowest)
			} else {
				None
			}
		}
		
		fn parse_prefix(&mut self) -> Option<Box<dyn ExpressionTrait>> {
			println!("parse_prefix");
		
			let mut expr = PrefixExpression{token: self.current_token.clone(),
				operator: self.current_token.literal.clone(),
				right: None
			};
			
			self.next_token();
			expr.right = self.parse_expression(Precedence::Prefix);
			return Some(Box::new(expr));
		}
		
		fn parse_infix(&mut self, left: Option<Box<dyn ExpressionTrait>>) -> Option<Box<dyn ExpressionTrait>> {
			println!("parse_infix");
			let mut expr = InfixExpression{
				token: self.current_token.clone(),
				left: left,
				operator: self.current_token.literal.clone(),
				right: None
			};
			
			let prec = self.current_precedence();
			self.next_token();
			expr.right = self.parse_expression(prec);
			return Some(Box::new(expr));
		}
		
		fn parse_block_statement(&mut self) -> Option<BlockStatement> {
			let mut block = BlockStatement{
				token: self.current_token.clone(),
				statements: vec![]
			};
			
			self.next_token();
			while !self.is_current_token(TokenType::Rbrace) && 
				!self.is_current_token(TokenType::Eof) {
				if let Some(stmt) = self.parse_statement() {
					block.statements.push(stmt);
				}
				
				self.next_token();
			}
			
			return Some(block);
		}
		
		fn parse_function_parameters(&mut self) -> Vec<IdentifierExpression> {
			if self.is_peek_token(TokenType::Rparen) {
				self.next_token();
				vec![]
			}
			
			self.next_token();
			let mut idents: Vec<IdentifierExpression> = vec![IdentifierExpression{
				token: self.current_token.clone(), value: self.current_token.literal.clone()}];
			
			while self.is_peek_token(TokenType::Comma) {
				self.next_token();
				self.next_token();
				idents.push(IdentifierExpression{token: self.current_token.clone(), 
					value: self.current_token.literal.clone()});
			}
			
			if !self.is_peek_token(TokenType::Rparen) {
				vec![]
			}
			
			return idents;
		}
		
		fn parse_function(&mut self) -> Option<Box<dyn ExpressionTrait>> {
			if !self.expect_peek(TokenType::Lparen) {
				None
			}
			
			let mut func = Box::new(FunctionExpression::new());
			func.parameters = self.parse_function_parameters();
			
			if !self.expect_peek(TokenType::Lbrace) {
				None
			}
			
			func.body = self.parse_block_statement();
			return Some(func);
		}
		
		fn parse_call_arguments(&mut self) -> Vec<Box<dyn ExpressionTrait> {
			if self.is_peek_token(TokenType::Rparen) {
				self.next_token();
				vec![]
			}
			
			self.next_token();
			let mut args: Vec<Box<dyn ExpressionTrait>> = vec![self.parse_expression(Precedence::Lowest)];
			
			while self.is_peek_token(TokenType::Comma) {
				self.next_token();
				self.next_token();
				args.push(self.parse_expression(Precedence::Lowest));
			}
			
			if !self.expect_peek(TokenType::Rparen) {
				vec![]
			}
			
			return args;
		}
		
		fn parse_call_expression(&mut self,  left: Option<Box<dyn ExpressionTrait>>) -> Option<Box<dyn ExpressionTrait>> {
			Some(Box::new(CallExpression{token: self.current_token.clone(),
				function: left,
				arguments: self.parse_call_arguments()
			}))
		}
		
		fn parse_expression(&mut self, prec: Precedence) -> Option<Box<dyn ExpressionTrait>> {
			println!("parse_expression");
			// parse prefix 
			let mut left_expr = match &self.current_token.token_type {
				TokenType::Identifier => self.parse_identifier(),
				TokenType::Integer => self.parse_integer(),
				TokenType::Bang => self.parse_prefix(),
				TokenType::Minus => self.parse_prefix(),
				TokenType::True => self.parse_boolean(),
				TokenType::False => self.parse_boolean(),
				TokenType::Lparen => self.parse_grouped_expression(),
				TokenType::Function => self.parse_function(),
				_ => {
					self.no_prefix_parse_fn_error(self.current_token.token_type.clone());
					return None;
				}
			};
			
			// parse infix
			while self.is_peek_token(TokenType::Semicolon) && prec < self.peek_precedence() {
				let next_prec = PRECEDENCES.get(&self.peek_token.token_type);
				if next_prec == None {
					return left_expr;
				}
				
				self.next_token();
				
				left_expr = match &self.current_token.token_type {
					TokenType::Lparen => self.parse_call_expression(left_expr),
					_ => self.parse_infix(left_expr)
				}
			}
			
			return left_expr;
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
		
		fn no_prefix_parse_fn_error(&mut self, tok: TokenType) {
			self.errors.push(format!("no prefix parse function for {:?} found", tok))
		}
		
		fn peek_precedence(&self) -> Precedence {
			match PRECEDENCES.get(&self.peek_token.token_type) {
				Some(prec) => prec.clone(),
				None => Precedence::Lowest
			}
		}
		
		fn current_precedence(&self) -> Precedence {
			match PRECEDENCES.get(&self.current_token.token_type) {
				Some(prec) => prec.clone(),
				None => Precedence::Lowest
			}
		}
	}
}
