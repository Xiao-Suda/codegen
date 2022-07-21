
use crate::*;

// ------------------------------------------------------------------------------------------------
// Helper functions for testing characters.

/// Corresponds to "DecDigit" in the lexical grammar.
#[allow(dead_code)]
fn is_dec_digit_start(c: char) -> bool {
	return c.is_ascii_digit();
}

/// Corresponds to "HexDigit" in the lexical grammar.
#[allow(dead_code)]
fn is_hex_digit_start(c: char) -> bool {
	return c.is_ascii_hexdigit();
}

/// Corresponds to "BinDigit" in the lexical grammar.
#[allow(dead_code)]
fn is_bin_digit_start(c: char) -> bool {
	return c == '0' || c == '1';
}

/// Corresponds to a few things, but basically allows for a digit or an underscore.
#[allow(dead_code)]
fn is_digit_cont(c: char, base: Base) -> bool {
	match base {
		Base::Dec => { return is_dec_digit_start(c) || c == '_'; }
		Base::Hex => { return is_hex_digit_start(c) || c == '_'; }
		Base::Bin => { return is_bin_digit_start(c) || c == '_'; }
	}
}

/// Corresponds to "IdStart" in the lexical grammar.
#[allow(dead_code)]
fn is_ident_start(c: char) -> bool {
	return c.is_alphabetic() || c == '$' || c == '_';
}

/// Corresponds to "IdCont" in the lexical grammar.
#[allow(dead_code)]
fn is_ident_cont (c: char) -> bool {
	return is_ident_start(c) || c.is_ascii_digit();
}

// ------------------------------------------------------------------------------------------------
// The Lexer object.

pub(crate) struct Lexer<'src> {
	iter:   SourceIter<'src>,
	tokens: Vec<Token>,
}

impl<'src> Lexer<'src> {
	/// The interface used by the rest of the compiler to lex source code.
	pub(crate) fn lex(src: &Source) -> LexResult<Vec<Token>> {
		let mut l = Lexer::new(src);
		l.lex_all()?;
		return Ok(l.tokens);
	}

	/// Constructor; takes the source code and gives a Lexer ready to lex it.
	fn new(src: &'src Source) -> Lexer<'src> {
		return Lexer {
			iter:   src.iter(),
			tokens: Vec::new(),
		};
	}

	/// Returns the current character.
	fn cur(&self) -> char {
		return self.iter.cur();
	}

	/// Returns the current character's location, for use in errors.
	fn loc(&self) -> Location {
		return self.iter.loc();
	}

	/// Returns the character after the current character.
	fn peek(&self) -> char {
		return self.iter.peek();
	}

	/// Moves to the next character.
	fn next(&mut self) {
		self.iter.next()
	}

	/// Moves two characters ahead (like calling .next() twice).
	#[allow(dead_code)]
	fn next_2(&mut self) {
		self.iter.next_2()
	}

	/// Implements the "Source" rule in the Truss lexical grammar. Loops over the
	/// entire input, skipping whitespace, pushing actual tokens into the `tokens` vec,
	/// and returns `Ok(())` if it was successful.
	fn lex_all(&mut self) -> LexResult<()> {
		let mut is_eof = false;

		while !is_eof {
			self.eat_whitespace_and_comments();

			let start  = self.iter.pos();
			let kind   = self.next_token()?;
			let end    = self.iter.pos();
			is_eof     = kind == TokenKind::Eof;

			assert!(end > start || is_eof, "next_token() is stuck!");
			self.tokens.push(Token { span: start .. end, kind });
		}

		return Ok(());
	}

	/// Used by lex_all() to skip whitespace and comments. Nom nom
	fn eat_whitespace_and_comments(&mut self) {
		loop {
			match self.cur() {
				// whitespace
				' ' | '\t' | '\n' => {
					self.next();
				}

				// comments
				'/' => {
					if self.peek() == '/' {
						loop {
							match self.cur() {
								'\n' | '\0' => break,
								_           => self.next(),
							}
						}
					} else {
						// something tokeny?
						break;
					}
				}

				// something tokeny?
				_ => break,
			}
		}
	}

	/// The core lexing method. This is called when lex_all() sees something that isn't
	/// whitespace, so it must be some kind of token... or maybe not. If successful, moves
	/// the character iterator ahead past the token and returns the token kind.
	fn next_token(&mut self) -> LexResult<TokenKind> {
		match self.cur() {
			// EOF
			'\0' => {
				return Ok(TokenKind::Eof);
			}

			// TODO: every other kind of token

			//simple symbols
			'+' => { self.next(); return Ok(TokenKind::Plus); }
			'-' => { self.next(); return Ok(TokenKind::Minus); }
			'*' => { self.next(); return Ok(TokenKind::Times); }
			'/' => { self.next(); return Ok(TokenKind::Divide); }
			'%' => { self.next(); return Ok(TokenKind::Modulo); }
			'(' => { self.next(); return Ok(TokenKind::LParen); }
			')' => { self.next(); return Ok(TokenKind::RParen); }
			'{' => { self.next(); return Ok(TokenKind::LBrace); }
			'}' => { self.next(); return Ok(TokenKind::RBrace); }
			'[' => { self.next(); return Ok(TokenKind::LBracket); }
			']' => { self.next(); return Ok(TokenKind::RBracket); }
			';' => { self.next(); return Ok(TokenKind::Semi); }
			':' => { self.next(); return Ok(TokenKind::Colon); }
			',' => { self.next(); return Ok(TokenKind::Comma); }
			'.' => { self.next(); return Ok(TokenKind::Dot); }

			//complex symbols
			'=' => { 
				if self.peek() == '='{
					self.next_2(); 
					return Ok(TokenKind::Eq);
				}
				else{
					self.next(); 
					return Ok(TokenKind::Assign);
				}
			}
			'!'   => { 
				if self.peek() == '='{
					self.next_2(); 
					return Ok(TokenKind::NotEq);
				}
				else{
					return Err(LexError::invalid_char(self.loc()));
				}
			}
			'<'  => { 
				if self.peek() == '='{
					self.next_2(); 
					return Ok(TokenKind::LessEq);
				}
				else{
					self.next(); 
					return Ok(TokenKind::Less);
				}
			}
			'>' => { 
				if self.peek() == '='{
					self.next_2(); 
					return Ok(TokenKind::GreaterEq);
				}
				else{
					self.next(); 
					return Ok(TokenKind::Greater);
				}
			}

			//Strings
			'"' => {
				return self.lex_string();
			}

			//identifiers
			'$' => {
				if is_ident_start(self.cur()) {
					return self.lex_ident();
				} else {
					return Err(LexError::invalid_char(self.loc()));
				}
			}

			_ => {
				if is_ident_start(self.cur()) {
					return self.lex_ident();
				} else if self.cur().is_numeric() {
					return self.lex_integer();
				}
				else {
					return Err(LexError::invalid_char(self.loc()));
				}
			}
		}
	}

	/// Used by next_token() to lex an identifier (Id) token.
	#[allow(dead_code)]
	fn lex_ident(&mut self) -> LexResult<TokenKind> {
		let mut s = String::new();
		'is_ident_cont: loop {
			loop {
				if self.cur().is_alphabetic() || self.cur().is_numeric() || self.cur() == '$' || self.cur() == '_' {
					s.push(self.cur());
					self.next();
				}
				else{
					break 'is_ident_cont;
				}
			}
		}

		match s.as_str() {
			"if" => return Ok(TokenKind::If),
			"else" => return Ok(TokenKind::Else),
			"for" => return Ok(TokenKind::For),
			"in" => return Ok(TokenKind::In),
			"fn" => return Ok(TokenKind::Fn),
			"let" => return Ok(TokenKind::Let),
			"while" => return Ok(TokenKind::While),
			"break" => return Ok(TokenKind::Break),
			"return" => return Ok(TokenKind::Return),
			"int" => return Ok(TokenKind::Int),
			"bool" => return Ok(TokenKind::Bool),
			"string" => return Ok(TokenKind::String),
			"and" => return Ok(TokenKind::And),
			"or" => return Ok(TokenKind::Or),
			"not" => return Ok(TokenKind::Not),
			"true" => return Ok(TokenKind::True),
			"false" => return Ok(TokenKind::False),
			"struct" => return Ok(TokenKind::Struct),
			"new" => return Ok(TokenKind::New),
			"null" => return Ok(TokenKind::Null),
			_ => return Ok(TokenKind::Id(s)),
		}
	}

	/// Used by next_token() to lex a string literal (StrLit) token.
	#[allow(dead_code)]
	fn lex_string(&mut self) -> LexResult<TokenKind> {

		let mut s = String::new();
		let _loc = self.loc();
		self.next();

		'is_ident_cont: loop {
			loop {
				match self.cur() {
					'"' => {
						//print!("Closing string!");
						break 'is_ident_cont;
					}
					
					'\n' => {
						//print!("Newline error!");
						return Err(LexError::unclosed_string(_loc));	
					}

					'\0' =>{
						//print!("Null error!");
						return Err(LexError::unclosed_string(_loc));
					}	

					'\\' => {
						if self.peek() == 'n' {
							s.push('\n');
							self.next_2(); 
						}
						else if self.peek() == '\\' {
							s.push('\\');
							self.next_2(); 
						}
						else if self.peek() == 't' {
							s.push('\t');
							self.next_2(); 
						}
						else if self.peek() == 'r' {
							s.push('\r');
							self.next_2(); 
						}
						else if self.peek() == '"' {
							s.push('\"');
							self.next_2(); 
						}
						else {
							return Err(LexError::unknown_escape(self.loc()));	
						}

					}
		
					_ => {
						//print!("Adding {} \n", self.cur());
						s.push(self.cur());
						self.next();
					}
				}
			}
		}
		self.next();
		return Ok(TokenKind::StrLit(s));
	
	}

	/// Used by next_token() to lex an integer literal (IntLit) token.
	#[allow(dead_code)]
	fn lex_integer(&mut self) -> LexResult<TokenKind> {

		let start = self.loc();   // for reporting errors
		let mut base = Base::Dec; // by default, but can be changed below

		// 1. check for 0x, 0X, 0b, 0B here and set base to Base::Hex or Base::Bin appropriately
		if self.cur() == '0' {
			if self.peek() == 'x' || self.peek() == 'X'{
				base = Base::Hex;
				self.next_2();
				if !is_hex_digit_start(self.cur()){
					return Err(LexError::incomplete_int(self.loc()));	
				}
			}
			else if self.peek() == 'b' || self.peek() == 'B'{
				base = Base::Bin;
				self.next_2();
				if !is_bin_digit_start(self.cur()){
					return Err(LexError::incomplete_int(self.loc()));	
				}
			}
		}

		// 2. loop to build up a String containing digits
		// 3. check for invalid alphabetic characters (and for binary, invalid digit characters)

		let mut s =  String::new();

		while is_digit_cont(self.cur(), base) {
			if self.cur() == '_' {
				self.next();
				continue;
			}

			s.push(self.cur());
			self.next();
		}

		if self.cur().is_alphabetic() {
			return Err(LexError::invalid_char(self.loc()));
		}

		if self.cur().is_ascii_digit() && base == Base::Bin {
			return Err(LexError::invalid_char(self.loc()));
		}

		// 4. convert to an actual integer
		// 5. if that succeeded, check that it's in the range of a 32-bit number and return it

		match i64::from_str_radix(&s, base as u32) {
			Ok(value) => {
				if value < (i32::MIN as i64) ||  value > (u32::MAX as i64) {
					return Err(LexError::int_range(start));
				} else {
					return Ok(TokenKind::IntLit(value, base));
				}
			}
		
			Err(..) => {
				return Err(LexError::int_range(start));
			}
		}
		
	}
}