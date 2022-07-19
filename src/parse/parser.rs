
use crate::*;

pub(crate) struct Parser<'t> {
	tokens: &'t [Token],
	i:      usize,
}

impl<'t> Parser<'t> {
	// Program: Decl*
	pub(crate) fn parse_program(tokens: &'t [Token]) -> ParseResult<Program> {
		let mut p = Self::new(tokens);

		let mut items = vec![];
		while p.cur() != TokenKind::Eof {
			items.push(p.parse_decl()?);
		}

		p.expect(TokenKind::Eof)?;

		return Ok(Program { items });
	}

	// Convenience methods for testing the parser.
	pub(crate) fn parse_one_stmt(tokens: &'t [Token]) -> ParseResult<Box<Stmt>> {
		let mut p = Self::new(tokens);
		let ret = p.parse_stmt()?;
		p.expect(TokenKind::Eof)?;
		return Ok(ret);
	}

	pub(crate) fn parse_one_exp(tokens: &'t [Token]) -> ParseResult<Box<Exp>> {
		let mut p = Self::new(tokens);
		let ret = p.parse_exp()?;
		p.expect(TokenKind::Eof)?;
		return Ok(ret);
	}

	#[cfg(test)]
	pub(crate) fn parse_one_decl(tokens: &'t [Token]) -> ParseResult<Box<Decl>> {
		let mut p = Self::new(tokens);
		let ret = p.parse_decl()?;
		p.expect(TokenKind::Eof)?;
		return Ok(ret);
	}

	#[cfg(test)]
	pub(crate) fn parse_one_type(tokens: &'t [Token]) -> ParseResult<Box<AstType>> {
		let mut p = Self::new(tokens);
		let ret = p.parse_type()?;
		p.expect(TokenKind::Eof)?;
		return Ok(ret);
	}

	fn new(tokens: &'t [Token]) -> Self {
		return Parser { tokens, i: 0 };
	}

	// Helper methods.
	pub(crate) fn cur(&self) -> TokenKind {
		if self.i < self.tokens.len() {
			return self.tokens[self.i].kind.clone();
		} else {
			return TokenKind::Eof;
		}
	}

	pub(crate) fn loc(&self) -> usize {
		return self.i;
	}

	pub(crate) fn next(&mut self) {
		self.i += 1;
	}

	pub(crate) fn expect(&mut self, expected: TokenKind) -> ParseResult<()> {
		let cur = self.cur();

		if cur != expected {
			if cur == TokenKind::Eof {
				return Err(ParseError::unexpected_eof(self.loc()));
			} else if expected == TokenKind::Eof {
				return Err(ParseError::extra_tokens(self.loc(), cur))
			} else {
				return Err(ParseError::expected_token(self.loc(), expected, cur));
			}
		}

		if expected != TokenKind::Eof {
			self.next();
		}

		return Ok(());
	}

	pub(crate) fn parse_ident(&mut self) -> ParseResult<Ident> {
		match self.cur() {
			TokenKind::Id(name) => {
				let loc = self.loc();
				self.next();
				return Ok(Ident::new(loc, &name));
			}
			_ => return Err(ParseError::expected_name(self.loc(), self.cur())),
		}
	}

	pub(crate) fn parse_comma_list<T>(&mut self,
		mut parse_item: impl FnMut(&mut Self) -> ParseResult<T>)
	-> ParseResult<Vec<T>> {
		let mut ret = vec![];

		ret.push(parse_item(self)?);

		while self.cur() == TokenKind::Comma {
			self.next();
			ret.push(parse_item(self)?);
		}

		return Ok(ret);
	}
}