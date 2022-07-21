
use crate::*;

impl Parser<'_> {
	// Stmt: (several kinds)
	pub(crate) fn parse_stmt(&mut self) -> ParseResult<Box<Stmt>> {

		let loc = self.loc();

		match self.cur() {

			TokenKind:: LBrace => {
				return self.parse_block();
			}

			TokenKind:: If => {
				return self.parse_ifs();
			}

			TokenKind:: While => {
				return self.parse_whiles();
			}

			TokenKind:: For => {
				return self.parse_fors();
			}

			TokenKind:: Let => {
				let res = self.parse_var_decl()?;
				let loca = res.loc;
				let name = res.name;
				let init = res.init;

				let var = VarDecl{loc: loca, name, init};
				return Ok(Stmt::new(loc, StmtKind::Let(var)));
			}

			TokenKind:: Return => {
				self.expect(TokenKind:: Return)?;

				// if the current token is NOT a semicolon, parse the expression
				if self.cur() == TokenKind::Semi {
					self.expect(TokenKind:: Semi)?;
					return Ok(Stmt::new(loc, StmtKind::Return(None)));
				} else {
					let lhs = self.parse_exp()?;
					self.expect(TokenKind:: Semi)?;
					return Ok(Stmt::new(loc, StmtKind::Return(Some(lhs))));
				}
			}

			TokenKind::Eof =>{
				return  Err(ParseError::unexpected_eof(self.loc()));
			}

			_ => {
				return self.parse_exp_or_assign_stmt();
			}
		}

		return Err(ParseError::bad_statement(self.loc(), self.cur()));
	}

	fn parse_exp_or_assign_stmt(&mut self) -> ParseResult<Box<Stmt>> {
		let loc = self.loc();

		let lhs = self.parse_exp()?;

		if self.cur() == TokenKind::Semi {
			self.expect(TokenKind:: Semi)?;
			return Ok(Stmt::new(loc, StmtKind::Exp(lhs)));
		}
		else if self.cur() == TokenKind::Assign {
			self.expect(TokenKind:: Assign)?;
			let rhs = self.parse_exp()?;
			self.expect(TokenKind:: Semi)?;
			return Ok(Stmt::new(loc, StmtKind::Assign{dst: lhs, src: rhs}));
		}
		else {
			return Err(ParseError::unexpected_eof(self.loc()));
		}
	}

	pub(crate) fn parse_block(&mut self) -> ParseResult<Box<Stmt>> {
		let loc = self.loc();

		self.expect(TokenKind::LBrace)?;

		let mut stmt_block = vec![];

		while self.cur() != TokenKind::RBrace{
			stmt_block.push(self.parse_stmt()?);

			if stmt_block.last().unwrap().returns() {
				break;
			}
		}

		self.expect(TokenKind::RBrace)?;

		return Ok(Stmt::new(loc, StmtKind::Block(stmt_block)));
	}

	fn parse_whiles(&mut self) -> ParseResult<Box<Stmt>> {
		let loc = self.loc();

		self.expect(TokenKind::While)?;
		let lhs = self.parse_exp()?;
		let stmt = self.parse_block()?;

		return Ok(Stmt::new(loc, StmtKind::While{ cond: lhs, code: stmt }));
	}

	fn parse_fors(&mut self) -> ParseResult<Box<Stmt>> {
		let loc = self.loc();

		self.expect(TokenKind::For)?;
		let id = self.parse_ident()?;

		self.expect(TokenKind::In)?;
		let lhs = self.parse_exp()?;

		self.expect(TokenKind::Comma)?;
		let lhs2 = self.parse_exp()?;
		let stmt = self.parse_block()?;

		let var = VarDecl{loc, name: id, init: lhs};

		return Ok(Stmt::new(loc, StmtKind::For{ var, hi: lhs2, code: stmt}));
		
	}

	
	fn parse_ifs(&mut self) -> ParseResult<Box<Stmt>> {
		let loc = self.loc();

		self.expect(TokenKind::If)?;
		let lhs = self.parse_exp()?;
		let stmt_block = self.parse_block()?;

		if self.cur() != TokenKind::Else {
			return Ok(Stmt::new(loc, StmtKind::If{ cond:  lhs, then: stmt_block, else_: None }));
		}

		self.expect(TokenKind::Else)?;

		if self.cur() == TokenKind::If {
			let else_stmt = self.parse_ifs()?;
			return Ok(Stmt::new(loc, StmtKind::If { cond: lhs, then: stmt_block, else_: Some(else_stmt)}));
		} else {
			let else_stmt = self.parse_block()?;
			return Ok(Stmt::new(loc, StmtKind::If{ cond:  lhs, then: stmt_block, else_: Some(else_stmt) }));	
		}
	}
	

}