
use crate::*;

impl Parser<'_> {
	pub(crate) fn parse_exp(&mut self) -> ParseResult<Box<Exp>> {
		let lhs = self.parse_term()?;
		return self.parse_binops(lhs, Precedence::MIN);
	}

	fn parse_binops(&mut self, mut lhs: Box<Exp>, min_prec: Precedence) -> ParseResult<Box<Exp>> {
		while self.cur().precedence() >= min_prec {
			// put the AST node location on the operator, not the operand
			let loc = self.loc();
			let op = self.cur();
			self.next();

			let mut rhs = self.parse_term()?;

			while self.cur().precedence() > op.precedence() {
				rhs = self.parse_binops(rhs, self.cur().precedence())?;
			}

			lhs = Exp::new(loc, ExpKind::Binary { lhs, op: op.to_binop(), rhs });
		}

		return Ok(lhs);
	}

	fn parse_term(&mut self) -> ParseResult<Box<Exp>> {

		let loc = self.loc();
		match self.cur() {

			TokenKind::Minus => {
				self.next();
				let lhs = self.parse_term()?;
				return Ok(Exp::new(loc, ExpKind::Unary { op: UnOp::Neg, lhs }));
			}

			TokenKind::Not => {
				self.next();
				let lhs = self.parse_term()?;
				return Ok(Exp::new(loc, ExpKind::Unary { op: UnOp::Not, lhs }));
			}

			_ => {
                let pri = self.parse_primary()?;
                return self.parse_postfix(pri);
            }
		}

		return Err(ParseError::bad_expression(self.loc(), self.cur()));
	}

	fn parse_primary(&mut self) -> ParseResult<Box<Exp>> {
        // use this as the first argument to Exp::new
        let loc = self.loc();

        match self.cur() {

			TokenKind::Id(name) => {
				self.next();
				let id = Ident::new(loc, &name);
				return Ok(Exp::new(loc, ExpKind::Id(id)));
			}

			TokenKind::IntLit(num, _base) => {
				self.next();
				return Ok(Exp::new(loc, ExpKind::IntLit(num)));
			}

			TokenKind::StrLit(stri) => {
				self.next();
				return Ok(Exp::new(loc, ExpKind::StrLit(stri)));
			}

			TokenKind::True => {
				self.next();
				return Ok(Exp::new(loc, ExpKind::BoolLit(true)));
			}

			TokenKind::False => {
				self.next();
				return Ok(Exp::new(loc, ExpKind::BoolLit(false)));
			}

			TokenKind::Null => {
				self.next();
				return Ok(Exp::new(loc, ExpKind::Null));
			}

			TokenKind::LParen => {
				self.expect(TokenKind::LParen)?;
				let ret = self.parse_exp()?;
				self.expect(TokenKind::RParen)?;
				return Ok(Exp::new(loc, ExpKind::Parens(ret)));
			}

			TokenKind::New => {
				self.expect(TokenKind::New)?;
				let name = self.parse_ident()?;
				self.expect(TokenKind::LParen)?;
				self.expect(TokenKind::RParen)?;

				let ty = AstType::new(loc, AstTypeKind::Struct(name));
   				return Ok(Exp::new(loc, ExpKind::New(ty)));
			}

            _ => return Err(ParseError::bad_expression(loc, self.cur())),
        }
    }

    // this avoids an error about "mut lhs" but you can remove it later once you assign
    // to lhs.
	#[allow(unused_mut)]
	fn parse_postfix(&mut self, mut lhs: Box<Exp>) -> ParseResult<Box<Exp>> {
		loop {
			// use this as the first argument to Exp::new
			let loc = self.loc();

			// this line avoids an error but you can get rid of it once you use "loc" somewhere.
			let _ = loc;

			match self.cur() {
				// TODO: the Postfix operators are parsed here
				TokenKind::LParen => {
					self.expect(TokenKind::LParen)?;

					let mut args = vec![];

					if self.cur() != TokenKind::RParen  {
						args = self.parse_comma_list(|this| this.parse_exp())?;
					}
					self.expect(TokenKind::RParen)?;

					lhs = Exp::new(loc, ExpKind::Call { callee: lhs, args });
				}

				TokenKind::Dot =>{

					self.expect(TokenKind::Dot)?;
					let name = self.parse_ident()?;
					
					lhs = Exp::new(loc, ExpKind::Field { obj: lhs, name });
				}

				_ => break,
			}
		}

		return Ok(lhs);
	}
}