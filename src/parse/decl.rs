
use colored::Colorize;
use crate::*;

impl Parser<'_> {
	// Decl: VarDecl | FuncDecl | StructDecl
	pub(crate) fn parse_decl(&mut self) -> ParseResult<Box<Decl>> {
		println!("{}", "Hey, this is the CODEGEN project.".red());
		println!("{}", "If you completed the parser in project 2, drop".red());
		println!("{}", "your completed exp.rs, decl.rs, and stmt.rs into".red());
		println!("{}", "the src/parse directory to use it.".red());

		return Err(ParseError::extra_tokens(self.loc(), self.cur()));
	}

	#[allow(dead_code)]
	pub(crate) fn parse_type(&mut self) -> ParseResult<Box<AstType>> {
		let loc = self.loc();

		match self.cur() {
			TokenKind::LParen => {
				self.next();
				self.expect(TokenKind::RParen)?;
				return Ok(AstType::new(loc, AstTypeKind::Void));
			}

			TokenKind::Bool => {
				self.next();
				return Ok(AstType::new(loc, AstTypeKind::Bool));
			}

			TokenKind::Int => {
				self.next();
				return Ok(AstType::new(loc, AstTypeKind::Int));
			}

			TokenKind::String => {
				self.next();
				return Ok(AstType::new(loc, AstTypeKind::String));
			}

			TokenKind::Fn => {
				self.next();
				self.expect(TokenKind::LParen)?;
				let mut args = vec![];

				if self.cur() != TokenKind::RParen {
					args = self.parse_comma_list(|this| this.parse_type())?;
				}

				self.expect(TokenKind::RParen)?;
				self.expect(TokenKind::Colon)?;

				let ret = self.parse_type()?;
				return Ok(AstType::new(loc, AstTypeKind::Func { args, ret }));
			}

			TokenKind::Id(n) => {
				let loc = self.loc();
				self.next();
				return Ok(AstType::new(loc, AstTypeKind::Struct(Ident::new(loc, &n))));
			}

			_ => return Err(ParseError::expected_type(loc, self.cur())),
		}
	}
}