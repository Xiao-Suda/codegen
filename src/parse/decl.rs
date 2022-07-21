
use crate::*;

impl Parser<'_> {
	// Decl: VarDecl | FuncDecl | StructDecl
	pub(crate) fn parse_decl(&mut self) -> ParseResult<Box<Decl>> {
		return match self.cur() {
			TokenKind::Let    => Ok(Decl::new_var(self.parse_var_decl()?)),
			TokenKind::Fn     => Ok(Decl::new_func(self.parse_func_decl(None)?)),
			TokenKind::Struct => Ok(Decl::new_struct(self.parse_struct_decl()?)),
			_ =>                 Err(ParseError::expected_decl(self.loc(), self.cur())),
		};
	}

	// VarDecl: 'let' Id '=' Exp ';'
	pub(crate) fn parse_var_decl(&mut self) -> ParseResult<VarDecl> {

		let loc = self.loc();
		
		self.expect(TokenKind::Let)?;
		let id = self.parse_ident()?;
		self.expect(TokenKind::Assign)?;
		let lhs = self.parse_exp()?;
		self.expect(TokenKind::Semi)?;

		let var = VarDecl{loc, name: id, init: lhs};

		return Ok(var);
	}

	// FuncDecl: 'fn' Id '(' FuncArgs? ')' (':' Type)? BlockStmt
	fn parse_func_decl(&mut self, struct_: Option<&Ident>) -> ParseResult<FuncDecl> {

		let _ = struct_;
		let loc = self.loc();

		let this = self.struct_name_to_this(loc, struct_);

		self.expect(TokenKind::Fn)?;
		let id = self.parse_ident()?;
		self.expect(TokenKind::LParen)?;

		//args time
		let mut args = vec![];

		if self.cur() != TokenKind::RParen {
			args = self.parse_comma_list(|this| {
				// here, you parse "Id ':' Type", putting the Id in "name" and type in "ty"
				let name = this.parse_ident()?;
				this.expect(TokenKind::Colon)?;
				let ty = this.parse_type()?;
	
				// then return a FuncArg built out of them.
				return Ok(FuncArg { name, ty });
			})?;
		}

		self.expect(TokenKind::RParen)?;

		while self.cur() == TokenKind::Colon {
			if self.cur() == TokenKind::LParen {
				self.expect(TokenKind::Colon)?;
				self.expect(TokenKind::LParen)?;

				//args time
				args = vec![];

				if self.cur() != TokenKind::RParen {
					args = self.parse_comma_list(|this| {
						// here, you parse "Id ':' Type", putting the Id in "name" and type in "ty"
						let name = this.parse_ident()?;
						this.expect(TokenKind::Colon)?;
						let ty = this.parse_type()?;
			
						// then return a FuncArg built out of them.
						return Ok(FuncArg { name, ty });
					})?;
				}

				self.expect(TokenKind::RParen)?;
			}
			else {
				break;
			}
			
		}

		let mut return_type = AstType::new(self.loc(), AstTypeKind::Void);

		if self.cur() == TokenKind::Colon {
			self.expect(TokenKind::Colon)?;
			let ty = self.parse_type()?;
			return_type = AstType::new(self.loc(), ty.kind);
		}

		let code = self.parse_block()?;

		if !return_type.is_void() && !code.returns() {
			return Err(ParseError::no_return(loc, id.name.clone()));
		}

		let func = FuncDecl {loc, name: id, this, args, code, return_type};

		return Ok(func);
	}

	// StructDecl: 'struct' Id (':' Id)? '{' FieldDecls FuncDecl* '}'
	fn parse_struct_decl(&mut self) -> ParseResult<StructDecl> {

		
		let loc = self.loc();
		

		self.expect(TokenKind::Struct)?;
		let id = self.parse_ident()?;
		self.expect(TokenKind::LBrace)?;
		let mut fields = vec![];

		fields = self.parse_comma_list(|this| {
			// here, you parse "Id ':' Type", putting the Id in "name" and type in "ty"
			let name = this.parse_ident()?;
			this.expect(TokenKind::Colon)?;
			let ty = this.parse_type()?;

			// then return a Field built out of them.
			return Ok(Field { name, ty });
		})?;

		let mut methods = vec![];

		while self.cur() == TokenKind::Fn {
			methods.push(self.parse_func_decl(Some(&id))?);
		}

		self.expect(TokenKind::RBrace)?;
		
		let wstruct = StructDecl{loc, name: id, fields, methods};

		return Ok(wstruct);
	}

	// given an optional identifier, constructs a FuncArg for it named 'this' and
	// using that identifier as the type of 'this'. (if given None, returns None.)
	#[allow(dead_code)]
	fn struct_name_to_this(&self, loc: usize, struct_: Option<&Ident>) -> Option<FuncArg> {
		return struct_.map(|struct_name| {
			FuncArg {
				name: Ident::new(loc, "this"),
				ty:   AstType::new(loc, AstTypeKind::Struct(struct_name.dup()))
			}
		});
	}

	// Type:       VoidType | 'bool' | 'int' | 'string' | FuncType | StructType
	// VoidType:   '(' ')'
	// FuncType:   'fn' '(' (Type (',' Type)*)? ')' ':' Type
	// StructType: Id
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