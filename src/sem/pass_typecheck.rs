
use crate::*;

pub(crate) struct TypecheckPass<'nc> {
	nc: &'nc NameCtx,
	tc: TypeCtx,
}

impl<'nc> TypecheckPass<'nc> {
	/// Runs the typechecking pass on a program. On success, gives a `NameCtx` which
	/// contains everything name-related: the scope tree, symbol table, declarations map,
	/// and use map.
	pub(crate) fn run(prog: &Program, nc: &'nc NameCtx) -> SemResult<TypeCtx> {
		let mut p = Self::new(nc);
		p.add_stdlib();
		p.visit_program(prog)?;
		return Ok(p.finish());
	}

	/// Runs the typechecking pass on a single statement. On success, gives a `NameCtx` which
	/// contains everything name-related: the scope tree, symbol table, declarations map,
	/// and use map.
	pub(crate) fn run_stmt(stmt: &Box<Stmt>, nc: &'nc NameCtx) -> SemResult<TypeCtx> {
		let mut p = Self::new(nc);
		p.add_stdlib();
		p.visit_stmt(stmt, &Type::new_void())?;
		return Ok(p.finish());
	}

	/// Runs the typechecking pass on a single expression. On success, gives a `NameCtx` which
	/// contains everything name-related: the scope tree, symbol table, declarations map,
	/// and use map.
	pub(crate) fn run_exp(exp: &Box<Exp>, nc: &'nc NameCtx) -> SemResult<TypeCtx> {
		let mut p = Self::new(nc);
		p.add_stdlib();
		p.visit_exp(exp)?;
		return Ok(p.finish());
	}

	fn new(nc: &'nc NameCtx) -> Self {
		return Self {
			nc,
			tc: TypeCtx::new(),
		};
	}

	fn finish(self) -> TypeCtx {
		return self.tc;
	}

	// -------------------------------------------------------------------------------------------
	// helpers

	/// Adds the "standard library" functions' type signatures to the type context.
	fn add_stdlib(&mut self) {
		let g = self.nc.get_globals();

		self.tc.set_sym_type(self.nc.lookup(g, "print_i").unwrap(), Type::new_func(
			vec![Type::new_int()],
			Type::new_void(),
		));
		self.tc.set_sym_type(self.nc.lookup(g, "print_s").unwrap(), Type::new_func(
			vec![Type::new_string()],
			Type::new_void(),
		));
		self.tc.set_sym_type(self.nc.lookup(g, "print_c").unwrap(), Type::new_func(
			vec![Type::new_int()],
			Type::new_void(),
		));
		self.tc.set_sym_type(self.nc.lookup(g, "print_b").unwrap(), Type::new_func(
			vec![Type::new_bool()],
			Type::new_void(),
		));
		self.tc.set_sym_type(self.nc.lookup(g, "println_i").unwrap(), Type::new_func(
			vec![Type::new_int()],
			Type::new_void(),
		));
		self.tc.set_sym_type(self.nc.lookup(g, "println_s").unwrap(), Type::new_func(
			vec![Type::new_string()],
			Type::new_void(),
		));
		self.tc.set_sym_type(self.nc.lookup(g, "println_c").unwrap(), Type::new_func(
			vec![Type::new_int()],
			Type::new_void(),
		));
		self.tc.set_sym_type(self.nc.lookup(g, "println_b").unwrap(), Type::new_func(
			vec![Type::new_bool()],
			Type::new_void(),
		));
		self.tc.set_sym_type(self.nc.lookup(g, "rand").unwrap(), Type::new_func(
			vec![Type::new_int()],
			Type::new_int(),
		));
	}

	/// Gets the symbol declared by `ident` (from the symbols pass) and sets the type
	/// of the symbol that it declares. For example, variable decls have their types
	/// set by this method.
	fn set_decl_type(&mut self, ident: &Ident, ty: Box<Type>) {
		let sym_id = self.nc.sym_declared_by(ident).expect("node is not a declaration");
		self.tc.set_sym_type(sym_id, ty);
	}

	/// The core of the typechecking algorithm. Checks if two types are equal to each other.
	/// Also accounts for checking the phony 'null' type against reference types.
	fn types_match(&self, act: &Box<Type>, exp: &Box<Type>) -> bool {
		return **act == **exp ||                 // exact match?
		(act.is_null() && exp.is_reference()) || // null + reference?
		(exp.is_null() && act.is_reference());   // reference + null?
	}

	/// Wrapper around `types_match` which returns a type mismatch error on failure.
	fn check_types_match(&self, loc: usize, act: &Box<Type>, exp: &Box<Type>, ctx: &str)
	-> SemResult<()> {
		if !self.types_match(act, exp) {
			return Err(SemError::type_mismatch(loc, &exp.to_string(), &act.to_string(), ctx));
		} else {
			return Ok(());
		}
	}

	/// Checks the lhs of an assignment to make sure it's valid.
	/// The only valid assignment LHSes are variables and field accesses.
	/// The object of a field access doesn't matter cause it could be e.g. `f().x`, that's ok.
	fn check_assignment_lhs(&self, dst: &Box<Exp>) -> SemResult<()> {
		let is_ok = match &dst.kind {
			ExpKind::Id(id) => self.nc.symbol(self.nc.sym_used_by(id).unwrap()).is_variable(),
			ExpKind::Field { .. } => true,
			_ => false,
		};

		if is_ok {
			return Ok(());
		} else {
			return Err(SemError::invalid_lhs(&dst));
		}
	}

	/// Shorthand for getting the symbol that an identifier references.
	fn get_referenced_symbol(&self, user: &Ident) -> &Symbol {
		return self.nc.symbol(self.nc.sym_used_by(user).expect("invalid symref"));
	}

	/// Shorthand for getting the type of a symbol.
	#[allow(dead_code)]
	fn get_sym_type(&self, sym_id: SymbolId) -> Option<Box<Type>> {
		return self.tc.get_sym_type(sym_id);
	}

	/// Helper to make errors for bad function calls.
	#[allow(dead_code)]
	fn function_type_error(&self, e: &Box<Exp>, actual: &Box<Type>) -> SemError {
		return SemError::type_mismatch(e.loc(), "function type", &actual.to_string(), &e.to_string());
	}

	/// A little bit of name resolution in the typechecking phase. Looks up the name of
	/// a field in a struct value. E.g. in `obj.x`, this is used to look up `x`.
	#[allow(dead_code)]
	fn lookup_struct_field(&self, struct_sym_id: SymbolId, name: &Ident) -> SemResult<SymbolId> {
		// must be a struct...
		assert!(self.nc.symbol(struct_sym_id).kind == SymbolKind::Struct);
		let field_scope = self.nc.symbol_scope(struct_sym_id).unwrap();
		if let Some(sym_id) = self.nc.lookup_no_traverse(field_scope, &name.name) {
			if self.nc.symbol(sym_id).is_field() {
				return Ok(sym_id);
			}
		}

		return Err(SemError::no_field_named(&name));
	}

	/// Like `lookup_struct_field` but for methods instead.
	#[allow(dead_code)]
	fn lookup_struct_method(&self, struct_sym_id: SymbolId, name: &Ident) -> SemResult<SymbolId> {
		// must be a struct...
		assert!(self.nc.symbol(struct_sym_id).kind == SymbolKind::Struct);
		let field_scope = self.nc.symbol_scope(struct_sym_id).unwrap();
		if let Some(sym_id) = self.nc.lookup_no_traverse(field_scope, &name.name) {
			if self.nc.symbol(sym_id).is_function() {
				return Ok(sym_id);
			}
		}

		return Err(SemError::no_method_named(&name));
	}

	/// Checks that the type `ty` actually refers to a struct. If so, returns the symbol id of the
	/// struct that it refers to. If not, returns an error at the location of `exp`.
	#[allow(dead_code)]
	fn check_struct_type(&self, exp: &Exp, ty: &Box<Type>) -> SemResult<SymbolId> {
		if let Type::Struct(ret) = **ty {
			return Ok(ret);
		} else {
			return Err(SemError::type_mismatch(exp.loc(), "struct type", &ty.to_string(), &exp.to_string()));
		}
	}

	/// For function calls, checks the argument types. Makes sure the number of arguments matches
	/// what the function expects, and then checks that each argument is the right type.
	#[allow(dead_code)]
	fn check_func_args(&mut self, loc: usize, args: &[Box<Exp>], arg_types: &[Box<Type>])
	-> SemResult<()> {
		// check number of args
		if arg_types.len() != args.len() {
			return Err(SemError::wrong_num_args(loc, arg_types.len(), args.len()));
		}

		// then check if the args match
		for i in 0 .. arg_types.len() {
			let arg_exp_type = self.visit_exp(&args[i])?;
			self.check_types_match(args[i].loc(), &arg_exp_type, &arg_types[i],
				&format!("argument {}", i + 1))?;
		}

		return Ok(());
	}

	// -------------------------------------------------------------------------------------------
	// Visiting Program, Types, Decls

	fn visit_program(&mut self, prog: &Program) -> SemResult<()> {
		// first pass does global vars and struct fields...
		for d in &prog.items {
			match d.as_ref() {
				Decl::Var(decl)    => self.visit_global_var_decl(decl)?,
				Decl::Func(decl)   => self.visit_func_decl_signature(decl)?,
				Decl::Struct(decl) => self.visit_struct_decl_fields_and_signatures(decl)?,
			}
		}

		// then we do functions and struct methods.
		for d in &prog.items {
			match d.as_ref() {
				Decl::Func(decl)   => self.visit_func_decl_code(decl)?,
				Decl::Struct(decl) => self.visit_struct_decl_methods(decl)?,
				_ => {}
			}
		}

		return Ok(());
	}

	/// Turns AST types into real types. This also checks that types referred to by name
	/// (i.e. struct type names) really are types, and aren't like, the names of functions.
	fn visit_type(&mut self, ty: &Box<AstType>) -> SemResult<Box<Type>> {
		match &ty.kind {
			AstTypeKind::Void   => return Ok(Type::new_void()),
			AstTypeKind::Bool   => return Ok(Type::new_bool()),
			AstTypeKind::Int    => return Ok(Type::new_int()),
			AstTypeKind::String => return Ok(Type::new_string()),

			AstTypeKind::Func { args, ret, .. } => {
				let mut new_args = vec![];

				for a in args {
					new_args.push(self.visit_type(&a)?);
				}

				let ret = self.visit_type(ret)?;
				return Ok(Type::new_func(new_args, ret));
			}

			AstTypeKind::Struct(name) => {
				let sym = self.get_referenced_symbol(name);

				if sym.kind == SymbolKind::Struct {
					return Ok(Type::new_struct(sym.id));
				} else {
					return Err(SemError::not_a_type(&name));
				}
			}
		}
	}

	/// Visits a global variable declaration. Checks that the initializer is an int/string/bool
	/// literal, according to the rules of the language.
	fn visit_global_var_decl(&mut self, decl: &VarDecl) -> SemResult<()> {
		// first check that the initializer is an int/string/bool literal.
		let ty = match &decl.init.kind {
			ExpKind::IntLit(..)  => Type::new_int(),
			ExpKind::StrLit(..)  => Type::new_string(),
			ExpKind::BoolLit(..) => Type::new_bool(),
			_ => return Err(SemError::non_constant_init(&decl.name)),
		};

		// now to actually *set* the node type by visiting it
		self.visit_exp(&decl.init)?;

		// and then set the declaration's type.
		self.set_decl_type(&decl.name, ty);
		return Ok(());
	}

	/// Visits a local variable declaration. Does not allow variables to be of type void or
	/// null (cause null could be any type).
	fn visit_local_var_decl(&mut self, decl: &VarDecl) -> SemResult<Box<Type>> {
		let ty = self.visit_exp(&decl.init)?;

		// no void/null variables allowed!
		if ty.is_void() {
			return Err(SemError::bad_var_type(&decl.name));
		} else if ty.is_null() {
			return Err(SemError::null_var(&decl.name));
		}

		self.set_decl_type(&decl.name, ty.clone());
		return Ok(ty);
	}

	/// Visits a field declaration in a struct decl. Fields cannot be void.
	fn visit_field(&mut self, field: &Field) -> SemResult<()> {
		let ty = self.visit_type(&field.ty)?;

		// no void field types allowed!
		if ty.is_void() {
			return Err(SemError::bad_field_type(&field.name));
		}

		self.set_decl_type(&field.name, ty);
		return Ok(());
	}

	/// Visits an argument declaration in a func decl. Arguments cannot be void.
	fn visit_arg(&mut self, arg: &FuncArg) -> SemResult<Box<Type>> {
		let ty = self.visit_type(&arg.ty)?;

		// no void arguments allowed!
		if ty.is_void() {
			return Err(SemError::bad_arg_type(&arg.name));
		}

		self.set_decl_type(&arg.name, ty.clone());
		return Ok(ty);
	}

	/// Visits the fields and *just the function signatures* of the methods within a struct.
	fn visit_struct_decl_fields_and_signatures(&mut self, decl: &StructDecl) -> SemResult<()> {
		for field in &decl.fields {
			self.visit_field(field)?;
		}

		for method in &decl.methods {
			self.visit_func_decl_signature(method)?;
		}

		return Ok(());
	}

	/// Visits the *code* of the methods within a struct.
	fn visit_struct_decl_methods(&mut self, decl: &StructDecl) -> SemResult<()> {
		for m in &decl.methods {
			self.visit_func_decl_code(&m)?;
		}

		return Ok(());
	}

	/// Visits *just the signature* of a func decl, and determines the type it should have.
	fn visit_func_decl_signature(&mut self, decl: &FuncDecl) -> SemResult<()> {
		if let Some(this) = &decl.this {
			self.visit_arg(this)?;
		}

		let mut arg_types = vec![];

		for arg in &decl.args {
			arg_types.push(self.visit_arg(arg)?);
		}

		let ret = self.visit_type(&decl.return_type)?;
		self.set_decl_type(&decl.name, Type::new_func(arg_types, ret));
		return Ok(());
	}

	/// Visits the code within a function. Its signature must have already been visited by
	/// `visit_func_decl_signature`, or this method will crash.
	fn visit_func_decl_code(&mut self, decl: &FuncDecl) -> SemResult<()> {
		let sym_id = self.nc.sym_declared_by(&decl.name).expect("???");
		let func_ty = self.get_sym_type(sym_id).expect("???");

		match func_ty.as_ref() {
			Type::Func { ret, .. } => {
				self.visit_stmt(&decl.code, &ret)?;
			}

			_ => unreachable!(),
		}

		return Ok(());
	}

	// -------------------------------------------------------------------------------------------
	// Visiting Statements

	/// Visits a statement. `rty` is the **return type** of the function that contains this
	/// statement. That's used to check `return` statements for correctness. `rty` should be
	/// passed unmodified to any recursive calls to `visit_stmt`.
	fn visit_stmt(&mut self, s: &Box<Stmt>, rty: &Box<Type>) -> SemResult<()> {
		use StmtKind::*;

		match &s.kind {
			Exp(exp) => {
				let exp_ty = self.visit_exp(&exp)?;

				// the type of the expression must be void.
				if !exp_ty.is_void() {
					return Err(SemError::non_void_expr(&exp));
				}
			}

			Let(var) => {
				self.visit_local_var_decl(var)?;
			}

			Block(stmts) => {
				for s in stmts {
					self.visit_stmt(&s, rty)?;
				}
			}

			Return(val) => {
				match val {
					Some(exp) => {
						let exp_ty = self.visit_exp(&exp)?;
						self.check_types_match(s.loc(), &exp_ty, &rty, "return value")?;
					}
					None => {
						if rty.is_void(){

						} else {
							return Err(SemError::type_mismatch(s.loc(), &rty.to_string(), &Type::new_void().to_string(), "return value"));
						}
					}
				}
			}

			Assign { dst, src } => {
				self.check_assignment_lhs(&dst)?;
				let dst_type = self.visit_exp(&dst)?;
				let src_type = self.visit_exp(&src)?;
				self.check_types_match(s.loc(), &src_type, &dst_type, "rhs of assignment")?;
			}

			While { cond, code } => {
				let cond_type = self.visit_exp(&cond)?;
				self.check_types_match(s.loc(), &cond_type, &Type::new_bool(), "'while' condition")?;
				self.visit_stmt(&code, rty)?;
			}

			If { cond, then, else_ } => {
				let cond_type = self.visit_exp(&cond)?;
				self.check_types_match(s.loc(), &cond_type, &Type::new_bool(), "'if' condition")?;
				self.visit_stmt(&then, rty)?;

				if let Some(e) = else_ {
					self.visit_stmt(&e, rty)?;
				}
			}

			For { var, hi, code } => {
				let var_type = self.visit_local_var_decl(&var)?;
				self.check_types_match(s.loc(), &var_type, &Type::new_int(), "'for' loop lower bound")?;
				let hi_type = self.visit_exp(&hi)?;
				self.check_types_match(s.loc(), &hi_type, &Type::new_int(), "'for' loop upper bound")?;
				self.visit_stmt(&code, rty)?;
			}
		}

		return Ok(());
	}

	// -------------------------------------------------------------------------------------------
	// Visiting Expressions

	/// Visits an expression, and returns the type of that expression on success. Also sets the
	/// expression node's type in the type context.
	fn visit_exp(&mut self, e: &Box<Exp>) -> SemResult<Box<Type>> {
		let ty = self.visit_exp_impl(e)?;
		self.tc.set_node_type(e.id, ty.clone());
		return Ok(ty);
	}

	/// Does the actual work of visiting an expression, and returns the type of that expression
	/// on success.
	fn visit_exp_impl(&mut self, e: &Box<Exp>) -> SemResult<Box<Type>> {
		use ExpKind::*;

		match &e.kind {
			IntLit(..) =>  return Ok(Type::new_int()),
			BoolLit(..) => return Ok(Type::new_bool()),
			StrLit(..) =>  return Ok(Type::new_string()),
			Null =>        return Ok(Type::new_null()),

			Id(ident) => {
				let sym = self.get_referenced_symbol(ident);
				
				match self.get_sym_type(sym.id){
					Some(ty) => 
					{
						return Ok(ty);
					}
					None => {
						return Err(SemError::not_a_value(&ident));
					}

				}
			}

			Field { obj, name } => {
				let obj_ty = self.visit_exp(obj)?;
				let sym = self.check_struct_type(&obj, &obj_ty)?;
				let looked = self.lookup_struct_field(sym, &name)?;

				let meh = self.get_sym_type(looked);

				return Ok(meh.unwrap());
			}

			Unary { op, lhs } => {

				let lhs_ty = self.visit_exp(lhs)?;

				match op {
					UnOp::Neg => 
					{
						self.check_types_match(lhs.loc(), &lhs_ty, &Type::new_int(), &e.to_string())?;
    					return Ok(Type::new_int());
					}

					UnOp::Not => 
					{
						self.check_types_match(lhs.loc(), &lhs_ty, &Type::new_bool(), &e.to_string())?;
   						return Ok(Type::new_bool());
					}
				}
			}

			Binary { op, lhs, rhs } => {
				match op {
					// you can match on multiple possibilities with |
					BinOp::Eq | BinOp::NotEq => {
						let lhs_ty = self.visit_exp(lhs)?;
						if lhs_ty.is_void() {
							return Err(SemError::type_mismatch(lhs.loc(), "non-void type", &lhs_ty.to_string(), &format!("lhs of '{}'", op)));
						}

						let rhs_ty = self.visit_exp(rhs)?;
						self.check_types_match(rhs.loc(), &rhs_ty, &lhs_ty, &format!("rhs of '{}'", op))?;
						return Ok(Type::new_bool());
					}

					BinOp::Less | BinOp::Greater | BinOp::LessEq | BinOp::GreaterEq => {
						let lhs_ty = self.visit_exp(lhs)?;
						self.check_types_match(lhs.loc(), &lhs_ty, &Type::new_int(), &format!("lhs of '{}'", op))?;

						let rhs_ty = self.visit_exp(rhs)?;
						self.check_types_match(rhs.loc(), &rhs_ty, &lhs_ty, &format!("rhs of '{}'", op))?;
						return Ok(Type::new_bool());
					}

					BinOp::And | BinOp::Or => {
						let lhs_ty = self.visit_exp(lhs)?;
						self.check_types_match(lhs.loc(), &lhs_ty, &Type::new_bool(), &format!("lhs of '{}'", op))?;

						let rhs_ty = self.visit_exp(rhs)?;
						self.check_types_match(rhs.loc(), &rhs_ty, &lhs_ty, &format!("rhs of '{}'", op))?;
						return Ok(Type::new_bool());
					}

					BinOp::Add => {
						let lhs_ty = self.visit_exp(lhs)?;
						if lhs_ty.is_int() {
							let rhs_ty = self.visit_exp(rhs)?;
							self.check_types_match(rhs.loc(), &rhs_ty, &lhs_ty, &format!("rhs of '{}'", op))?;
							return Ok(lhs_ty);
						}
						else if lhs_ty.is_string() {
							let rhs_ty = self.visit_exp(rhs)?;
							self.check_types_match(rhs.loc(), &rhs_ty, &lhs_ty, &format!("rhs of '{}'", op))?;
							return Ok(lhs_ty);
						}
						return Err(SemError::type_mismatch(lhs.loc(), "'int' or 'string'", &lhs_ty.to_string(), &format!("lhs of '{}'", op)));
					}

					BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
						let lhs_ty = self.visit_exp(lhs)?;
						self.check_types_match(lhs.loc(), &lhs_ty, &Type::new_int(), &format!("lhs of '{}'", op))?;

						let rhs_ty = self.visit_exp(rhs)?;
						self.check_types_match(rhs.loc(), &rhs_ty, &lhs_ty, &format!("rhs of '{}'", op))?;
						return Ok(Type::new_int());
					}
				}
			}

			Call { callee, args } => {
				let callee_ty;

				if let Field { obj, name } = &callee.kind {
					let obj_ty = self.visit_exp(obj)?;
					let sym = self.check_struct_type(&obj, &obj_ty)?;
					let looked = self.lookup_struct_method(sym, &name)?;
					callee_ty = self.get_sym_type(looked).unwrap();
				} else {
					callee_ty = self.visit_exp(&callee)?;
				}

				if let Type::Func { args: arg_types, ret: ret_type } = *callee_ty {
					self.check_func_args(e.loc(), &args, &arg_types)?;
					return Ok(ret_type);
				} else {
					return Err(self.function_type_error(&e, &callee_ty));
				}
			}

			New(ty) => {
				let struct_ty = self.visit_type(ty)?;
				self.check_struct_type(&e, &struct_ty)?;
				return Ok(struct_ty);
			}

			Parens(exp) => {
				return self.visit_exp(exp);
			}
		}
	}
}
