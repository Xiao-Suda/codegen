
use crate::*;

// ------------------------------------------------------------------------------------
// Token ctor shorthands
// ------------------------------------------------------------------------------------

pub mod token {
	use super::*;

	pub fn id(s: &str) -> TokenKind {
		return TokenKind::Id(s.into());
	}

	pub fn strlit(s: &str) -> TokenKind {
		return TokenKind::StrLit(s.into());
	}

	pub fn intlit(i: i64) -> TokenKind {
		return TokenKind::IntLit(i, Base::Dec);
	}
}

// ------------------------------------------------------------------------------------
// AST ctor shorthands
// ------------------------------------------------------------------------------------

pub mod exp {
	use super::*;

	pub fn new_id(s: &str) -> Box<Exp> {
		return Exp::new(0, ExpKind::Id(Ident::new(0, s)));
	}

	pub fn new_int_lit(v: i64)  -> Box<Exp> {
		return Exp::new(0, ExpKind::IntLit(v));
	}

	pub fn new_bool_lit(v: bool) -> Box<Exp> {
		return Exp::new(0, ExpKind::BoolLit(v));
	}

	pub fn new_str_lit(s: &str) -> Box<Exp> {
		return Exp::new(0, ExpKind::StrLit(s.into()));
	}

	pub fn new_null() -> Box<Exp> {
		return Exp::new(0, ExpKind::Null);
	}

	pub fn new_unary(op: UnOp, lhs: Box<Exp>) -> Box<Exp> {
		return Exp::new(0, ExpKind::Unary { op, lhs });
	}

	pub fn new_binary(lhs: Box<Exp>, op: BinOp, rhs: Box<Exp>) -> Box<Exp> {
		return Exp::new(0, ExpKind::Binary { op, lhs, rhs });
	}

	pub fn new_func_call(callee: Box<Exp>, args: Vec<Box<Exp>>) -> Box<Exp> {
		return Exp::new(0, ExpKind::Call { callee, args });
	}

	pub fn new_field(obj: Box<Exp>, name: Ident) -> Box<Exp> {
		return Exp::new(0, ExpKind::Field { obj, name });
	}

	pub fn new_new(ty: Box<AstType>) -> Box<Exp> {
		return Exp::new(0, ExpKind::New(ty));
	}

	pub fn new_parens(exp: Box<Exp>) -> Box<Exp> {
		return Exp::new(0, ExpKind::Parens(exp));
	}
}

pub mod stmt {
	use super::*;

	pub fn new_block(stmts: Vec<Box<Stmt>>) -> Box<Stmt> {
		return Stmt::new(0, StmtKind::Block(stmts));
	}

	pub fn new_if(cond: Box<Exp>, then: Box<Stmt>, else_: Option<Box<Stmt>>)
	-> Box<Stmt> {
		return Stmt::new(0, StmtKind::If { cond, then, else_ });
	}

	pub fn new_while(cond: Box<Exp>, code: Box<Stmt>) -> Box<Stmt> {
		return Stmt::new(0, StmtKind::While { cond, code });
	}

	pub fn new_for(var: VarDecl, hi: Box<Exp>, code: Box<Stmt>) -> Box<Stmt> {
		return Stmt::new(0, StmtKind::For { var, hi, code });
	}

	pub fn new_exp(exp: Box<Exp>) -> Box<Stmt> {
		return Stmt::new(0, StmtKind::Exp(exp));
	}

	pub fn new_assign(dst: Box<Exp>, src: Box<Exp>) -> Box<Stmt> {
		return Stmt::new(0, StmtKind::Assign { dst, src });
	}

	pub fn new_return(val: Option<Box<Exp>>) -> Box<Stmt> {
		return Stmt::new(0, StmtKind::Return(val));
	}

	pub fn new_let(var: VarDecl) -> Box<Stmt> {
		return Stmt::new(0, StmtKind::Let(var));
	}
}

pub mod asttype {
	use super::*;

	pub fn new_void() -> Box<AstType> {
		return AstType::new(0, AstTypeKind::Void);
	}

	pub fn new_bool() -> Box<AstType> {
		return AstType::new(0, AstTypeKind::Bool);
	}

	pub fn new_int() -> Box<AstType> {
		return AstType::new(0, AstTypeKind::Int);
	}

	pub fn new_string() -> Box<AstType> {
		return AstType::new(0, AstTypeKind::String);
	}

	pub fn new_func(args: Vec<Box<AstType>>, ret: Box<AstType>) -> Box<AstType> {
		return AstType::new(0, AstTypeKind::Func { args, ret });
	}

	pub fn new_struct(id: Ident) -> Box<AstType> {
		return AstType::new(0, AstTypeKind::Struct(id));
	}
}

pub mod misc {
	use super::*;
	pub fn ident(s: &str) -> Ident {
		return Ident::new(0, s);
	}

	pub fn idexp(s: &str) -> Box<Exp> {
		return exp::new_id(s);
	}

	pub fn call(callee: &str, args: Vec<Box<Exp>>) -> Box<Exp> {
		return exp::new_func_call(idexp(callee), args);
	}

	pub fn call_stmt(callee: &str, args: Vec<Box<Exp>>) -> Box<Stmt> {
		return stmt::new_exp(call(callee, args));
	}

	pub fn vardecl(name: &str, init: Box<Exp>) -> VarDecl {
		return VarDecl { loc: 0, name: ident(name), init };
	}

	pub fn funcarg(name: &str, ty: Box<AstType>) -> FuncArg {
		return FuncArg { name: ident(name), ty };
	}

	pub fn field(name: &str, ty: Box<AstType>) -> Field {
		return Field { name: ident(name), ty };
	}

	pub fn prog(items: Vec<Box<Decl>>) -> Program {
		return Program { items };
	}

	pub fn letstmt(name: &str, init: Box<Exp>) -> Box<Stmt> {
		return stmt::new_let(VarDecl { loc: 0, name: ident(name), init });
	}
}