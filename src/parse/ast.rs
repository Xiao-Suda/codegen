
use std::sync::atomic::{ AtomicUsize, Ordering };
use serde::{ Serialize, Deserialize };

// ------------------------------------------------------------------------------------
// NodeId
// ------------------------------------------------------------------------------------

#[derive(Serialize, Deserialize)]
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct NodeId(pub usize);

static NODE_ID: AtomicUsize = AtomicUsize::new(1);

impl NodeId {
	pub const INVALID: NodeId = NodeId(0);

	pub fn new() -> Self {
		return NodeId(NODE_ID.fetch_add(1, Ordering::SeqCst));
	}
}

// ------------------------------------------------------------------------------------
// HasLocation
// ------------------------------------------------------------------------------------

pub trait HasLocation {
	fn loc(&self) -> usize;
}

// ------------------------------------------------------------------------------------
// Ident
// ------------------------------------------------------------------------------------

#[derive(Serialize, Deserialize)]
pub struct Ident {
	pub id:   NodeId,
	pub loc:  usize,
	pub name: String,
}

impl PartialEq for Ident {
	fn eq(&self, other: &Self) -> bool {
		return self.name == other.name;
	}
}

impl Eq for Ident {}

impl HasLocation for Ident {
	fn loc(&self) -> usize {
		return self.loc;
	}
}

impl Ident {
	pub fn new(loc: usize, name: &str) -> Self {
		return Self { id: NodeId::new(), loc, name: name.into() };
	}

	// Makes a duplicate with the same name/location but a new node id.
	pub fn dup(&self) -> Self {
		return Self::new(self.loc, &self.name);
	}
}

/* -------------------------------------------------------------------------------------
AstType

Grammar:

	Type:       VoidType | 'bool' | 'int' | 'string' | FuncType | StructType
	VoidType:   '(' ')'
	FuncType:   'fn' '(' (Type (',' Type)*)? ')' ':' Type
	StructType: Id
------------------------------------------------------------------------------------- */

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq)]
pub enum AstTypeKind {
	Void,
	Bool,
	Int,
	String,
	Func { args: Vec<Box<AstType>>, ret: Box<AstType> },
	Struct(Ident),
}

#[derive(Serialize, Deserialize)]
#[derive(Debug)]
pub struct AstType {
	pub id:   NodeId,
	pub loc:  usize,
	pub kind: AstTypeKind,
}

impl PartialEq for AstType {
	fn eq(&self, other: &Self) -> bool {
		return self.kind == other.kind;
	}
}

impl Eq for AstType {}

impl HasLocation for AstType {
	fn loc(&self) -> usize {
		return self.loc;
	}
}

impl AstType {
	pub fn new(loc: usize, kind: AstTypeKind) -> Box<Self> {
		return Box::new(AstType { id: NodeId::new(), loc, kind });
	}

	pub fn is_void(&self) -> bool {
		return matches!(self.kind, AstTypeKind::Void);
	}
}

/* -------------------------------------------------------------------------------------
Declarations

Grammar:

	Program:    Decl*
	Decl:       VarDecl | FuncDecl | StructDecl

	VarDecl:    'let' Id '=' Exp ';'

	FuncDecl:   'fn' Id '(' FuncArgs? ')' (':' Type)? BlockStmt
	FuncArgs:   FuncArg (',' FuncArg)*
	FuncArg:    Id ':' Type

	StructDecl: 'struct' Id '{' FieldDecls FuncDecl* '}'
	FieldDecls: FieldDecl (',' FieldDecl)*
	FieldDecl:  Id ':' Type
------------------------------------------------------------------------------------- */

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq)]
pub struct Program {
	pub items: Vec<Box<Decl>>,
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq)]
pub struct FuncArg {
	pub name: Ident,
	pub ty:   Box<AstType>,
}

impl HasLocation for FuncArg {
	fn loc(&self) -> usize {
		return self.name.loc();
	}
}

#[derive(Serialize, Deserialize)]
#[derive(Debug)]
pub struct FuncDecl {
	pub loc:         usize,
	pub name:        Ident,
	pub this:        Option<FuncArg>,
	pub args:        Vec<FuncArg>,
	pub return_type: Box<AstType>,
	pub code:        Box<Stmt>,
}

impl PartialEq for FuncDecl {
	fn eq(&self, other: &FuncDecl) -> bool {
		return self.name == other.name &&
		self.this == other.this &&
		self.args == other.args &&
		self.return_type == other.return_type &&
		self.code == other.code;
	}
}

impl Eq for FuncDecl {}

impl HasLocation for FuncDecl {
	fn loc(&self) -> usize {
		return self.loc;
	}
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq)]
pub struct Field {
	pub name: Ident,
	pub ty:   Box<AstType>,
}

impl HasLocation for Field {
	fn loc(&self) -> usize {
		return self.name.loc();
	}
}

#[derive(Serialize, Deserialize)]
#[derive(Debug)]
pub struct StructDecl {
	pub loc:     usize,
	pub name:    Ident,
	pub fields:  Vec<Field>,
	pub methods: Vec<FuncDecl>,
}

impl PartialEq for StructDecl {
	fn eq(&self, other: &StructDecl) -> bool {
		return self.name == other.name &&
		self.fields == other.fields &&
		self.methods == other.methods;
	}
}

impl Eq for StructDecl {}

impl HasLocation for StructDecl {
	fn loc(&self) -> usize {
		return self.loc;
	}
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, Eq)]
pub struct VarDecl {
	pub loc:  usize,
	pub name: Ident,
	pub init: Box<Exp>
}

impl PartialEq for VarDecl {
	fn eq(&self, other: &VarDecl) -> bool {
		return self.name == other.name && self.init == other.init;
	}
}

impl HasLocation for VarDecl {
	fn loc(&self) -> usize {
		return self.loc;
	}
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq)]
pub enum Decl {
	Func(FuncDecl),
	Struct(StructDecl),
	Var(VarDecl),
}

impl HasLocation for Decl {
	fn loc(&self) -> usize {
		match self {
			Decl::Func(fd)   => return fd.loc(),
			Decl::Struct(sd) => return sd.loc(),
			Decl::Var(vd)    => return vd.loc(),
		}
	}
}

impl Decl {
	pub fn new_func(fd: FuncDecl) -> Box<Self> {
		return Box::new(Self::Func(fd));
	}

	pub fn new_struct(sd: StructDecl) -> Box<Self> {
		return Box::new(Self::Struct(sd));
	}

	pub fn new_var(vd: VarDecl) -> Box<Self> {
		return Box::new(Self::Var(vd));
	}
}

/* -------------------------------------------------------------------------------------
Statements.

	Stmt: BlockStmt | IfStmt | WhileStmt | ForStmt | ExpStmt | AssignStmt | ReturnStmt | LetStmt
	BlockStmt:  '{' Stmt* '}'
	IfStmt:     'if' Exp BlockStmt ('else' (BlockStmt | IfStmt))?
	WhileStmt:  'while' Exp BlockStmt
	ForStmt:    'for' Id 'in' Exp ',' Exp BlockStmt
	ExpStmt:    Exp ';'
	AssignStmt: Exp '=' Exp ';'
	ReturnStmt: 'return' Exp? ';'
	LetStmt:    VarDecl
------------------------------------------------------------------------------------- */

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
	Block  (Vec<Box<Stmt>>),
	If     { cond:  Box<Exp>, then: Box<Stmt>, else_: Option<Box<Stmt>> },
	While  { cond:  Box<Exp>, code: Box<Stmt> },
	For    { var:   VarDecl, hi: Box<Exp>, code: Box<Stmt> },
	Exp    (Box<Exp>),
	Assign { dst:   Box<Exp>, src: Box<Exp> },
	Return (Option<Box<Exp>>),
	Let    (VarDecl),
}

#[derive(Serialize, Deserialize)]
#[derive(Debug)]
pub struct Stmt {
	pub id:   NodeId,
	pub loc:  usize,
	pub kind: StmtKind,
}

impl PartialEq for Stmt {
	fn eq(&self, other: &Self) -> bool {
		return self.kind == other.kind;
	}
}

impl Eq for Stmt {}

impl HasLocation for Stmt {
	fn loc(&self) -> usize {
		return self.loc;
	}
}

impl Stmt {
	pub fn new(loc: usize, kind: StmtKind) -> Box<Self> {
		return Box::new(Self { id: NodeId::new(), loc, kind });
	}

	// whether or not this statement is (or ends with) a return statement.
	// parser guarantees that return statements can only be last ones in blocks.
	pub fn returns(&self) -> bool {
		use StmtKind::*;

		return match &self.kind {
			Return(..) => true,
			Block(stmts) => stmts.last().map_or(false, |s| s.returns()),
			While { code, .. } | For { code, .. } => code.returns(),

			// if statement "returns" if BOTH sides return.
			// if there is no else clause, then it counts as "not returning".
			If { then, else_, .. } =>
				then.returns() &&
				else_.as_ref().map_or(false, |s| s.returns()),

			_ => false,
		};
	}
}

/* -------------------------------------------------------------------------------------
Expressions. Operator precedence and associativity are specified in parse::operator.

	Exp:       Term (BinOp Term)*
	Term:      UnOp* Primary Postfix*
	Primary:   Id | Num | StrLit | BoolLit | 'null' | New | Parens

	BinOp:     '+'|'-'|'*'|'/'|'%'|'<<'|'>>'|'<'|'>'|'<='|'>='|'=='|'!='|'and'|'or'
	UnOp:      '-'|'not'
	Postfix:   Call | Field
	Call:      '(' (Exp (',' Exp)*)? ')'
	Field:     '.' Id

	Id:        <Token::Id>
	Num:       <Token::Num>
	StrLit:    <Token::StrLit>
	BoolLit:   <Token::True> | <Token::False>
	New:       'new' Id '(' ')'
	Parens:    '(' Exp ')'
------------------------------------------------------------------------------------- */

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum UnOp { Neg, Not, }

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinOp { Add, Sub, Mul, Div, Mod,
Less, Greater, LessEq, GreaterEq, Eq, NotEq, And, Or }

#[derive(Serialize, Deserialize)]
#[derive(PartialEq, Eq)]
pub enum ExpKind {
	Id     (Ident),
	IntLit (i64),
	BoolLit(bool),
	StrLit (String),
	Null,
	Unary  { op: UnOp,  lhs: Box<Exp>, },
	Binary { op: BinOp, lhs: Box<Exp>, rhs: Box<Exp>, },
	Call   { callee: Box<Exp>, args: Vec<Box<Exp>> },
	Field  { obj: Box<Exp>, name: Ident, },
	New    (Box<AstType>),
	Parens (Box<Exp>),
}

#[derive(Serialize, Deserialize)]
#[derive(Debug)]
pub struct Exp {
	pub id:   NodeId,
	pub loc:  usize,
	pub kind: ExpKind,
}

impl PartialEq for Exp {
	fn eq(&self, other: &Self) -> bool {
		return self.kind == other.kind;
	}
}

impl Eq for Exp {}

impl HasLocation for Exp {
	fn loc(&self) -> usize {
		return self.loc;
	}
}

impl Exp {
	pub fn new(loc: usize, kind: ExpKind) -> Box<Self> {
		return Box::new(Self { id: NodeId::new(), loc, kind });
	}
}