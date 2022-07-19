
use std::fmt::{ Display, Formatter, Result as FmtResult };

use crate::*;

#[derive(Debug, PartialEq, Eq)]
pub enum SemErrorKind {
	RedefinedName(String),
	UndefinedName(String),
	NoFieldNamed(String),
	NoMethodNamed(String),
	NonConstantInit(String),
	BadArgType(String),
	BadFieldType(String),
	BadVarType(String),
	NullVar(String),
	TypeMismatch { exp: String, act: String, ctx: String },
	NonVoidExpr(String),
	WrongNumArgs { exp: usize, act: usize },
	InvalidLhs(String),
	NotAType(String),
	NotAValue(String),
}

impl Display for SemErrorKind {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use SemErrorKind::*;

		match self {
			RedefinedName(n)   => write!(f, "name '{}' has already been defined.", n),
			UndefinedName(n)   => write!(f, "name '{}' has not been defined.", n),
			NoFieldNamed(n)    => write!(f, "no field named '{}' in the given struct.", n),
			NoMethodNamed(n)   => write!(f, "no method named '{}' in the given struct.", n),
			NonConstantInit(n) => write!(f, "global '{}' must have a constant initializer.", n),
			BadArgType(n)      => write!(f, "argument '{}' cannot be of type void.", n),
			BadFieldType(n)    => write!(f, "field '{}' cannot be of type void.", n),
			BadVarType(n)      => write!(f, "variable '{}' cannot be of type void.", n),
			NullVar(n)         => write!(f, "variable '{}' cannot have a null initializer.", n),
			TypeMismatch { exp, act, ctx } =>
				write!(f, "expected type '{}' in '{}', not '{}'", exp, ctx, act),
			NonVoidExpr(e) => write!(f, "expression '{}' does not have void type.", e),
			WrongNumArgs { exp, act } =>
				write!(f, "function takes {} arguments but was given {}", exp, act),
			InvalidLhs(e) => write!(f, "cannot assign into LHS of '{}'.", e),
			NotAType(n)   => write!(f, "'{}' does not name a type.", n),
			NotAValue(n)  => write!(f, "'{}' cannot be used as a value.", n),
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct SemError {
	pub token_idx: usize,
	pub kind:      SemErrorKind,
}

impl SemError {
	pub fn redefined_name(token_idx: usize, name: &str) -> Self {
		return Self { token_idx, kind: SemErrorKind::RedefinedName(name.into()) };
	}

	pub fn undefined_name(token_idx: usize, name: &str) -> Self {
		return Self { token_idx, kind: SemErrorKind::UndefinedName(name.into()) };
	}

	pub fn no_field_named(field: &Ident) -> Self {
		return Self { token_idx: field.loc(), kind: SemErrorKind::NoFieldNamed(field.name.clone()) };
	}

	pub fn no_method_named(field: &Ident) -> Self {
		return Self { token_idx: field.loc(), kind: SemErrorKind::NoMethodNamed(field.name.clone()) };
	}

	pub fn non_constant_init(var: &Ident) -> Self {
		return Self { token_idx: var.loc(), kind: SemErrorKind::NonConstantInit(var.name.clone()) };
	}

	pub fn bad_arg_type(arg: &Ident) -> Self {
		return Self { token_idx: arg.loc(), kind: SemErrorKind::BadArgType(arg.name.clone()) };
	}

	pub fn bad_field_type(field: &Ident) -> Self {
		return Self { token_idx: field.loc(), kind: SemErrorKind::BadFieldType(field.name.clone()) };
	}

	pub fn bad_var_type(var: &Ident) -> Self {
		return Self { token_idx: var.loc(), kind: SemErrorKind::BadVarType(var.name.clone()) };
	}

	pub fn null_var(var: &Ident) -> Self {
		return Self { token_idx: var.loc(), kind: SemErrorKind::NullVar(var.name.clone()) };
	}

	pub fn type_mismatch(token_idx: usize, exp: &str, act: &str, ctx: &str) -> Self {
		return Self {
			token_idx,
			kind: SemErrorKind::TypeMismatch {
				exp: exp.into(),
				act: act.into(),
				ctx: ctx.into()
			}
		};
	}

	pub fn non_void_expr(exp: &Exp) -> Self {
		return Self { token_idx: exp.loc(), kind: SemErrorKind::NonVoidExpr(exp.to_string()) };
	}

	pub fn wrong_num_args(token_idx: usize, exp: usize, act: usize) -> Self {
		return Self { token_idx, kind: SemErrorKind::WrongNumArgs { exp, act } };
	}

	pub fn invalid_lhs(exp: &Exp) -> Self {
		return Self { token_idx: exp.loc(), kind: SemErrorKind::InvalidLhs(exp.to_string()) };
	}

	pub fn not_a_type(ident: &Ident) -> Self {
		return Self { token_idx: ident.loc(), kind: SemErrorKind::NotAType(ident.name.clone()) };
	}

	pub fn not_a_value(ident: &Ident) -> Self {
		return Self { token_idx: ident.loc(), kind: SemErrorKind::NotAValue(ident.name.clone()) };
	}
}

impl Display for SemError {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "(token #{}): {}", self.token_idx, self.kind)
	}
}

impl std::error::Error for SemError {}

pub type SemResult<T> = Result<T, SemError>;