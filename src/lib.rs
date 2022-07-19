#![allow(dead_code)]
#![allow(unused_imports)]

#![deny(unused_must_use)]      // makes it an error to forget ?
#![deny(unused_variables)]     // makes it an error to have unused variables
#![deny(unused_mut)]           // makes it an error to have mut variables that don't need to be mut
#![deny(unreachable_patterns)] // makes it an error to have matches with arms that can't run
#![allow(non_snake_case)]      // stop being so damn picky, rust
#![allow(unused_parens)]       // again

use serde::{ Serialize, Deserialize };

pub mod error;
pub mod lex;
pub mod parse;
pub mod sem;
pub mod cg;
#[cfg(test)]
pub mod test_util;

pub use error::*;
pub use lex::*;
pub use parse::*;
pub use sem::*;
pub use cg::*;

pub struct Compiler;

impl Compiler {
	pub fn lex(src: &Source) -> CompileResult<Vec<Token>> {
		return Ok(Lexer::lex(src)?);
	}

	pub fn lex_and_parse(src: &Source) -> CompileResult<(Vec<Token>, Program)> {
		let tokens = Self::lex(src)?;

		match Parser::parse_program(&tokens) {
			Ok(ast) => return Ok((tokens, ast)),
			Err(e)  => return Err(CompileError::from_parse_error(e, &tokens, src)),
		}
	}

	pub fn lex_and_parse_stmt(src: &Source) -> CompileResult<(Vec<Token>, Box<Stmt>)> {
		let tokens = Self::lex(src)?;

		match Parser::parse_one_stmt(&tokens) {
			Ok(ast) => return Ok((tokens, ast)),
			Err(e)  => return Err(CompileError::from_parse_error(e, &tokens, src)),
		}
	}

	pub fn lex_and_parse_exp(src: &Source) -> CompileResult<(Vec<Token>, Box<Exp>)> {
		let tokens = Self::lex(src)?;

		match Parser::parse_one_exp(&tokens) {
			Ok(ast) => return Ok((tokens, ast)),
			Err(e)  => return Err(CompileError::from_parse_error(e, &tokens, src)),
		}
	}

	pub fn sem_symbols(prog: &Program) -> SemResult<NameCtx> {
		return SymbolsPass::run(prog);
	}

	pub fn sem_symbols_stmt(stmt: &Box<Stmt>) -> SemResult<NameCtx> {
		return SymbolsPass::run_stmt(stmt);
	}

	pub fn sem_symbols_exp(exp: &Box<Exp>) -> SemResult<NameCtx> {
		return SymbolsPass::run_exp(exp);
	}

	pub fn sem_typecheck(prog: &Program, nc: &NameCtx) -> SemResult<TypeCtx> {
		return TypecheckPass::run(prog, nc);
	}

	pub fn sem_typecheck_stmt(stmt: &Box<Stmt>, nc: &NameCtx) -> SemResult<TypeCtx> {
		return TypecheckPass::run_stmt(stmt, nc);
	}

	pub fn sem_typecheck_exp(exp: &Box<Exp>, nc: &NameCtx) -> SemResult<TypeCtx> {
		return TypecheckPass::run_exp(exp, nc);
	}

	pub fn semantic(prog: &Program, tokens: &[Token], src: &Source)
	-> CompileResult<(NameCtx, TypeCtx)> {
		let nc = match Self::sem_symbols(prog) {
			Ok(nc) => nc,
			Err(e) => return Err(CompileError::from_sem_error(e, tokens, src)),
		};

		let tc = match Self::sem_typecheck(prog, &nc) {
			Ok(tc) => tc,
			Err(e) => return Err(CompileError::from_sem_error(e, tokens, src)),
		};

		return Ok((nc, tc));
	}

	pub fn semantic_stmt(stmt: &Box<Stmt>, tokens: &[Token], src: &Source)
	-> CompileResult<(NameCtx, TypeCtx)> {
		let nc = match Self::sem_symbols_stmt(stmt) {
			Ok(nc) => nc,
			Err(e) => return Err(CompileError::from_sem_error(e, tokens, src)),
		};

		let tc = match Self::sem_typecheck_stmt(stmt, &nc) {
			Ok(tc) => tc,
			Err(e) => return Err(CompileError::from_sem_error(e, tokens, src)),
		};

		return Ok((nc, tc));
	}

	pub fn semantic_exp(exp: &Box<Exp>, tokens: &[Token], src: &Source)
	-> CompileResult<(NameCtx, TypeCtx)> {
		let nc = match Self::sem_symbols_exp(exp) {
			Ok(nc) => nc,
			Err(e) => return Err(CompileError::from_sem_error(e, tokens, src)),
		};

		let tc = match Self::sem_typecheck_exp(exp, &nc) {
			Ok(tc) => tc,
			Err(e) => return Err(CompileError::from_sem_error(e, tokens, src)),
		};

		return Ok((nc, tc));
	}

	pub fn freeze(filename: &str, prog: Program, nc: NameCtx, tc: TypeCtx) {
		FrozenProgram { prog, nc, tc }.write_to_file(filename);
	}

	pub fn unfreeze(filename: &str) -> (Program, NameCtx, TypeCtx) {
		let prog = FrozenProgram::read_from_file(filename);
		return (prog.prog, prog.nc, prog.tc);
	}

	pub fn direct_codegen(prog: &Program, nc: &NameCtx, tc: &TypeCtx) -> (TextSeg, DataSeg) {
		return DirectCg::run(prog, nc, tc);
	}

	pub fn codegen_no_stdlib(prog: &Program, nc: &NameCtx, tc: &TypeCtx) -> (TextSeg, DataSeg) {
		return DirectCg::run_no_stdlib(prog, nc, tc);
	}

	pub fn codegen_stmt(stmt: &Box<Stmt>, nc: &NameCtx, tc: &TypeCtx) -> (TextSeg, DataSeg) {
		return DirectCg::run_stmt(stmt, nc, tc);
	}
	pub fn codegen_exp(exp: &Box<Exp>, nc: &NameCtx, tc: &TypeCtx) -> (TextSeg, DataSeg) {
		return DirectCg::run_exp(exp, nc, tc);
	}
}

#[derive(Serialize, Deserialize)]
pub struct FrozenProgram {
	prog: Program,
	nc:   NameCtx,
	tc:   TypeCtx,
}

impl FrozenProgram {
	pub fn write_to_file(self, filename: &str) {
		let serialized = ron::to_string(&self).unwrap();
		match std::fs::write(filename, serialized) {
			Ok(()) => {}
			Err(e) => {
				eprintln!("could not serialize to file '{}': {}", filename, e);
				std::process::exit(1);
			}
		}
	}

	pub fn read_from_file(filename: &str) -> Self {
		let serialized = match std::fs::read_to_string(filename) {
			Ok(s) => s,
			Err(e) => {
				eprintln!("could not read from file '{}': {}", filename, e);
				std::process::exit(1)
			}
		};

		match ron::from_str::<Self>(&serialized) {
			Ok(fp) => return fp,
			Err(e) => {
				eprintln!("error deserializing file '{}': {}", filename, e);
				std::process::exit(1)
			}
		}
	}
}