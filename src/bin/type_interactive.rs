use std::fmt::{ Display, Formatter, Result as FmtResult };
use colored::Colorize;
use rustyline::{ Editor, KeyEvent, KeyCode, Modifiers, Cmd, error::ReadlineError };

use codegen_proj::*;

#[derive(Clone, Copy)]
enum Mode {
	Decl,
	Stmt,
	Exp,
}

impl Display for Mode {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			Mode::Decl => write!(f, "decl"),
			Mode::Stmt => write!(f, "stmt"),
			Mode::Exp  => write!(f, "exp"),
		}
	}
}

fn main() {
	let mut rl = Editor::<()>::new();
	rl.bind_sequence(KeyEvent(KeyCode::Tab, Modifiers::NONE),   Cmd::Insert(1, "\t".into()));
	rl.bind_sequence(KeyEvent(KeyCode::Down, Modifiers::SHIFT), Cmd::Insert(1, "\n".into()));

	println!("{}",
		"--------------------------------------------------------------------------".bright_blue());
	println!("{}",
		"Type some code. Shift+Down inserts a newline; Enter submits; Ctrl+C quits.".bright_blue());
	println!("{}",
		"Change modes by entering the following:".bright_blue());
	println!("  {} for declaration mode", "@d".red());
	println!("  {} for statement mode", "@s".red());
	println!("  {} for expressions mode", "@e".red());

	println!("{}", "Note: you must have your lexer and parser working for this to work.".red());
	println!("{}",
		"--------------------------------------------------------------------------".bright_blue());

	let mut mode = Mode::Exp;

	loop {
		let line = rl.readline(&format!("{} > ", mode));
		match line {
			Ok(line) => {
				rl.add_history_entry(line.as_str());

				match line.as_str() {
					"@d" => {
						mode = Mode::Decl;
						println!("switching to {} mode", mode);
					}
					"@s" => {
						mode = Mode::Stmt;
						println!("switching to {} mode", mode);
					}
					"@e" => {
						mode = Mode::Exp;
						println!("switching to {} mode", mode);
					}
					line => {
						do_it(line, mode);
					}
				}
			},
			Err(ReadlineError::Interrupted) => break,
			Err(ReadlineError::Eof) => break,
			Err(err) => {
				println!("Error: {:?}", err);
				break
			}
		}
	}

	println!("byeeeeee!");
}

fn do_it(line: &str, mode: Mode) {
	let src = Source::from_str(line, "<stdin>");

	match mode {
		Mode::Decl => {
			match Compiler::lex_and_parse(&src) {
				Ok((tokens, prog)) => {
					match Compiler::semantic(&prog, &tokens, &src) {
						Ok((_nc, tc)) => {
							println!("{}", "<typechecking succeeded>".green());
							let mut v = TypeVisitor { tc: &tc };
							v.visit_program(&prog);
						}

						Err(e) => println!("{} {}", "error:".red(), e),
					}
				}
				Err(e) => println!("{} {}", "error:".red(), e),
			}
		}
		Mode::Stmt => {
			match Compiler::lex_and_parse_stmt(&src) {
				Ok((tokens, stmt)) => {
					match Compiler::semantic_stmt(&stmt, &tokens, &src) {
						Ok((_nc, tc)) => {
							println!("{}", "<typechecking succeeded>".green());
							let mut v = TypeVisitor { tc: &tc };
							v.visit_stmt(&stmt);
						}

						Err(e) => println!("{} {}", "error:".red(), e),
					}
				}
				Err(e) => println!("{} {}", "error:".red(), e),
			}
		}
		Mode::Exp => {
			match Compiler::lex_and_parse_exp(&src) {
				Ok((tokens, exp)) => {
					match Compiler::semantic_exp(&exp, &tokens, &src) {
						Ok((_nc, tc)) => {
							println!("{}", "<typechecking succeeded>".green());
							let mut v = TypeVisitor { tc: &tc };
							v.visit_exp(&exp);
						}

						Err(e) => println!("{} {}", "error:".red(), e),
					}
				}
				Err(e) => println!("{} {}", "error:".red(), e),
			}
		}
	}
}

// Expression visitor

struct TypeVisitor<'tc> {
	tc: &'tc TypeCtx,
}

impl<'tc> Visitor for TypeVisitor<'tc> {
	fn visit_exp(&mut self, exp: &Box<Exp>) {
		// visit children first..
		walk::exp(self, exp);

		// then show this exp's type.
		let ty = match self.tc.get_node_type(exp.id) {
			Some(ty) => ty,
			None => {
				println!("There is no type associated with this expression node: '{:?}'", exp);
				println!("This should never happen.");
				panic!();
			}
		};

		let exp_str = format!("{}", exp);
		let ty_str = format!("{}", ty);
		println!("type of '{}' = '{}'", exp_str.green(), ty_str.red());
	}
}