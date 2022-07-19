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

	println!("{}", "Note: you must have your lexer, parser, and typechecking working for this to work.".red());
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
						Ok((nc, tc)) => {
							let (text, data) = Compiler::codegen_no_stdlib(&prog, &nc, &tc);
							show_output(text, data);
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
						Ok((nc, tc)) => {
							let (text, data) = Compiler::codegen_stmt(&stmt, &nc, &tc);
							show_output(text, data);
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
						Ok((nc, tc)) => {
							let (text, data) = Compiler::codegen_exp(&exp, &nc, &tc);
							show_output(text, data);
						}

						Err(e) => println!("{} {}", "error:".red(), e),
					}
				}
				Err(e) => println!("{} {}", "error:".red(), e),
			}
		}
	}
}

fn show_output(text: TextSeg, data: DataSeg) {
	println!();

	// very simple/cheesy "syntax highlighting" to make the output a little more readable
	for line in data.to_string().lines() {
		if line.trim() == ".data" {
			println!("{}", line.red());
		} else {
			println!("{}", line);
		}
	}

	println!();

	for line in text.to_string().lines() {
		if line.trim() == ".text" {
			println!("{}", line.red());
		} else if line.trim_start().starts_with("#") {
			println!("{}", line.green());
		} else {
			println!("{}", line);
		}
	}

	println!("{}", "#-----------------------------------------------------------".green());
}