
use crate::*;
use crate::test_util::{ exp, stmt, asttype, misc::* };

// ------------------------------------------------------------------------------------
// Tests
// ------------------------------------------------------------------------------------

// TODO: these tests only indirectly test decls and uses. NameCtxChecker doesn't really
// have a way to directly test them. how would that work...?

#[test]
fn stdlib() {
	use SymbolKind::*;

	test_ok("// empty source code", prog(vec![]))
		.should_have("println_i", ExternFunc)
		.should_have("println_s", ExternFunc)
		.should_have("println_c", ExternFunc)
		.should_have("println_b", ExternFunc)
		.should_have("print_i",   ExternFunc)
		.should_have("print_s",   ExternFunc)
		.should_have("print_c",   ExternFunc)
		.should_have("print_b",   ExternFunc)
		.should_have("rand",      ExternFunc);
}

#[test]
fn global_vars() {
	use SymbolKind::*;

	// just some globals
	test_ok("let g = 0; let h = 1;", prog(vec![
		vardecl("g", intlit(0)),
		vardecl("h", intlit(1)),
	]))
		.should_have("g", GlobalVar)
		.should_have("h", GlobalVar);

	// testing redefinition errors
	test_err("let g = 0; let g = 1;", prog(vec![
		vardecl("g", intlit(0)),
		vardecl("g", intlit(1)),
	]), SemErrorKind::RedefinedName("g".into()));

	// the initializer is invalid, but that won't be checked until later;
	// just using it to check resolution (and a forward reference in this case).
	test_ok("let g = h; let h = 1;", prog(vec![
		vardecl("g", idexp("h")),
		vardecl("h", intlit(1)),
	]))
		.should_have("g", GlobalVar)
		.should_have("h", GlobalVar);

	// ditto
	test_err("let g = h;", prog(vec![
		vardecl("g", idexp("h")),
	]), SemErrorKind::UndefinedName("h".into()));
}

#[test]
fn funcs() {
	use SymbolKind::*;

	// just some functions
	test_ok("fn f() {} fn main() {}", prog(vec![
		fndecl("f", None, vec![], vec![]),
		fndecl("main", None, vec![], vec![]),
	]))
		.should_have("f", Function)
		.should_have("main", Function);

	// args
	test_ok("fn f(x: int, y: int) {}", prog(vec![
		fndecl("f", None, vec!["x", "y"], vec![]),
	]))
		.should_have("f", Function)
		.child_named("f")
			.shouldnt_have("this")
			.should_have("x", LocalVar)
			.should_have("y", LocalVar);

	// it's OK to have args the same name as globals; they shadow the globals
	test_ok("let g = 0; fn f(g: int) {}", prog(vec![
		vardecl("g", intlit(0)),
		fndecl("f", None, vec!["g"], vec![]),
	]))
		.should_have("g", GlobalVar)
		.should_have("f", Function)
		.child_named("f")
			.should_have("g", LocalVar);

	// can't have args of the same name
	test_err("fn f(x: int, x: int) {}", prog(vec![
		fndecl("f", None, vec!["x", "x"], vec![]),
	]), SemErrorKind::RedefinedName("x".into()));
}

#[test]
fn structs() {
	use SymbolKind::*;

	// just some structs
	test_ok("struct S {}  struct T {}", prog(vec![
		structdecl("S", vec![], vec![]),
		structdecl("T", vec![], vec![]),
	]))
		.should_have("S", Struct)
		.should_have("T", Struct);

	// again not a valid global initializer but it should pass name resolution
	test_ok("let g = new S(); struct S {}", prog(vec![
		vardecl("g", exp::new_new(asttype::new_struct(ident("S")))),
		structdecl("S", vec![], vec![]),
	]))
		.should_have("g", GlobalVar)
		.should_have("S", Struct);

	// fields
	test_ok("struct S { x: int, y: int }", prog(vec![
		structdecl("S", vec!["x", "y"], vec![]),
	]))
		.should_have("S", Struct)
		.child_named("S")
			.should_have("x", Field)
			.should_have("y", Field);

	// methods
	test_ok("struct S { fn f() {} }", prog(vec![
		structdecl("S", vec![], vec![
			fndecl("f", Some("S"), vec![], vec![]),
		]),
	]))
		.should_have("S", Struct)
		.child_named("S")
			.should_have("f", Function)
			.child_named("f")
				.should_have("this", LocalVar);
}

#[test]
fn stmts() {
	use SymbolKind::*;

	// blocks
	test_ok("fn main() { { let x = 1; } { let y = 1; } }", prog(vec![
		fndecl("main", None, vec![], vec![
			stmt::new_block(vec![
				letstmt("x", intlit(1)),
			]),
			stmt::new_block(vec![
				letstmt("y", intlit(1)),
			]),
		]),
	]))
		.should_have("main", Function)
		.child_named("main")
			.shouldnt_have("x")
			.shouldnt_have("y")
			.child(0)
				.should_have("x", LocalVar)
				.shouldnt_have("y")
			.parent()
			.child(1)
				.should_have("y", LocalVar)
				.shouldnt_have("x");

	// if-else
	test_ok("fn main() { let x = 1; if x { let y = 2; } else { let z = 3; } }", prog(vec![
		fndecl("main", None, vec![], vec![
			letstmt("x", intlit(1)),
			stmt::new_if(
				idexp("x"),
				stmt::new_block(vec![
					letstmt("y", intlit(2)),
				]),
				Some(stmt::new_block(vec![
					letstmt("z", intlit(3)),
				])),
			),
		]),
	]))
		.should_have("main", Function)
		.child_named("main")
			.should_have("x", LocalVar)
			.shouldnt_have("y")
			.shouldnt_have("z")
			.child(0)
				.should_have("x", LocalVar)
				.should_have("y", LocalVar)
				.shouldnt_have("z")
			.parent()
			.child(1)
				.should_have("x", LocalVar)
				.should_have("z", LocalVar)
				.shouldnt_have("y");

	// while
	test_ok("fn main() { let x = 1; while x { let y = 2; } }", prog(vec![
		fndecl("main", None, vec![], vec![
			letstmt("x", intlit(1)),
			stmt::new_while(
				idexp("x"),
				stmt::new_block(vec![
					letstmt("y", intlit(2)),
				]),
			),
		]),
	]))
		.should_have("main", Function)
		.child_named("main")
			.should_have("x", LocalVar)
			.shouldnt_have("y")
			.child(0)
				.should_have("x", LocalVar)
				.should_have("y", LocalVar);

	// for
	test_ok("fn main() { let x = 1; for i in 0, 10 { let y = 2; } }", prog(vec![
		fndecl("main", None, vec![], vec![
			letstmt("x", intlit(1)),
			stmt::new_for(
				VarDecl { loc: 0, name: ident("i"), init: intlit(0) },
				intlit(10),
				stmt::new_block(vec![
					letstmt("y", intlit(2)),
				]),
			),
		]),
	]))
		.should_have("main", Function)
		.child_named("main")
			.should_have("x", LocalVar)
			.shouldnt_have("i")
			.shouldnt_have("y")
			.child(0)
				.should_have("x", LocalVar)
				.should_have("i", LocalVar)
				.should_have("y", LocalVar);
}

// ------------------------------------------------------------------------------------
// Test harness functions
// ------------------------------------------------------------------------------------

#[track_caller]
fn test_ok(src: &'static str, ast: Program) -> NameCtxChecker {
	match Compiler::sem_symbols(&ast) {
		Ok(nc) => return NameCtxChecker::new(src, nc),
		Err(e) =>
			panic!("on '{}', symbol pass should have succeeded but failed with error: {}", src, e),
	}
}

#[track_caller]
fn test_err(src: &'static str, ast: Program, expected: SemErrorKind) {
	match SymbolsPass::run(&ast) {
		Ok(..) =>
			panic!("on '{}' should have failed with error '{}', but succeeded", src, expected),
		Err(actual) => {
			if expected != actual.kind {
				panic!(concat!("on '{}', should have failed with error:\n\t{}",
					"\nbut failed with this error instead:\n\t{}"),
					src, expected, actual.kind)
			}
		}
	}
}

fn intlit(val: i64) -> Box<Exp> {
	return exp::new_int_lit(val);
}

fn vardecl(name: &str, init: Box<Exp>) -> Box<Decl> {
	return Decl::new_var(VarDecl { loc: 0, name: ident(name), init });
}

fn fndecl(name: &str, this: Option<&str>, args: Vec<&str>, code: Vec<Box<Stmt>>) -> Box<Decl> {
	let this = this.map(|struct_name| {
		FuncArg {
			name: ident("this"),
			ty: asttype::new_struct(ident(struct_name)),
		}
	});

	let args = args.iter().map(|arg_name| FuncArg {
		name: ident(arg_name),
		ty: asttype::new_int(),
	}).collect();

	return Decl::new_func(FuncDecl {
		loc: 0,
		name: ident(name),
		this,
		args,
		return_type: asttype::new_void(),
		code: stmt::new_block(code),
	});
}

fn structdecl(name: &str, fields: Vec<&str>, methods: Vec<Box<Decl>>) -> Box<Decl> {
	let fields = fields.iter().map(|field_name| Field {
		name: ident(field_name),
		ty: asttype::new_int(),
	}).collect();

	let methods = methods.into_iter().map(|decl| {
		match *decl {
			Decl::Func(fd) => fd,
			_ => panic!(),
		}
	}).collect();

	return Decl::new_struct(StructDecl {
		loc: 0,
		name: ident(name),
		fields,
		methods
	});
}