
use crate::*;

use crate::test_util::{ exp, stmt, asttype, misc::* };

// ------------------------------------------------------------------------------------
// Expression Tests

#[test]
fn exp_literals() {
	exp_ok("10", intlit(10),
		Type::new_int());

	exp_ok("\"hello\"", strlit("hello"),
		Type::new_string());

	exp_ok("true", boollit(true),
		Type::new_bool());

	exp_ok("false", boollit(false),
		Type::new_bool());
}

#[test]
fn exp_ident() {
	use SemErrorKind::*;

	// these just exist.
	exp_ok("var_int", id("var_int"),
		Type::new_int());

	exp_ok("var_str", id("var_str"),
		Type::new_string());

	exp_ok("var_bool", id("var_bool"),
		Type::new_bool());

	// this is just making sure the standard library functions are accessible.
	test_ok("// empty source code", prog(vec![]))
		.should_have("println_i", Type::new_func(vec![Type::new_int()],    Type::new_void()))
		.should_have("println_s", Type::new_func(vec![Type::new_string()], Type::new_void()))
		.should_have("println_c", Type::new_func(vec![Type::new_int()],    Type::new_void()))
		.should_have("println_b", Type::new_func(vec![Type::new_bool()],   Type::new_void()))
		.should_have("print_i",   Type::new_func(vec![Type::new_int()],    Type::new_void()))
		.should_have("print_s",   Type::new_func(vec![Type::new_string()], Type::new_void()))
		.should_have("print_c",   Type::new_func(vec![Type::new_int()],    Type::new_void()))
		.should_have("print_b",   Type::new_func(vec![Type::new_bool()],   Type::new_void()))
		.should_have("rand",      Type::new_func(vec![Type::new_int()],    Type::new_int()));

	// can't use a struct's name where an expression is expected.
	exp_err("Point", id("Point"),
		NotAValue("Point".into()));
}

#[test]
fn exp_field() {
	use SemErrorKind::*;

	// this should work and give the type of the field.
	exp_ok("var_point.x", field(id("var_point"), "x"),
		Type::new_int());

	exp_ok("var_point.y", field(id("var_point"), "y"),
		Type::new_int());

	// can't get methods as if they were fields.
	exp_err("var_point.set", field(id("var_point"), "set"),
		NoFieldNamed("set".into()));

	// can't get fields from non-structs.
	exp_mismatch("var_int.x", field(id("var_int"), "x"),
		"struct type", // expected type
		"int",         // what you gave
		"var_int");    // context

	// var_point is a struct, but it has no field z.
	exp_err("var_point.z", field(id("var_point"), "z"),
		NoFieldNamed("z".into()));
}

#[test]
fn exp_unary() {
	// negation
	exp_ok("-5", neg(intlit(5)),
		Type::new_int());

	exp_ok("--5", neg(neg(intlit(5))),
		Type::new_int());

	exp_ok("-var_int", neg(id("var_int")),
		Type::new_int());

	exp_mismatch("-true", neg(boollit(true)),
		"int",      // expected type
		"bool",     // what you gave
		"(-true)"); // context

	exp_mismatch("-var_bool", neg(id("var_bool")),
		"int",          // expected type
		"bool",         // what you gave
		"(-var_bool)"); // context

	// logical 'not'
	exp_ok("not true", not(boollit(true)),
		Type::new_bool());

	exp_ok("not not true", not(not(boollit(true))),
		Type::new_bool());

	exp_mismatch("not 5", not(intlit(5)),
		"bool",     // expected type
		"int",      // what you gave
		"(not 5)"); // context

	exp_mismatch("not var_int", not(id("var_int")),
		"bool",           // expected type
		"int",            // what you gave
		"(not var_int)"); // context
}

#[test]
fn exp_call() {
	use SemErrorKind::*;

	stmt_ok("println_i(10);", stmt::new_exp(call("println_i", vec![intlit(10)])));

	exp_ok("rand(10)", call("rand", vec![intlit(10)]),
		Type::new_int());

	exp_err("println_i()", call("println_i", vec![]),
		WrongNumArgs { exp: 1, act: 0 });

	exp_err("println_i(10, 20)", call("println_i", vec![intlit(10), intlit(20)]),
		WrongNumArgs { exp: 1, act: 2 });

	exp_mismatch("println_s(10)", call("println_s", vec![intlit(10)]),
		"string",      // expected type
		"int",         // what you gave
		"argument 1"); // context

	exp_err("rand()", call("rand", vec![]),
		WrongNumArgs { exp: 1, act: 0 });

	exp_err("rand(1, 2, 3)", call("rand", vec![intlit(1), intlit(2), intlit(3)]),
		WrongNumArgs { exp: 1, act: 3 });

	exp_mismatch("rand(true)", call("rand", vec![boollit(true)]),
		"int",         // expected type
		"bool",        // what you gave
		"argument 1"); // context

	exp_mismatch("var_int(10)", call("var_int", vec![intlit(10)]),
		"function type", // expected type
		"int",           // what you gave
		"var_int(10)");  // context
}

#[test]
fn exp_method_call() {
	use SemErrorKind::*;

	// valid method call
	stmt_ok("var_point.set(3, 4);",
		stmt::new_exp(method_call(field(id("var_point"), "set"), vec![intlit(3), intlit(4)])));

	// can't call fields as methods
	stmt_err("var_point.x();",
		stmt::new_exp(method_call(field(id("var_point"), "x"), vec![])),
		NoMethodNamed("x".into()));
}

#[test]
fn exp_new() {
	use SemErrorKind::*;

	// can't use `new` on something that isn't a struct.
	exp_err("new var_int()", new("var_int"),
		NotAType("var_int".into()));

	// normal case.
	exp_ok_with("new Point()", new("Point"),
		|tcc| tcc.get_struct_type("Point"));
}

#[test]
fn exp_equality() {
	// two of same type is okay.
	exp_ok("3 == 5", eq(intlit(3), intlit(5)),
		Type::new_bool());

	exp_ok("3 != 5", neq(intlit(3), intlit(5)),
		Type::new_bool());

	exp_ok("\"a\" == \"b\"", eq(strlit("a"), strlit("b")),
		Type::new_bool());

	exp_ok("\"a\" != \"b\"", neq(strlit("a"), strlit("b")),
		Type::new_bool());

	// even function types can be compared.
	exp_ok("println_s == println_s", eq(id("println_s"), id("println_s")),
		Type::new_bool());

	// but void is not okay to compare (like what println_i returns).
	exp_mismatch("println_i(5) == 5", eq(call("println_i", vec![intlit(5)]), intlit(5)),
		"non-void type", // expected type
		"()",            // what you gave
		"lhs of '=='");  // context
}

#[test]
fn exp_relational() {
	// can compare two ints, only
	exp_ok("3 < 5", lt(intlit(3), intlit(5)),
		Type::new_bool());

	exp_ok("3 <= 5", le(intlit(3), intlit(5)),
		Type::new_bool());

	exp_ok("3 > 5", gt(intlit(3), intlit(5)),
		Type::new_bool());

	exp_ok("3 >= 5", ge(intlit(3), intlit(5)),
		Type::new_bool());

	exp_mismatch("true < 5", lt(boollit(true), intlit(5)),
		"int",         // expected type
		"bool",        // what you gave
		"lhs of '<'"); // context

	exp_mismatch("3 < true", lt(intlit(3), boollit(true)),
		"int",         // expected type
		"bool",        // what you gave
		"rhs of '<'"); // context
}

#[test]
fn exp_logical() {
	// only booleans allowed!
	exp_ok("true and false", and(boollit(true), boollit(false)),
		Type::new_bool());

	exp_ok("true or false", or(boollit(true), boollit(false)),
		Type::new_bool());

	exp_ok("var_int == 3 or var_str == \"hi\"",
		or(eq(id("var_int"), intlit(3)), eq(id("var_str"), strlit("hi"))),
		Type::new_bool());

	exp_mismatch("3 or 4", or(intlit(3), intlit(4)),
		"bool",         // expected type
		"int",          // what you gave
		"lhs of 'or'"); // context

	exp_mismatch("true or 4", or(boollit(true), intlit(4)),
		"bool",         // expected type
		"int",          // what you gave
		"rhs of 'or'"); // context
}

#[test]
fn exp_arithmetic() {
	exp_ok("3 + 5", add(intlit(3), intlit(5)),
		Type::new_int());

	exp_ok("\"a\" + \"b\"", add(strlit("a"), strlit("b")),
		Type::new_string());

	exp_ok("3 - 5", sub(intlit(3), intlit(5)),
		Type::new_int());

	exp_ok("3 * 5", mul(intlit(3), intlit(5)),
		Type::new_int());

	exp_ok("3 * 5", div(intlit(3), intlit(5)),
		Type::new_int());

	exp_ok("3 % 5", mod_(intlit(3), intlit(5)),
		Type::new_int());

	exp_mismatch("3 + \"b\"", add(intlit(3), strlit("b")),
		"int",               // expected type
		"string",            // what you gave
		"rhs of '+'");       // context

	exp_mismatch("\"a\" + 5", add(strlit("a"), intlit(5)),
		"string",            // expected type
		"int",               // what you gave
		"rhs of '+'");       // context

	exp_mismatch("true + 5", add(boollit(true), intlit(5)),
		"'int' or 'string'", // expected type
		"bool",              // what you gave
		"lhs of '+'");       // context

	exp_mismatch("true - 5", sub(boollit(true), intlit(5)),
		"int",               // expected type
		"bool",              // what you gave
		"lhs of '-'");       // context

	exp_mismatch("true * 5", mul(boollit(true), intlit(5)),
		"int",               // expected type
		"bool",              // what you gave
		"lhs of '*'");       // context

	exp_mismatch("true / 5", div(boollit(true), intlit(5)),
		"int",               // expected type
		"bool",              // what you gave
		"lhs of '/'");       // context

	exp_mismatch("true % 5", mod_(boollit(true), intlit(5)),
		"int",               // expected type
		"bool",              // what you gave
		"lhs of '%'");       // context

	exp_mismatch("3 - true", sub(intlit(3), boollit(true)),
		"int",               // expected type
		"bool",              // what you gave
		"rhs of '-'");       // context

	exp_mismatch("3 * true", mul(intlit(3), boollit(true)),
		"int",               // expected type
		"bool",              // what you gave
		"rhs of '*'");       // context

	exp_mismatch("3 / true", div(intlit(3), boollit(true)),
		"int",               // expected type
		"bool",              // what you gave
		"rhs of '/'");       // context

	exp_mismatch("3 % true", mod_(intlit(3), boollit(true)),
		"int",               // expected type
		"bool",              // what you gave
		"rhs of '%'");       // context
}

// ------------------------------------------------------------------------------------
// Statement tests

#[test]
fn stmt_exp() {
	use SemErrorKind::*;

	// this appears in the exp_call test too, so if that works, this should work.
	stmt_ok("println_i(10);", stmt::new_exp(call("println_i", vec![intlit(10)])));

	// only void-returning functions can be used as expression statements.
	// stmt_err("
	stmt_err("rand(5);", stmt::new_exp(call("rand", vec![intlit(5)])),
		NonVoidExpr("rand(5)".into()));
}

#[test]
fn stmt_let() {
	use SemErrorKind::*;

	// normal variable declaration.
	stmt_ok("let x = 10;", letstmt("x", intlit(10)));

	// println_i returns void when called, and you can't have a variable of type void.
	stmt_err("let x = println_i(10);", letstmt("x", call("println_i", vec![intlit(10)])),
		BadVarType("x".into()));

	// You can't initialize a variable with null, because null could be any reference type.
	stmt_err("let x = null;", letstmt("x", exp::new_null()),
		NullVar("x".into()));
}

#[test]
fn stmt_block() {
	use SemErrorKind::*;

	// block statements are EZ
	stmt_ok("{}", stmt::new_block(vec![]));

	stmt_ok("{ println_i(10); }", stmt::new_block(vec![
		stmt::new_exp(call("println_i", vec![intlit(10)]))
	]));

	//                                  lol look at all these closing parens and brackets vvvvvv
	stmt_err("{ rand(5);} ", stmt::new_block(vec![stmt::new_exp(call("rand", vec![intlit(5)]))]),
		NonVoidExpr("rand(5)".into()));
}

#[test]
fn stmt_return() {
	// in the test AST, we're in a void-returning function.
	stmt_ok("return;", stmt::new_return(None));

	// returning a value from a void-returning function is not allowed.
	stmt_mismatch("return 5;", stmt::new_return(Some(intlit(5))),
		"()",             // expected type
		"int",            // what you gave
		"return value");  // context

	// it's okay to return a "value" of type () as long as the function returns (), too.
	// this is consistent with Rust's treatment of ().
	test_ok("fn test() { return println_i(10); }", prog(vec![
		fndecl("test", None, vec![], asttype::new_void(), vec![
			stmt::new_return(Some(call("println_i", vec![intlit(10)])))
		])]));

	// now let's test the other way: we SHOULD be able to return a value from this function
	test_ok("fn test(): int { return 5; }", prog(vec![
		fndecl("test", None, vec![], asttype::new_int(), vec![
			stmt::new_return(Some(intlit(5)))
		])]));

	// returning the wrong type should be no good
	test_mismatch("fn test(): int { return true; }", prog(vec![
		fndecl("test", None, vec![], asttype::new_int(), vec![
			stmt::new_return(Some(boollit(true)))
		])]),
		"int",           // expected type
		"bool",          // what you gave
		"return value"); // context

	// and returning nothing should also be an error
	test_mismatch("fn test(): int { return; }", prog(vec![
		fndecl("test", None, vec![], asttype::new_int(), vec![
			stmt::new_return(None)
		])]),
		"int",           // expected type
		"()",            // what you gave
		"return value"); // context
}

#[test]
fn stmt_assign() {
	use SemErrorKind::*;

	// some valid assignments
	stmt_ok("var_int = 10;",     stmt::new_assign(id("var_int"), intlit(10)));
	stmt_ok("var_str = \"hi\";", stmt::new_assign(id("var_str"), strlit("hi")));
	stmt_ok("var_bool = true;",  stmt::new_assign(id("var_bool"), boollit(true)));
	stmt_ok("var_point.x = 10;", stmt::new_assign(field(id("var_point"), "x"), intlit(10)));

	// type mismatches
	stmt_mismatch("var_int = true;", stmt::new_assign(id("var_int"), boollit(true)),
		"int",                // expected type
		"bool",               // what you gave
		"rhs of assignment"); // context

	stmt_mismatch("var_bool = 10;", stmt::new_assign(id("var_bool"), intlit(10)),
		"bool",               // expected type
		"int",                // what you gave
		"rhs of assignment"); // context

	// completely invalid lhs
	stmt_err("rand(5) = 10;", stmt::new_assign(call("rand", vec![intlit(5)]), intlit(10)),
		InvalidLhs("rand(5)".into()));
}

#[test]
fn stmt_while() {
	// booleans are fine
	stmt_ok("while true {}", stmt::new_while(boollit(true), stmt::new_block(vec![])));
	stmt_ok("while var_bool {}", stmt::new_while(id("var_bool"), stmt::new_block(vec![])));

	// anything else is not
	stmt_mismatch("while 10 {}", stmt::new_while(intlit(10), stmt::new_block(vec![])),
		"bool",               // expected type
		"int",                // what you gave
		"'while' condition"); // context

	stmt_mismatch("while var_int {}", stmt::new_while(id("var_int"), stmt::new_block(vec![])),
		"bool",               // expected type
		"int",                // what you gave
		"'while' condition"); // context

	// also make sure the function return type is propagated into the while
	stmt_ok("while true { return; }", stmt::new_while(boollit(true), stmt::new_block(vec![
		stmt::new_return(None)])));
}

#[test]
fn stmt_if() {
	// booleans are fine
	stmt_ok("if true {}", stmt::new_if(boollit(true), stmt::new_block(vec![]), None));
	stmt_ok("if var_bool {}", stmt::new_if(id("var_bool"), stmt::new_block(vec![]), None));

	// anything else is not
	stmt_mismatch("if 10 {}", stmt::new_if(intlit(10), stmt::new_block(vec![]), None),
		"bool",            // expected type
		"int",             // what you gave
		"'if' condition"); // context

	stmt_mismatch("if var_int {}", stmt::new_if(id("var_int"), stmt::new_block(vec![]), None),
		"bool",            // expected type
		"int",             // what you gave
		"'if' condition"); // context

	// also make sure the function return type is propagated into both sides
	stmt_ok("if true { return; } else { return; }",
		stmt::new_if(boollit(true),
			stmt::new_block(vec![stmt::new_return(None)]),
			Some(stmt::new_block(vec![stmt::new_return(None)]))));
}

#[test]
fn stmt_for() {
	// start with a good one
	stmt_ok("for i in 0, 10 {}", stmt::new_for(vardecl("i", intlit(0)), intlit(10),
		stmt::new_block(vec![])));

	// bad lower bound
	stmt_mismatch("for i in true, 10 {}", stmt::new_for(vardecl("i", boollit(true)), intlit(10),
		stmt::new_block(vec![])),
		"int",                     // expected type
		"bool",                    // what you gave
		"'for' loop lower bound"); // context

	// bad upper bound
	stmt_mismatch("for i in 0, true {}", stmt::new_for(vardecl("i", intlit(0)), boollit(true),
		stmt::new_block(vec![])),
		"int",                     // expected type
		"bool",                    // what you gave
		"'for' loop upper bound"); // context

	// also make sure the function return type is propagated into the loop
	stmt_ok("for i in 0, 10 { return; }", stmt::new_for(vardecl("i", intlit(0)), intlit(10),
		stmt::new_block(vec![stmt::new_return(None)])));
}

// ------------------------------------------------------------------------------------
// Test harness functions
// ------------------------------------------------------------------------------------

#[track_caller]
fn stmt_ok(src: &'static str, stmt: Box<Stmt>) {
	test_ok(src, stmt_ast(stmt));
}

#[track_caller]
fn stmt_err(src: &'static str, stmt: Box<Stmt>, expected: SemErrorKind) {
	test_err(src, stmt_ast(stmt), expected);
}

#[track_caller]
fn exp_ok(src: &'static str, exp: Box<Exp>, ty: Box<Type>) {
	test_ok(src, exp_ast(exp))
		.child_named("test")
		.should_have("_RESULT_", ty);
}

#[track_caller]
fn exp_ok_with(src: &'static str, exp: Box<Exp>, ty: impl Fn(&TypeCtxChecker) -> Box<Type>) {
	test_ok(src, exp_ast(exp))
		.child_named("test")
		.should_have_with("_RESULT_", ty);
}

#[track_caller]
fn exp_err(src: &'static str, exp: Box<Exp>, expected: SemErrorKind) {
	test_err(src, exp_ast(exp), expected);
}

#[track_caller]
fn exp_mismatch(src: &'static str, exp: Box<Exp>, expected: &str, act: &str, ctx: &str) {
	exp_err(src, exp, SemErrorKind::TypeMismatch {
		exp: expected.into(),
		act: act.into(),
		ctx: ctx.into(),
	})
}

#[track_caller]
fn stmt_mismatch(src: &'static str, stmt: Box<Stmt>, expected: &str, act: &str, ctx: &str) {
	stmt_err(src, stmt, SemErrorKind::TypeMismatch {
		exp: expected.into(),
		act: act.into(),
		ctx: ctx.into(),
	})
}

#[track_caller]
fn test_mismatch(src: &'static str, prog: Program, expected: &str, act: &str, ctx: &str) {
	test_err(src, prog, SemErrorKind::TypeMismatch {
		exp: expected.into(),
		act: act.into(),
		ctx: ctx.into(),
	})
}

#[track_caller]
fn test_ok(src: &'static str, ast: Program) -> TypeCtxChecker {
	let nc = Compiler::sem_symbols(&ast).expect("don't change the test cases, please");

	match Compiler::sem_typecheck(&ast, &nc) {
		Ok(tc) => return TypeCtxChecker::new(src, nc, tc),
		Err(e) =>
			panic!("on '{}', type pass should have succeeded but failed with error: {}", src, e),
	}
}

#[track_caller]
fn test_err(src: &'static str, ast: Program, expected: SemErrorKind) {
	let nc = Compiler::sem_symbols(&ast).expect("don't change the test cases, please");

	match TypecheckPass::run(&ast, &nc) {
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

fn exp_ast(exp: Box<Exp>) -> Program {
	return stmt_ast(letstmt("_RESULT_", exp));
}

fn stmt_ast(stmt: Box<Stmt>) -> Program {
	/*
	struct Point {
		x: int,
		y: int,

		fn set(x: int, y: int) {}
	}

	fn test(var_int: int, var_str: string, var_bool: bool, var_point: Point) {
		let _RESULT_ = <the expression being tested>;
	}
	*/

	return prog(vec![
		structdecl("Point",
			vec![
				("x", asttype::new_int()),
				("y", asttype::new_int()),
			],
			vec![
				fndecl("set", Some("Point"), vec![
					("x", asttype::new_int()),
					("y", asttype::new_int()),
				], asttype::new_void(), vec![
					// can't put anything in here cause it'd cause test failures
					// before they get to statements.
				])
			],
		),
		fndecl("test", None, vec![
			("var_int", asttype::new_int()),
			("var_str", asttype::new_string()),
			("var_bool", asttype::new_bool()),
			("var_point", asttype::new_struct(ident("Point"))),
		],
		asttype::new_void(),
		vec![
			stmt
		])
	]);
}

// ------------------------------------------------------------------------------------
// AST Helpers
// ------------------------------------------------------------------------------------

fn fndecl(name: &str, this: Option<&str>, args: Vec<(&str, Box<AstType>)>, ret: Box<AstType>,
code: Vec<Box<Stmt>>) -> Box<Decl> {
	let this = this.map(|struct_name| {
		FuncArg {
			name: ident("this"),
			ty: asttype::new_struct(ident(struct_name)),
		}
	});

	let args = args.into_iter().map(|(arg_name, ty)| FuncArg {
		name: ident(arg_name),
		ty,
	}).collect();

	return Decl::new_func(FuncDecl {
		loc: 0,
		name: ident(name),
		this,
		args,
		return_type: ret,
		code: stmt::new_block(code),
	});
}

fn structdecl(name: &str, fields: Vec<(&str, Box<AstType>)>, methods: Vec<Box<Decl>>) -> Box<Decl> {
	let fields = fields.into_iter().map(|(field_name, ty)| Field {
		name: ident(field_name),
		ty,
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

fn letstmt(name: &str, init: Box<Exp>) -> Box<Stmt> {
	return stmt::new_let(vardecl(name, init));
}

fn intlit(val: i64) -> Box<Exp> {
	return exp::new_int_lit(val);
}

fn strlit(val: &str) -> Box<Exp> {
	return exp::new_str_lit(val);
}

fn boollit(val: bool) -> Box<Exp> {
	return exp::new_bool_lit(val);
}

fn id(name: &str) -> Box<Exp> {
	return exp::new_id(name);
}

fn field(obj: Box<Exp>, name: &str) -> Box<Exp> {
	return exp::new_field(obj, ident(name));
}

fn call(callee: &str, args: Vec<Box<Exp>>) -> Box<Exp> {
	return exp::new_func_call(id(callee), args);
}

fn method_call(callee: Box<Exp>, args: Vec<Box<Exp>>) -> Box<Exp> {
	return exp::new_func_call(callee, args);
}

fn new(ty: &str) -> Box<Exp> {
	return exp::new_new(asttype::new_struct(ident(ty)));
}

fn neg(e: Box<Exp>) -> Box<Exp> {
	return exp::new_unary(UnOp::Neg, e);
}

fn not(e: Box<Exp>) -> Box<Exp> {
	return exp::new_unary(UnOp::Not, e);
}

fn eq(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::Eq, r);
}

fn neq(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::NotEq, r);
}

fn lt(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::Less, r);
}

fn le(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::LessEq, r);
}

fn gt(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::Greater, r);
}

fn ge(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::GreaterEq, r);
}

fn and(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::And, r);
}

fn or(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::Or, r);
}

fn add(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::Add, r);
}

fn sub(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::Sub, r);
}

fn mul(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::Mul, r);
}

fn div(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::Div, r);
}

fn mod_(l: Box<Exp>, r: Box<Exp>) -> Box<Exp> {
	return exp::new_binary(l, BinOp::Mod, r);
}