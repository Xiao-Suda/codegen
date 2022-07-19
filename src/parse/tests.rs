
use std::fmt::Write;

use crate::*;
use crate::test_util::{ token::*, misc::*, exp, stmt, asttype };

// ------------------------------------------------------------------------------------
// Type tests
// ------------------------------------------------------------------------------------

#[test]
fn good_types() {
	use TokenKind::*;
	// ()
	test_ok_type(&[LParen, RParen],
		asttype::new_void());

	// bool
	test_ok_type(&[Bool],
		asttype::new_bool());

	// int
	test_ok_type(&[Int],
		asttype::new_int());

	// string
	test_ok_type(&[String],
		asttype::new_string());

	// S
	test_ok_type(&[id("S")],
		asttype::new_struct(ident("S")));

	// fn(): ()
	test_ok_type(&[Fn, LParen, RParen, Colon, LParen, RParen],
		asttype::new_func(
			vec![],
			asttype::new_void()));

	// fn(int, int): ()
	test_ok_type(&[Fn, LParen, Int, Comma, Int, RParen, Colon, LParen, RParen],
		asttype::new_func(
			vec![asttype::new_int(), asttype::new_int()],
			asttype::new_void()));

	// fn(): S
	test_ok_type(&[Fn, LParen, RParen, Colon, id("S")],
		asttype::new_func(
			vec![],
			asttype::new_struct(ident("S"))));
}

#[test]
fn bad_types() {
	use ParseErrorKind::*;
	use TokenKind::*;
	// ((
	test_err_type(&[LParen, LParen],
		ExpectedToken { exp: RParen, act: LParen }, 1);

	// fn fn
	test_err_type(&[Fn, Fn],
		ExpectedToken { exp: LParen, act: Fn }, 1);

	// fn(int,)
	test_err_type(&[Fn, LParen, Int, Comma, RParen],
		ExpectedType(RParen), 4);

	// fn():
	test_err_type(&[Fn, LParen, RParen, Colon],
		ExpectedType(Eof), 4);
}

// ------------------------------------------------------------------------------------
// Expression tests
// ------------------------------------------------------------------------------------

#[test]
fn good_primary_exps() {
	use TokenKind::*;
	// x
	test_ok_exp(&[id("x")],
		idexp("x"));

	// 123
	test_ok_exp(&[intlit(123)],
		exp::new_int_lit(123));

	// true
	test_ok_exp(&[True],
		exp::new_bool_lit(true));

	// false
	test_ok_exp(&[False],
		exp::new_bool_lit(false));

	// "hello"
	test_ok_exp(&[strlit("hello")],
		exp::new_str_lit("hello"));

	// "null"
	test_ok_exp(&[Null],
		exp::new_null());

	// (x)
	test_ok_exp(&[LParen, id("x"), RParen],
		exp::new_parens(idexp("x")));

	// ((x))
	test_ok_exp(&[LParen, LParen, id("x"), RParen, RParen],
		exp::new_parens(exp::new_parens(idexp("x"))));

	// new S()
	test_ok_exp(&[New, id("S"), LParen, RParen],
		exp::new_new(
			asttype::new_struct(ident("S"))));
}

#[test]
fn bad_primary_exps() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// (x
	test_err_exp(&[LParen, id("x")],
		UnexpectedEof, 2);

	// (x;
	test_err_exp(&[LParen, id("x"), Semi],
		ExpectedToken { exp: RParen, act: Semi }, 2);

	// new new
	test_err_exp(&[New, New],
		ExpectedName(New), 1);

	// new S{}
	test_err_exp(&[New, id("S"), LBrace, RBrace],
		ExpectedToken { exp: LParen, act: LBrace }, 2);

	// if
	test_err_exp(&[If],
		BadExpression(If), 0);
}

#[test]
fn good_unary_exps() {
	use TokenKind::*;
	// -x
	test_ok_exp(&[Minus, id("x")],
		exp::new_unary(UnOp::Neg, idexp("x")));

	// not x
	test_ok_exp(&[Not, id("x")],
		exp::new_unary(UnOp::Not, idexp("x")));

	// not - not x
	test_ok_exp(&[Not, Minus, Not, id("x")],
		exp::new_unary(UnOp::Not,
			exp::new_unary(UnOp::Neg,
				exp::new_unary(UnOp::Not, idexp("x")))));

	// -x + y
	// (this should give (-x) + y, not -(x + y))
	test_ok_exp(&[Minus, id("x"), Plus, id("y")],
		exp::new_binary(
			exp::new_unary(UnOp::Neg, idexp("x")),
			BinOp::Add,
			idexp("y")));
}

#[test]
fn bad_unary_exps() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// -;
	test_err_exp(&[Minus, Semi],
		BadExpression(Semi), 1);

	// not;
	test_err_exp(&[Not, Semi],
		BadExpression(Semi), 1);
}

#[test]
fn good_postfix_exps() {
	use TokenKind::*;
	// x.y
	test_ok_exp(&[id("x"), Dot, id("y")],
		exp::new_field(
			idexp("x"),
			ident("y")));

	// x.y.z
	test_ok_exp(&[id("x"), Dot, id("y"), Dot, id("z")],
		exp::new_field(
			exp::new_field(
				idexp("x"),
				ident("y")),
			ident("z")));

	// f()
	test_ok_exp(&[id("f"), LParen, RParen],
		call("f", vec![]));

	// f()()
	test_ok_exp(&[id("f"), LParen, RParen, LParen, RParen],
		exp::new_func_call(
			call("f", vec![]),
			vec![]));

	// f(1)
	test_ok_exp(&[id("f"), LParen, intlit(1), RParen],
		call("f", vec![exp::new_int_lit(1)]));

	// f(1, 2, 3)
	test_ok_exp(&[id("f"), LParen, intlit(1), Comma, intlit(2), Comma, intlit(3), RParen],
		call("f", vec![exp::new_int_lit(1), exp::new_int_lit(2), exp::new_int_lit(3)]));

	// obj.method()
	test_ok_exp(&[id("obj"), Dot, id("method"), LParen, RParen],
		exp::new_func_call(
			exp::new_field(
				idexp("obj"),
				ident("method")),
			vec![]));
}

#[test]
fn bad_postfix_exps() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// x.+
	test_err_exp(&[id("x"), Dot, Plus],
		ExpectedName(Plus), 2);

	// f{
	test_err_exp(&[id("f"), LBrace],
		ExtraTokens(LBrace), 1);

	// f(;
	test_err_exp(&[id("f"), LParen, Semi],
		BadExpression(Semi), 2);

	// f(1 2)
	test_err_exp(&[id("f"), LParen, intlit(1), intlit(2), RParen],
		ExpectedToken { exp: RParen, act: intlit(2) }, 3);

	// f(1,)
	test_err_exp(&[id("f"), LParen, intlit(1), Comma, RParen],
		BadExpression(RParen), 4);
}

#[test]
fn compound_exps() {
	use TokenKind::*;

	// x.y + z
	// if this is failing because your postfix parsing is expecting Eof, well, it
	// shouldn't be expecting Eof. See the instructions.
	test_ok_exp(&[id("x"), Dot, id("y"), Plus, id("z")],
		exp::new_binary(
			exp::new_field(
				idexp("x"),
				ident("y")),
			BinOp::Add,
			idexp("z")));

	// x + y * z
	test_ok_exp(&[id("x"), Plus, id("y"), Times, id("z")],
		exp::new_binary(
			idexp("x"),
			BinOp::Add,
			exp::new_binary(
				idexp("y"),
				BinOp::Mul,
				idexp("z"))));

	// (x + y) * z
	test_ok_exp(&[LParen, id("x"), Plus, id("y"), RParen, Times, id("z")],
		exp::new_binary(
			exp::new_parens(
				exp::new_binary(
					idexp("x"),
					BinOp::Add,
					idexp("y"))),
			BinOp::Mul,
			idexp("z")));

	// -f(x).y * z
	test_ok_exp(&[Minus, id("f"), LParen, id("x"), RParen, Dot, id("y"), Times, id("z")],
		exp::new_binary(
			exp::new_unary(
				UnOp::Neg,
				exp::new_field(
					call("f", vec![idexp("x")]),
					ident("y"))),
			BinOp::Mul,
			idexp("z")));
}

// ------------------------------------------------------------------------------------
// Statement tests
// ------------------------------------------------------------------------------------

#[test]
fn good_simple_stmts() {
	use TokenKind::*;

	// return;
	test_ok_stmt(&[Return, Semi],
		stmt::new_return(None));

	// return 0;
	test_ok_stmt(&[Return, intlit(0), Semi],
		stmt::new_return(Some(exp::new_int_lit(0))));

	// println("hi!");
	test_ok_stmt(&[id("println"), LParen, strlit("hi!"), RParen, Semi],
		call_stmt("println", vec![exp::new_str_lit("hi!")]));

	// x = 5;
	test_ok_stmt(&[id("x"), Assign, intlit(5), Semi],
		stmt::new_assign(
			idexp("x"),
			exp::new_int_lit(5)));

	// 5 = 5;
	// (yes, this is syntactically valid! but it won't be semantically valid.)
	test_ok_stmt(&[intlit(5), Assign, intlit(5), Semi],
		stmt::new_assign(
			exp::new_int_lit(5),
			exp::new_int_lit(5)));

	// 5 + 5;
	// (same here)
	test_ok_stmt(&[intlit(5), Plus, intlit(5), Semi],
		stmt::new_exp(
			exp::new_binary(
				exp::new_int_lit(5),
				BinOp::Add,
				exp::new_int_lit(5))));
}

#[test]
fn bad_simple_stmts() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// return if;
	test_err_stmt(&[Return, If, Semi],
		BadExpression(If), 1);

	// return <eof>
	test_err_stmt(&[Return],
		BadExpression(Eof), 1);

	// return 5 10;
	test_err_stmt(&[Return, intlit(5), intlit(10), Semi],
		ExpectedToken { exp: Semi, act: intlit(10) }, 2);

	// f() <eof>
	test_err_stmt(&[id("f"), LParen, RParen],
		UnexpectedEof, 3);

	// x = ;
	test_err_stmt(&[id("x"), Assign, Semi],
		BadExpression(Semi), 2);

	// x = 5 10;
	test_err_stmt(&[id("x"), Assign, intlit(5), intlit(10)],
		ExpectedToken { exp: Semi, act: intlit(10) }, 3);
}

#[test]
fn good_block_stmts() {
	use TokenKind::*;

	// {}
	test_ok_stmt(&[LBrace, RBrace],
		stmt::new_block(vec![]));

	// {{}}
	test_ok_stmt(&[LBrace, LBrace, RBrace, RBrace],
		stmt::new_block(vec![
			stmt::new_block(vec![]),
		]));

	// { f(); }
	test_ok_stmt(&[LBrace, id("f"), LParen, RParen, Semi, RBrace],
		stmt::new_block(vec![
			call_stmt("f", vec![]),
		]));

	// { f(); g(); h(); }
	test_ok_stmt(&[LBrace,
		id("f"), LParen, RParen, Semi,
		id("g"), LParen, RParen, Semi,
		id("h"), LParen, RParen, Semi, RBrace],
		stmt::new_block(vec![
			call_stmt("f", vec![]),
			call_stmt("g", vec![]),
			call_stmt("h", vec![]),
		]));

	// { return; }
	// (if your code made an empty {}, be sure to push the statement even if it's a return.)
	test_ok_stmt(&[LBrace, Return, Semi, RBrace],
		stmt::new_block(vec![
			stmt::new_return(None),
		]));

	// { f(); return; }
	test_ok_stmt(&[LBrace, id("f"), LParen, RParen, Semi, Return, Semi, RBrace],
		stmt::new_block(vec![
			call_stmt("f", vec![]),
			stmt::new_return(None),
		]));
}

#[test]
fn bad_block_stmts() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// { <eof>
	test_err_stmt(&[LBrace],
		UnexpectedEof, 1);

	// {}}
	test_err_stmt(&[LBrace, RBrace, RBrace],
		ExtraTokens(RBrace), 2);

	// { return; f(); }
	// (after a return, the only valid thing is a close brace.)
	test_err_stmt(&[LBrace, Return, Semi, id("f"), LParen, RParen, Semi, RBrace],
		ExpectedToken { exp: RBrace, act: id("f") }, 3);

	// { { return; } f(); }
	// (this applies recursively, too.)
	test_err_stmt(&[LBrace, LBrace, Return, Semi, RBrace, id("f"), LParen, RParen, Semi, RBrace],
		ExpectedToken { exp: RBrace, act: id("f") }, 5);
}

#[test]
fn good_ctrl_stmts() {
	use TokenKind::*;

	// while f() { g(); }
	test_ok_stmt(&[While, id("f"), LParen, RParen, LBrace, id("g"), LParen, RParen, Semi, RBrace],
		stmt::new_while(
			call("f", vec![]),
			stmt::new_block(vec![
				call_stmt("g", vec![]),
			])));

	// for i in 0, 10 { println(i); }
	test_ok_stmt(
		&[For, id("i"), In, intlit(0), Comma, intlit(10), LBrace,
			id("println"), LParen, id("i"), RParen, Semi,
		RBrace],
		stmt::new_for(
			VarDecl { loc: 0, name: ident("i"), init: exp::new_int_lit(0) },
			exp::new_int_lit(10),
			stmt::new_block(vec![
				call_stmt("println", vec![idexp("i")]),
			])));

	// if f() { g(); }
	test_ok_stmt(&[If, id("f"), LParen, RParen, LBrace, id("g"), LParen, RParen, Semi, RBrace],
		stmt::new_if(
			call("f", vec![]),
			stmt::new_block(vec![call_stmt("g", vec![])]),
			None));

	// { if f() { g(); } }
	// (don't check for Eof, check if the next token is an Else)
	test_ok_stmt(
		&[LBrace,
			If, id("f"), LParen, RParen, LBrace, id("g"), LParen, RParen, Semi, RBrace,
		RBrace],
		stmt::new_block(vec![
			stmt::new_if(
				call("f", vec![]),
				stmt::new_block(vec![call_stmt("g", vec![])]),
				None)
		]));

	// if f() { g(); } else { h(); }
	test_ok_stmt(
		&[If, id("f"), LParen, RParen, LBrace,
			id("g"), LParen, RParen, Semi,
		RBrace, Else, LBrace,
			id("h"), LParen, RParen, Semi,
		RBrace],

		stmt::new_if(
			call("f", vec![]),
			stmt::new_block(vec![call_stmt("g", vec![])]),
			Some(stmt::new_block(vec![call_stmt("h", vec![])]))));

	// if f() { g(); } else if h() { i(); }
	test_ok_stmt(
		&[If, id("f"), LParen, RParen, LBrace,
			id("g"), LParen, RParen, Semi,
		RBrace, Else, If, id("h"), LParen, RParen, LBrace,
			id("i"), LParen, RParen, Semi,
		RBrace],

		stmt::new_if(
			call("f", vec![]),
			stmt::new_block(vec![call_stmt("g", vec![])]),
			Some(stmt::new_if(
				call("h", vec![]),
				stmt::new_block(vec![call_stmt("i", vec![])]),
				None))));

	// if f() { g(); } else if h() { i(); } else { j(); }
	test_ok_stmt(
		&[If, id("f"), LParen, RParen, LBrace,
			id("g"), LParen, RParen, Semi,
		RBrace, Else, If, id("h"), LParen, RParen, LBrace,
			id("i"), LParen, RParen, Semi,
		RBrace, Else, LBrace,
			id("j"), LParen, RParen, Semi,
		RBrace],

		stmt::new_if(
			call("f", vec![]),
			stmt::new_block(vec![call_stmt("g", vec![])]),
			Some(stmt::new_if(
				call("h", vec![]),
				stmt::new_block(vec![call_stmt("i", vec![])]),
				Some(stmt::new_block(vec![call_stmt("j", vec![])])))))); // we're in paren hell
}

#[test]
fn bad_ctrl_stmts() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// while {}
	test_err_stmt(
		&[While, LBrace, RBrace],
		BadExpression(LBrace), 1);

	// while f() g();
	test_err_stmt(
		&[While, id("f"), LParen, RParen, id("g"), LParen, RParen, Semi],
		ExpectedToken { exp: LBrace, act: id("g") }, 4);

	// for i in 10 {}
	test_err_stmt(
		&[For, id("i"), In, intlit(10), LBrace, RBrace],
		ExpectedToken { exp: Comma, act: LBrace }, 4);

	// for i in 0, {}
	test_err_stmt(
		&[For, id("i"), In, intlit(0), Comma, LBrace, RBrace],
		BadExpression(LBrace), 5);

	// for i in 0, 10 g();
	test_err_stmt(
		&[For, id("i"), In, intlit(0), Comma, intlit(10), id("g"), LParen, RParen, Semi],
		ExpectedToken { exp: LBrace, act: id("g") }, 6);

	// if {}
	test_err_stmt(
		&[If, LBrace, RBrace],
		BadExpression(LBrace), 1);

	// if f() g();
	test_err_stmt(
		&[If, id("f"), LParen, RParen, id("g"), LParen, RParen, Semi],
		ExpectedToken { exp: LBrace, act: id("g") }, 4);

	// if f() { g(); } else <eof>
	test_err_stmt(
		&[If, id("f"), LParen, RParen, LBrace,
			id("g"), LParen, RParen, Semi,
		RBrace, Else],
		UnexpectedEof, 11);
}

#[test]
fn let_stmts() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// let x = 10;
	test_ok_stmt(&[Let, id("x"), Assign, intlit(10), Semi],
		stmt::new_let(vardecl("x", exp::new_int_lit(10))));

	// let x;
	test_err_stmt(&[Let, id("x"), Semi],
		ExpectedToken { exp: Assign, act: Semi }, 2);
}

// ------------------------------------------------------------------------------------
// Decl tests
// ------------------------------------------------------------------------------------

#[test]
fn var_decls() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// let x = 10;
	test_ok_decl(&[Let, id("x"), Assign, intlit(10), Semi],
		Decl::new_var(vardecl("x", exp::new_int_lit(10))));

	// let x;
	test_err_decl(&[Let, id("x"), Semi],
		ExpectedToken { exp: Assign, act: Semi }, 2);
}

#[test]
fn good_func_decls() {
	use TokenKind::*;

	// fn f() {}
	test_ok_decl(&[Fn, id("f"), LParen, RParen, LBrace, RBrace],
		Decl::new_func(funcdecl("f", None, vec![], asttype::new_void(),
			vec![])));

	// fn f(x: int) {}
	test_ok_decl(&[Fn, id("f"), LParen, id("x"), Colon, Int, RParen, LBrace, RBrace],
		Decl::new_func(
			funcdecl("f", None, vec![funcarg("x", asttype::new_int())], asttype::new_void(),
				vec![])));

	// fn f(x: int, y: bool) {}
	test_ok_decl(
		&[Fn, id("f"), LParen, id("x"), Colon, Int, Comma, id("y"), Colon, Bool, RParen, LBrace,
		RBrace],

		Decl::new_func(funcdecl("f", None,
			vec![funcarg("x", asttype::new_int()), funcarg("y", asttype::new_bool())],
			asttype::new_void(),
			vec![])));

	// fn f(): () {}
	test_ok_decl(&[Fn, id("f"), LParen, RParen, Colon, LParen, RParen, LBrace, RBrace],
		Decl::new_func(funcdecl("f", None, vec![], asttype::new_void(),
			vec![])));

	// fn f(): int { return 10; }
	test_ok_decl(
		&[Fn, id("f"), LParen, RParen, Colon, Int, LBrace,
			Return, intlit(10), Semi,
		RBrace],

		Decl::new_func(funcdecl("f", None, vec![], asttype::new_int(),
			vec![
				stmt::new_return(Some(exp::new_int_lit(10)))
			])));
}

#[test]
fn bad_func_decls() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// fn fn
	test_err_decl(&[Fn, Fn],
		ExpectedName(Fn), 1);

	// fn f {}
	test_err_decl(&[Fn, id("f"), LBrace, RBrace],
		ExpectedToken { exp: LParen, act: LBrace }, 2);

	// fn f(x)
	test_err_decl(&[Fn, id("f"), LParen, id("x"), RParen],
		ExpectedToken { exp: Colon, act: RParen }, 4);

	// fn f(x:)
	test_err_decl(&[Fn, id("f"), LParen, id("x"), Colon, RParen],
		ExpectedType(RParen), 5);

	// fn f(x: 7)
	test_err_decl(&[Fn, id("f"), LParen, id("x"), Colon, intlit(7), RParen],
		ExpectedType(intlit(7)), 5);

	// fn f(x: int,)
	test_err_decl(&[Fn, id("f"), LParen, id("x"), Colon, Int, Comma, RParen],
		ExpectedName(RParen), 7);

	// fn f(x: int, y: )
	test_err_decl(&[Fn, id("f"), LParen, id("x"), Colon, Int, Comma, id("y"), Colon, RParen],
		ExpectedType(RParen), 9);

	// fn f(): {}
	test_err_decl(&[Fn, id("f"), LParen, RParen, Colon, LBrace, RBrace],
		ExpectedType(LBrace), 5);

	// fn f(): 5 {}
	test_err_decl(&[Fn, id("f"), LParen, RParen, Colon, intlit(5), LBrace, RBrace],
		ExpectedType(intlit(5)), 5);

	// fn f(): int {}
	// DO NOT HARDCODE YOUR ERROR TO USE A LOCATION OF 0 FOR THIS.
	// use the location of the 'fn' token.
	test_err_decl(&[Fn, id("f"), LParen, RParen, Colon, Int, LBrace, RBrace],
		NoReturn("f".into()), 0);

	// fn f(): int { if x { return 5; } }
	// DO NOT HARDCODE YOUR ERROR TO USE A LOCATION OF 0 FOR THIS.
	// use the location of the 'fn' token.
	test_err_decl(
		&[Fn, id("f"), LParen, RParen, Colon, Int, LBrace,
			If, id("x"), LBrace, Return, intlit(5), Semi, RBrace,
		RBrace],
		NoReturn("f".into()), 0);
}

#[test]
fn good_struct_decls() {
	use TokenKind::*;

	// struct P { x: int }
	test_ok_decl(
		&[Struct, id("P"), LBrace,
			id("x"), Colon, Int,
		RBrace],
		structdecl("P", vec![field("x", asttype::new_int())], vec![]));

	// struct P { x: int, y: int }
	// (commas come between fields)
	test_ok_decl(
		&[Struct, id("P"), LBrace,
			id("x"), Colon, Int, Comma,
			id("y"), Colon, Int,
		RBrace],
		structdecl("P",
			vec![field("x", asttype::new_int()), field("y", asttype::new_int())],
			vec![]));

	// struct Cat { age: int fn meow() {} }
	// (func decls inside structs have a 'this' argument; it should not be in the arguments
	// vec but a separate thing.)
	test_ok_decl(
		&[Struct, id("Cat"), LBrace,
			id("age"), Colon, Int,
			Fn, id("meow"), LParen, RParen, LBrace, RBrace,
		RBrace],
		structdecl("Cat",
			vec![field("age", asttype::new_int())],
			vec![
				funcdecl("meow", Some("Cat"), vec![], asttype::new_void(), vec![])
			]));
}

#[test]
fn bad_struct_decls() {
	use ParseErrorKind::*;
	use TokenKind::*;

	// struct struct
	test_err_decl(&[Struct, Struct],
		ExpectedName(Struct), 1);

	// struct S: {}
	test_err_decl(&[Struct, id("S"), Colon, LBrace, RBrace],
		ExpectedToken { exp: LBrace, act: Colon }, 2);

	// struct S;
	test_err_decl(&[Struct, id("S"), Semi],
		ExpectedToken { exp: LBrace, act: Semi }, 2);

	// struct S {}
	test_err_decl(&[Struct, id("S"), LBrace, RBrace],
		ExpectedName(RBrace), 3);

	// struct S { x }
	test_err_decl(&[Struct, id("S"), LBrace, id("x"), RBrace],
		ExpectedToken { exp: Colon, act: RBrace }, 4);

	// struct S { x: }
	test_err_decl(&[Struct, id("S"), LBrace, id("x"), Colon, RBrace],
		ExpectedType(RBrace), 5);

	// struct S { x: 5 }
	test_err_decl(&[Struct, id("S"), LBrace, id("x"), Colon, intlit(5), RBrace],
		ExpectedType(intlit(5)), 5);

	// struct S { x: int fn f(){}, }
	test_err_decl(
		&[Struct, id("S"), LBrace,
			id("x"), Colon, Int,
			Fn, id("f"), LParen, RParen, LBrace, RBrace, Comma,
		RBrace],
		ExpectedToken { exp: RBrace, act: Comma }, 12);
}

#[test]
fn full_program() {
	use TokenKind::*;

	// struct P2 { x: int, y: int }
	// struct P3 { z: int }
	// let glob = 10;
	// fn main() {
	//     println("hello!");
	// }

	test_ok_prog(&[
		Struct, id("P2"), LBrace,
			id("x"), Colon, Int, Comma,
			id("y"), Colon, Int,
		RBrace,
		Struct, id("P3"), LBrace,
			id("z"), Colon, Int,
		RBrace,
		Let, id("glob"), Assign, intlit(10), Semi,
		Fn, id("main"), LParen, RParen, LBrace,
			id("println"), LParen, strlit("hello!"), RParen, Semi,
		RBrace],

		Program { items: vec![
			structdecl("P2",
				vec![field("x", asttype::new_int()), field("y", asttype::new_int())],
				vec![]),
			structdecl("P3",
				vec![field("z", asttype::new_int())],
				vec![]),
			Decl::new_var(vardecl("glob", exp::new_int_lit(10))),
			Decl::new_func(funcdecl("main", None, vec![], asttype::new_void(), vec![
				call_stmt("println", vec![exp::new_str_lit("hello!")])
			]))
		]});
}

// ------------------------------------------------------------------------------------
// AST helpers
// ------------------------------------------------------------------------------------

pub fn funcdecl(name: &str, this: Option<&str>, args: Vec<FuncArg>, return_type: Box<AstType>,
code: Vec<Box<Stmt>>) -> FuncDecl {
	return FuncDecl {
		loc: 0,
		name: ident(name),
		this: this.map(|ty_name|
			funcarg("this", asttype::new_struct(ident(ty_name)))),
		args,
		return_type,
		code: stmt::new_block(code),
	};
}

pub fn structdecl(name: &str, fields: Vec<Field>, methods: Vec<FuncDecl>)
-> Box<Decl> {
	return Decl::new_struct(StructDecl {
		loc: 0,
		name: ident(name),
		fields,
		methods,
	});
}

// ------------------------------------------------------------------------------------
// Test harness functions
// ------------------------------------------------------------------------------------

// there are more "Rustic" ways of doing this but it's a test harness so idc.
#[track_caller]
fn test_ok_type(input: &[TokenKind], expected: Box<AstType>) {
	test_ok_impl(input, expected, Parser::parse_one_type(&synthesize_tokens(input)));
}

#[track_caller]
fn test_ok_exp(input: &[TokenKind], expected: Box<Exp>) {
	test_ok_impl(input, expected, Parser::parse_one_exp(&synthesize_tokens(input)));
}

#[track_caller]
fn test_ok_stmt(input: &[TokenKind], expected: Box<Stmt>) {
	test_ok_impl(input, expected, Parser::parse_one_stmt(&synthesize_tokens(input)));
}

#[track_caller]
fn test_ok_decl(input: &[TokenKind], expected: Box<Decl>) {
	test_ok_impl(input, expected, Parser::parse_one_decl(&synthesize_tokens(input)));
}

#[track_caller]
fn test_ok_prog(input: &[TokenKind], expected: Program) {
	test_ok_impl(input, Box::new(expected),
		Parser::parse_program(&synthesize_tokens(input)).map(|p| Box::new(p)));
}

#[track_caller]
fn test_err_type(input: &[TokenKind], kind: ParseErrorKind, token_idx: usize) {
	test_err_impl(input, kind, token_idx, Parser::parse_one_type(&synthesize_tokens(input)));
}

#[track_caller]
fn test_err_exp(input: &[TokenKind], kind: ParseErrorKind, token_idx: usize) {
	test_err_impl(input, kind, token_idx, Parser::parse_one_exp(&synthesize_tokens(input)));
}

#[track_caller]
fn test_err_stmt(input: &[TokenKind], kind: ParseErrorKind, token_idx: usize) {
	test_err_impl(input, kind, token_idx, Parser::parse_one_stmt(&synthesize_tokens(input)));
}

#[track_caller]
fn test_err_decl(input: &[TokenKind], kind: ParseErrorKind, token_idx: usize) {
	test_err_impl(input, kind, token_idx, Parser::parse_one_decl(&synthesize_tokens(input)));
}

#[track_caller]
fn test_ok_impl<T>(input: &[TokenKind], expected: Box<T>, result: ParseResult<Box<T>>)
where
	T: Eq + std::fmt::Debug + std::fmt::Display
{
	match result {
		Ok(actual)  => {
			if expected != actual {
				panic!(concat!("\non this input: {:?}\n\n",
					"expected:\n{}\n\n",
					"but your code gave:\n{}\n"),
					input, expected, actual);
			}
		}

		Err(ParseError { token_idx, kind }) => {
			panic!("\non this input:\n{}\nbut it should have given:\n{}\n",
				show_error_in_tokens(input, token_idx, kind), expected);
		}
	}
}

#[track_caller]
fn test_err_impl<T>(input: &[TokenKind], exp_kind: ParseErrorKind, exp_token_idx: usize,
	result: ParseResult<Box<T>>)
where
	T: Eq + std::fmt::Debug + std::fmt::Display
{
	match result {
		Ok(actual) => {
			panic!(concat!("\non this input: {:?}\n\n",
				"expected error '{:?}' at token index {}\n\n",
				"but your code succeeded and gave:\n{}\n"),
				input, exp_kind, exp_token_idx, actual);
		}

		Err(ParseError { token_idx, kind }) => {
			use ParseErrorKind::*;

			// special-case for ExpectedToken: it's OK if the exp member differs, only act counts
			match (&kind, &exp_kind) {
				(ExpectedToken { act: a1, .. }, ExpectedToken { act: a2, .. }) => {
					if *a1 == *a2 && token_idx == exp_token_idx {
						return;
					}
				}

				_ => {}
			}

			if kind != exp_kind || token_idx != exp_token_idx {
				panic!(concat!("\non this input:\n{}\n",
					"but it should have given error '{:?}' at token index {}\n"),
					show_error_in_tokens(input, token_idx, kind), exp_kind, exp_token_idx);
			}
		}
	}
}

fn synthesize_tokens(input: &[TokenKind]) -> Vec<Token> {
	return input.iter().enumerate().map(|(i, kind)| {
		let span = i .. i + 1;
		Token { span, kind: kind.clone() }
	}).collect();
}

fn show_error_in_tokens(input: &[TokenKind], token_idx: usize, kind: ParseErrorKind) -> String {
	let mut ret = String::new();

	for (i, token) in input.iter().enumerate() {
		if i == token_idx {
			writeln!(&mut ret,
				"{}:  {:?}      <-- you gave an error here: {:?}", i, token, kind).unwrap();
		} else {
			writeln!(&mut ret, "{}:  {:?}", i, token).unwrap();
		}
	}

	// special case: error is at Eof location
	if token_idx == input.len() {
		writeln!(&mut ret,
			"{}:  {:?}      <-- you gave an error here: {:?}",
			token_idx, TokenKind::Eof, kind).unwrap();
	}

	return ret;
}