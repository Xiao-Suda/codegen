
use std::collections::{ HashMap, hash_map::Iter as HashIter };
use std::fmt::{ Display, Formatter, Result as FmtResult };
use serde::{ Serialize, Deserialize };

use crate::*;

// ------------------------------------------------------------------------------------------------
// Arena
// ------------------------------------------------------------------------------------------------

pub trait ArenaIndex: Copy {
	fn from_usize(i: usize) -> Self;
	fn to_usize(&self) -> usize;
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, Clone)]
pub struct Arena<T, Idx: ArenaIndex> {
	vals: Vec<T>,
	phantom: std::marker::PhantomData<Idx>,
}

impl<T, Idx: ArenaIndex> Arena<T, Idx> {
	pub fn new() -> Self {
		return Self {
			vals: vec![],
			phantom: Default::default(),
		};
	}

	pub fn insert(&mut self, val: T) -> Idx {
		let next = self.next_idx();
		self.vals.push(val);
		return next;
	}

	pub fn insert_with(&mut self, create: impl FnOnce(Idx) -> T) -> Idx {
		let next = self.next_idx();
		self.vals.push(create(next));
		return next;
	}

	pub fn get(&self, idx: Idx) -> &T {
		let i = idx.to_usize();
		return &self.vals[i];
	}

	pub fn get_mut(&mut self, idx: Idx) -> &mut T {
		let i = idx.to_usize();
		return &mut self.vals[i];
	}

	fn next_idx(&self) -> Idx {
		return <Idx as ArenaIndex>::from_usize(self.vals.len());
	}
}

// ------------------------------------------------------------------------------------------------
// Symbol, SymbolKind, SymbolId, and SymbolArena
// ------------------------------------------------------------------------------------------------

#[derive(Serialize, Deserialize)]
#[derive(Debug, Clone)]
pub struct Symbol {
	pub id:          SymbolId,
	pub loc:         usize,
	pub name:        String,
	pub kind:        SymbolKind,
	pub in_scope:    ScopeId,
	pub child_scope: Option<ScopeId>,
}

impl Symbol {
	pub fn is_variable(&self) -> bool {
		return matches!(self.kind, SymbolKind::LocalVar | SymbolKind::GlobalVar);
	}

	pub fn is_field(&self) -> bool {
		return matches!(self.kind, SymbolKind::Field);
	}

	pub fn is_function(&self) -> bool {
		return matches!(self.kind, SymbolKind::Function);
	}
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum SymbolKind {
	LocalVar,
	GlobalVar,
	Field,
	Struct,
	Function,
	ExternFunc,
	ExternVar,
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct SymbolId(usize);

impl ArenaIndex for SymbolId {
	fn from_usize(i: usize) -> Self { return Self(i); }
	fn to_usize(&self) -> usize { return self.0; }
}

type SymbolArena = Arena<Symbol, SymbolId>;

// ------------------------------------------------------------------------------------------------
// Scope, ScopeId, and ScopeArena
// ------------------------------------------------------------------------------------------------

#[derive(Serialize, Deserialize)]
#[derive(Debug)]
pub struct ScopeChild {
	name: String,
	id:   ScopeId,
}

#[derive(Serialize, Deserialize)]
#[derive(Debug)]
struct Scope {
	id:       ScopeId,
	name:     String,
	symbols:  HashMap<String, SymbolId>,
	children: Vec<ScopeChild>,
	parent:   Option<ScopeId>,
}

impl Scope {
	fn new(id: ScopeId, name: &str, parent: Option<ScopeId>) -> Self {
		return Self {
			id,
			name: name.into(),
			symbols: HashMap::new(),
			children: vec![],
			parent,
		};
	}

	fn get_symbol(&self, name: &str) -> Option<SymbolId> {
		return self.symbols.get(name).copied();
	}

	fn has_symbol(&self, name: &str) -> bool {
		return self.get_symbol(name).is_some();
	}

	fn add_symbol(&mut self, name: &str, id: SymbolId) {
		self.symbols.insert(name.into(), id);
	}
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct ScopeId(usize);

impl ArenaIndex for ScopeId {
	fn from_usize(i: usize) -> Self { return Self(i); }
	fn to_usize(&self) -> usize { return self.0; }
}

type ScopeArena = Arena<Scope, ScopeId>;

impl ScopeArena {
	// lookup `name` starting at scope `start` and traversing up the parent scopes.
	// returns Some(id) if the name is found; and None if not.
	fn lookup(&self, name: &str, start: ScopeId) -> Option<SymbolId> {
		let mut scope = Some(start);

		while let Some(scope_idx) = scope {
			let cur_scope = self.get(scope_idx);

			if let Some(sym_id) = cur_scope.get_symbol(name) {
				return Some(sym_id);
			}

			scope = cur_scope.parent;
		}

		return None;
	}
}

// ------------------------------------------------------------------------------------------------
// ScopeTree
// ------------------------------------------------------------------------------------------------

#[derive(Serialize, Deserialize)]
#[derive(Debug)]
struct ScopeTree {
	scopes:  ScopeArena,  // all scopes in the tree
	symbols: SymbolArena, // all symbol declarations in the tree
	globals: ScopeId,     // root of the scope tree
}

impl ScopeTree {
	fn get_globals(&self) -> ScopeId {
		return self.globals;
	}

	fn scope_name(&self, scope: ScopeId) -> &str {
		return &self.scopes.get(scope).name;
	}

	fn scope_symbols(&self, scope: ScopeId) -> &HashMap<String, SymbolId> {
		return &self.scopes.get(scope).symbols;
	}

	fn scope_children(&self, scope: ScopeId) -> &Vec<ScopeChild> {
		return &self.scopes.get(scope).children;
	}

	fn scope_child_named(&self, parent: ScopeId, name: &str) -> Option<ScopeId> {
		return self.scopes.get(parent).children.iter().find(|c| c.name == name).map(|c| c.id);
	}

	fn scope_parent(&self, scope: ScopeId) -> Option<ScopeId> {
		return self.scopes.get(scope).parent;
	}

	fn lookup(&self, scope: ScopeId, name: &str) -> Option<SymbolId> {
		return self.scopes.lookup(name, scope);
	}

	fn lookup_no_traverse(&self, scope: ScopeId, name: &str) -> Option<SymbolId> {
		return self.scopes.get(scope).get_symbol(name);
	}

	fn symbol_scope(&self, sym_id: SymbolId) -> Option<ScopeId> {
		return self.symbols.get(sym_id).child_scope;
	}

	fn symbol(&self, sym_id: SymbolId) -> &Symbol {
		return self.symbols.get(sym_id);
	}

	// --------------------------------------------------------------------------------------------
	// private methods

	fn new_scope(&mut self, name: &str, parent: ScopeId) -> ScopeId {
		let id = self.scopes.insert_with(|id| Scope::new(id, name, Some(parent)));

		let parent = self.scopes.get_mut(parent);

		if parent.children.iter().find(|c| c.name == name).is_some() {
			panic!("scope name {} has already been defined.", name);
		}

		parent.children.push(ScopeChild { name: name.into(), id });

		return id;
	}

	// tries to create and add a symbol into the given scope. returns None if there is already
	// a symbol with that name in the scope, or Some(id) if it succeeded.
	fn add_symbol(&mut self, scope: ScopeId, loc: usize, name: &str, kind: SymbolKind,
	child_scope: Option<ScopeId>) -> Option<SymbolId> {
		// check for redefinition
		if self.scopes.get(scope).has_symbol(name) {
			return None;
		}

		// make the symbol
		let sym_id = self.symbols.insert_with(|id| Symbol {
			loc,
			id,
			name: name.into(),
			kind,
			in_scope: scope,
			child_scope,
		});

		// insert it into the current scope
		self.scopes.get_mut(scope).add_symbol(name, sym_id);

		return Some(sym_id);
	}

	fn fmt_helper(&self, f: &mut Formatter, scope: ScopeId, depth: usize) -> FmtResult {
		let indent = "  ".repeat(depth);
		let scope = &self.scopes.get(scope);

		writeln!(f, "{}[ {} ] -----------------", indent, scope.name)?;

		for (name, sym_id) in scope.symbols.iter() {
			writeln!(f, "{}  {} ({:?})", indent, name, self.symbols.get(*sym_id).kind)?;
		}

		for child in &scope.children {
			writeln!(f, "")?;
			self.fmt_helper(f, child.id, depth + 1)?;
		}

		return Ok(());
	}
}

impl Display for ScopeTree {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.fmt_helper(f, self.globals, 0)
	}
}

// ------------------------------------------------------------------------------------------------
// Uses, Decls, NameCtx
// ------------------------------------------------------------------------------------------------

// Same underlying type, but different meanings.
type Uses  = HashMap<NodeId, SymbolId>;
type Decls = HashMap<NodeId, SymbolId>;

#[derive(Serialize, Deserialize)]
#[derive(Debug)]
pub struct NameCtx {
	scopetree: ScopeTree,
	decls:     Decls,
	uses:      Uses,
}

impl NameCtx {
	/// given an `Ident` AST node, get the symbol it declares, if any.
	pub fn sym_declared_by(&self, name: &Ident) -> Option<SymbolId> {
		return self.decls.get(&name.id).copied();
	}

	/// given an `Ident` AST node, get the symbol it uses, if any.
	pub fn sym_used_by(&self, name: &Ident) -> Option<SymbolId> {
		return self.uses.get(&name.id).copied();
	}

	/// iterator over all declarations in the program.
	pub fn all_decls(&self) -> HashIter<NodeId, SymbolId> {
		return self.decls.iter();
	}

	/// gets the ScopeId of the global scope.
	pub fn get_globals(&self) -> ScopeId {
		return self.scopetree.get_globals();
	}

	/// gets the name of a scope.
	pub fn scope_name(&self, scope: ScopeId) -> &str {
		return self.scopetree.scope_name(scope);
	}

	/// gets the symbols defined within a scope.
	pub fn scope_symbols(&self, scope: ScopeId) -> &HashMap<String, SymbolId> {
		return self.scopetree.scope_symbols(scope);
	}

	/// gets the child scopes of a scope.
	pub fn scope_children(&self, scope: ScopeId) -> &Vec<ScopeChild> {
		return self.scopetree.scope_children(scope);
	}

	/// gets the ScopeId of a child scope of `parent` whose name is `name`.
	/// returns None if there is no child scope of that name.
	/// this is O(n) so don't call it repeatedly if you can avoid it.
	pub fn scope_child_named(&self, parent: ScopeId, name: &str) -> Option<ScopeId> {
		return self.scopetree.scope_child_named(parent, name);
	}

	/// gets the ScopeId of the parent scope of `scope`, if it has one.
	/// (the global scope has no parent and will return None.)
	pub fn scope_parent(&self, scope: ScopeId) -> Option<ScopeId> {
		return self.scopetree.scope_parent(scope);
	}

	/// performs the name lookup algorithm, looking for `name` starting at `scope`.
	/// if found, returns the SymbolId that the name refers to; if not, returns None.
	pub fn lookup(&self, scope: ScopeId, name: &str) -> Option<SymbolId> {
		return self.scopetree.lookup(scope, name);
	}

	/// just gets a symbol named `name` from `scope`.
	/// if it exists, returns the SymbolId that the name refers to; if not, returns None.
	pub fn lookup_no_traverse(&self, scope: ScopeId, name: &str) -> Option<SymbolId> {
		return self.scopetree.lookup_no_traverse(scope, name);
	}

	/// gets the child scope of a symbol, if it has one.
	pub fn symbol_scope(&self, sym_id: SymbolId) -> Option<ScopeId> {
		return self.scopetree.symbol_scope(sym_id);
	}

	/// gets the Symbol object that a SymbolId refers to.
	pub fn symbol(&self, sym_id: SymbolId) -> &Symbol {
		return self.scopetree.symbol(sym_id);
	}
}

// ------------------------------------------------------------------------------------------------
// NameCtxBuilder
// ------------------------------------------------------------------------------------------------

pub struct NameCtxBuilder {
	scopetree: ScopeTree,
	decls:     Decls,
	uses:      Uses,
	cur_scope: ScopeId,
}

impl NameCtxBuilder {
	// -------------------------------------------------------------------------------------------
	// public methods

	pub fn new() -> Self {
		let mut scopes = ScopeArena::new();
		let globals = scopes.insert_with(|id| Scope::new(id, "globals", None));

		return Self {
			scopetree: ScopeTree {
				scopes,
				globals,
				symbols: Arena::new(),
			},
			decls:     Decls::new(),
			uses:      Uses::new(),
			cur_scope: globals,
		};
	}

	/// "finalizes" the building and returns a finished NameCtx. consumes self.
	pub fn finish(self) -> NameCtx {
		return NameCtx {
			scopetree: self.scopetree,
			decls:    self.decls,
			uses:     self.uses,
		};
	}

	/// creates a new scope inside the current scope of the given name, and enters it.
	/// this is used for "anonymous" scopes like those created for statements. if you want
	/// to enter the scope of a struct or function, use `enter_decl`.
	/// panics if there is already a scope of the given name in the current scope.
	pub fn enter_scope(&mut self, name: &str) {
		self.cur_scope = self.scopetree.new_scope(name, self.cur_scope);
	}

	/// enter the scope of a symbol that has already been added with `add_struct` or `add_func`.
	/// if used correctly, this cannot fail.
	/// panics if the symbol has not been added to this scope.
	pub fn enter_decl(&mut self, name: &Ident) {
		// first get the decl
		let sym_id = match self.decls.get(&name.id) {
			Some(id) => *id,
			None => panic!(concat!("there is no symbol declared by '{}'.",
				"did you forget to do phase one?"), name.name),
		};

		// then see if it was declared in the current scope
		let sym = self.scopetree.symbol(sym_id);
		if sym.in_scope != self.cur_scope {
			panic!("symbol '{}' was not declared in the scope '{}'.", sym.name,
				self.scopetree.scope_name(self.cur_scope));
		}

		// then enter its scope.
		self.cur_scope = sym.child_scope.expect("symbol has no child scope??");
	}

	/// exits the current scope. and moves to its parent.
	/// panics if you try to exit the global scope.
	pub fn leave_scope(&mut self) {
		let parent = self.scopetree.scope_parent(self.cur_scope).expect("scope stack underflow!");
		self.cur_scope = parent;
	}

	/// Returns `true` if currently at global scope.
	pub fn is_global(&self) -> bool {
		return self.cur_scope == self.scopetree.get_globals();
	}

	/// attempts to add a variable to the current scope. if the current scope is the globals,
	/// the variable will be marked as a global; otherwise, as a local.
	/// `decl_by` is the ID of the AST that declared it, or `None` if it is e.g. a built-in symbol.
	/// returns the newly-created variable's SymbolId.
	/// fails if there is already a symbol of the given name in the current scope.
	pub fn add_var(&mut self, loc: usize, id: &Ident) -> SemResult<SymbolId> {
		let kind = if self.is_global() { SymbolKind::GlobalVar } else { SymbolKind::LocalVar };
		return self.add_symbol(loc, &id.name, Some(id.id), kind, None);
	}

	/// like `add_var`, but adds a struct field.
	pub fn add_field(&mut self, loc: usize, id: &Ident) -> SemResult<SymbolId> {
		return self.add_symbol(loc, &id.name, Some(id.id), SymbolKind::Field, None);
	}

	/// like `add_var` but adds a structure.
	pub fn add_struct(&mut self, loc: usize, id: &Ident) -> SemResult<SymbolId> {
		return self.add_parent_symbol(loc, &id.name, id.id, SymbolKind::Struct);
	}

	/// like `add_var` but adds a function.
	pub fn add_func(&mut self, loc: usize, id: &Ident) -> SemResult<SymbolId> {
		return self.add_parent_symbol(loc, &id.name, id.id, SymbolKind::Function);
	}

	/// like `add_var`, but adds an external function.
	pub fn add_extern_func(&mut self, name: &str) -> SemResult<SymbolId> {
		// location is set to 0, but should never cause a redefinition error.
		return self.add_symbol(0, name, None, SymbolKind::ExternFunc, None);
	}

	/// like `add_var`, but adds an external variable.
	pub fn add_extern_var(&mut self, name: &str) -> SemResult<SymbolId> {
		// location is set to 0, but should never cause a redefinition error.
		return self.add_symbol(0, name, None, SymbolKind::ExternVar, None);
	}

	/// resolves the given identifier `user` with the name lookup algorithm.
	/// on success, adds a 'use' from the referring AST node `user` to the found symbol.
	/// on failure, gives a SemError::UndefinedName error.
	pub fn resolve(&mut self, user: &Ident) -> SemResult<()> {
		match self.scopetree.lookup(self.cur_scope, &user.name) {
			Some(sym_id) => {
				let res = self.uses.insert(user.id, sym_id);
				assert!(res.is_none(), "AST node {:?} is already referring to a symbol named {}",
					user.id, user.name);
				return Ok(());
			}

			None => {
				Err(SemError::undefined_name(user.loc(), &user.name))
			}
		}
	}

	// -------------------------------------------------------------------------------------------
	// private methods

	fn add_parent_symbol(&mut self, loc: usize, name: &str, decl_by: NodeId, kind: SymbolKind)
	-> SemResult<SymbolId> {
		// make the child scope first
		let child = self.scopetree.new_scope(name, self.cur_scope);

		// then create the symbol, with that scope as its child scope
		let sym_id = self.add_symbol(loc, name, Some(decl_by), kind, Some(child))?;

		return Ok(sym_id);
	}

	fn add_symbol(&mut self, loc: usize, name: &str, decl_by: Option<NodeId>, kind: SymbolKind,
	child_scope: Option<ScopeId>) -> SemResult<SymbolId> {
		// add it to the symbol table/current scope...
		let sym_id = match self.scopetree.add_symbol(self.cur_scope, loc, name, kind, child_scope) {
			Some(id) => id,
			None =>
				return Err(SemError::redefined_name(loc, name)),
		};

		// and add it to the decls map, if there's a node that declared it
		if let Some(decl_by) = decl_by {
			let res = self.decls.insert(decl_by, sym_id);
			assert!(res.is_none(), "attempting to add '{}' to the decls map a second time", name);
		}

		return Ok(sym_id);
	}
}

// ------------------------------------------------------------------------------------------------
// NameCtxChecker
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct NameCtxChecker {
	src:   &'static str,
	nc:    NameCtx,
	scope: ScopeId,
}

impl NameCtxChecker {
	pub fn new(src: &'static str, nc: NameCtx) -> Self {
		let scope = nc.get_globals();
		return Self { src, nc, scope };
	}

	pub fn dump(self) -> Self {
		println!("{}", self.nc.scopetree);
		return self;
	}

	pub(crate) fn get_src(&self) -> &'static str {
		return self.src;
	}

	pub(crate) fn lookup(&self, name: &str) -> Option<SymbolId> {
		return self.nc.lookup(self.scope, name);
	}

	pub(crate) fn get_struct_type(&self, name: &str) -> Box<Type> {
		let sym_id = self.nc.lookup(self.nc.get_globals(), name).unwrap();
		let sym = self.nc.symbol(sym_id);
		assert!(sym.kind == SymbolKind::Struct);
		return Type::new_struct(sym_id);
	}

	#[track_caller]
	pub fn child_named(self, name: &str) -> Self {
		if let Some(child) = self.nc.scope_child_named(self.scope, name) {
			return Self { scope: child, ..self };
		} else {
			panic!("no scope named '{}' in parent scope '{}', in test case:\n{}\n",
				name, self.nc.scope_name(self.scope), self.src);
		}
	}

	#[track_caller]
	pub fn child(self, i: usize) -> Self {
		let children = self.nc.scope_children(self.scope);
		if let Some(child) = children.get(i) {
			return Self { scope: child.id, ..self };
		} else {
			panic!(
				"tried to get child scope #{} in parent scope '{}', but it only has {} children, \
				in test case:\n{}\n", i, self.nc.scope_name(self.scope), children.len(), self.src);
		}
	}

	#[track_caller]
	pub fn parent(self) -> Self {
		if let Some(parent) = self.nc.scope_parent(self.scope) {
			return Self { scope: parent, ..self };
		} else {
			panic!("AAAAAA no parent scope? that's not good");
		}
	}

	#[track_caller]
	pub fn should_have(self, name: &str, expected: SymbolKind) -> Self {
		if let Some(sym_id) = self.lookup(name) {
			let sym = self.nc.symbol(sym_id);

			if sym.kind != expected {
				panic!("expected kind '{:?}', but your code gave kind '{:?}', in test case:\n{}\n",
					expected, sym.kind, self.src);
			}
		} else {
			panic!("no symbol named '{}' accessible from scope '{}', in test case:\n{}\n",
				name, self.nc.scope_name(self.scope), self.src);
		}

		return self;
	}

	#[track_caller]
	pub fn shouldnt_have(self, name: &str) -> Self {
		if let Some(..) = self.nc.lookup(self.scope, name) {
			panic!("symbol '{}' should NOT be accessible from scope '{}', in test case:\n{}\n",
				name, self.nc.scope_name(self.scope), self.src);
		}

		return self;
	}
}