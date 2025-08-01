package main

import "fmt"

/* Abstract Syntax Tree */

/* type expression:

   for now, we only have a primitive type (long),
   which is TypePrimitive{"long"} */

type TypeExpr interface {
	ast_to_str_type() string
}

type TypePrimitive struct {
	name string // type name (always "long", for now)
}

type TypeStruct struct {
	name string // struct name
}

type TypePointer struct {
	type_ TypeExpr
}

type TypeArray struct {
	type_ TypeExpr
	size  Expr
}

type DeclStmtUnion struct {
	decl *Declaration
	stmt Stmt
}

/* Declarator */
type Declarator interface {
	ast_to_str_declarator() string
}

type DeclaratorPointer struct {
	ptr  string
	decl Declarator
}

type DeclaratorArray struct {
	size Expr
	decl Declarator
}

type DeclaratorFunc struct {
	params []*DeclarationParameter
	decl   Declarator
}

type DeclaratorParenthesis struct {
	decl Declarator
}

type DeclaratorIdentifier struct {
	name string
}

func (decl *DeclaratorPointer) ast_to_str_declarator() string {
	return fmt.Sprintf("%s %s", decl.ptr, decl.decl.ast_to_str_declarator())
}

func (decl *DeclaratorArray) ast_to_str_declarator() string {
	if decl.size == nil {
		return fmt.Sprintf("%s[]", decl.decl.ast_to_str_declarator())
	} else {
		return fmt.Sprintf("%s[%s]", decl.decl.ast_to_str_declarator(), decl.size.ast_to_str_expr())
	}
}

func (decl *DeclaratorFunc) ast_to_str_declarator() string {
	params := concat(", ", map_array(func(p *DeclarationParameter) string {
		return fmt.Sprintf("%s %s", p.specifier.ast_to_str_type(), p.decl.ast_to_str_declarator())
	}, decl.params))
	return fmt.Sprintf("%s(%s)", decl.decl.ast_to_str_declarator(), params)
}

func (decl *DeclaratorParenthesis) ast_to_str_declarator() string {
	return fmt.Sprintf("(%s)", decl.decl.ast_to_str_declarator())
}

func (decl *DeclaratorIdentifier) ast_to_str_declarator() string {
	return decl.name
}

/*
variable declaration
*/
type Declaration struct {
	decls     []*InitDeclarator
	specifier TypeExpr
}

func (decl *Declaration) ast_to_str_decl() string {
	decls := concat(", ", map_array(func(d *InitDeclarator) string {
		if d.init == nil {
			return d.decl.ast_to_str_declarator()
		} else {
			return fmt.Sprintf("%s = %s", d.decl.ast_to_str_declarator(), d.init.ast_to_str_expr())
		}
	}, decl.decls))
	return fmt.Sprintf("%s %s;", decl.specifier.ast_to_str_type(), decls)
}

type InitDeclarator struct {
	decl Declarator
	init Expr
}

/*
Parameter declaration (used in function declarations)
*/
type DeclarationParameter struct {
	specifier TypeExpr
	decl      Declarator
}

/* expression */
type Expr interface {
	ast_to_str_expr() string
}

/* 1, 2, 3, ... */
type ExprIntLiteral struct{ val int64 }

/* 1.0f, 1.2f, ... */
type ExprFloatLiteral struct{ val float32 }

/* 1.0, 1.2, ... */
type ExprDoubleLiteral struct{ val float64 }

/* x, y, z, ... */
type ExprId struct{ name string }

/* -x, x - y, ... */
type ExprOp struct {
	op   string
	args []Expr
}

/* &x */
type ExprRef struct {
	arg Expr
}

/* *x */
type ExprDeref struct {
	arg Expr
}

/* (int) x */
type ExprCast struct {
	type_name *TypeName
	arg       Expr
}

/* x.y */
type ExprStruct struct {
	arg   Expr
	field string
}

/* x->y */
type ExprPointer struct {
	arg   Expr
	field string
}

/* x[y] */
type ExprArray struct {
	arg   Expr
	index Expr
}

/* x++ */
type ExprPost struct {
	op  string
	arg Expr
}

/* f(1, 2, 3) */
type ExprCall struct {
	fun  Expr
	args []Expr
}

/* (x + y) */
type ExprParen struct{ sub_expr Expr }

/* statement */
type Stmt interface {
	ast_to_str_stmt() string
}

/* ; */
type StmtEmpty struct{}

/* continue; */
type StmtContinue struct{}

/* break; */
type StmtBreak struct{}

/* return e; */
type StmtReturn struct{ expr Expr }

/* f(x); */
type StmtExpr struct{ expr Expr }

/* { int x; return x + 1; } */
type StmtCompound struct {
	decls_or_stmts []*DeclStmtUnion
}

func (d *DeclStmtUnion) ast_to_str_decl_or_stmt() string {
	if d.decl != nil {
		return d.decl.ast_to_str_decl()
	} else if d.stmt != nil {
		return d.stmt.ast_to_str_stmt()
	} else {
		panic("DeclStmtUnion is nil")
	}
}

/* if (expr) stmt [else stmt] */
type StmtIf struct {
	cond      Expr
	then_stmt Stmt
	else_stmt Stmt
}

/* while (expr) stmt */
type StmtWhile struct {
	cond Expr
	body Stmt
}

/* for (expr; expr; expr) stmt */
type StmtFor struct {
	init *DeclStmtUnion
	cond *StmtExpr
	step Expr
	body Stmt
}

/* toplevel definition */
type Def interface {
	ast_to_str_def() string
}

/*
function definition

	e.g., long f(long x, long y) { return x; }
*/
type DefFun struct {
	type_expr TypeExpr
	decl      Declarator
	body      Stmt
}

/* program is just a list of definitions */
type Program struct {
	defs []Def
}

/* convert ast back to C string

   ast_to_str_xxx converts an AST to a string
   valid as a C program.

   it will not be used anywhere in your compiler,
   but is given for illustrating how you walk
   the AST and what kind of C program each AST
   is actually meant to represent.
*/

// [f(a[0]), f(a[1]), ... ]
func map_array[S any, T any](f func(S) T, a []S) []T {
	b := make([]T, len(a))
	for i, v := range a {
		b[i] = f(v)
	}
	return b
}

// a[0] sep a[1] sep ... sep a[n-1]
func concat(sep string, a []string) string {
	n := len(a)
	if n == 0 {
		return ""
	}
	if n == 1 {
		return a[0]
	}
	l := concat(sep, a[:n/2])
	r := concat(sep, a[n/2:])
	return fmt.Sprintf("%s%s%s", l, sep, r)
}

/*
AST for type expression -> C string

	TypeExpr::Primitive{"long"} -> "long"
*/
func (type_expr *TypePrimitive) ast_to_str_type() string {
	return type_expr.name
}

func (type_expr *TypeStruct) ast_to_str_type() string {
	return fmt.Sprintf("struct %s", type_expr.name)
}

func (type_expr *TypePointer) ast_to_str_type() string {
	return fmt.Sprintf("*%s", type_expr.type_.ast_to_str_type())
}

func (type_expr *TypeArray) ast_to_str_type() string {
	if type_expr.size == nil {
		return fmt.Sprintf("%s[]", type_expr.type_.ast_to_str_type())
	} else {
		return fmt.Sprintf("%s[%s]", type_expr.type_.ast_to_str_type(), type_expr.size.ast_to_str_expr())
	}
}

/*
AST for function parameter -> C string

	Decl{TypePrimitive("long"), "x"} -> "long x"
*/
func (param *DeclarationParameter) ast_to_str_param() string {
	return fmt.Sprintf("%s %s", param.specifier.ast_to_str_type(), param.decl.ast_to_str_declarator())
}

/* AST for an expression -> C string */

/* ExprIntLiteral{123} -> "123" */
func (expr *ExprIntLiteral) ast_to_str_expr() string {
	return fmt.Sprintf("%d", expr.val)
}

/* ExprFloatLiteral{1.0f} -> "1.0f" */
func (expr *ExprFloatLiteral) ast_to_str_expr() string {
	return fmt.Sprintf("%f", expr.val)
}

/* ExprDoubleLiteral{1.0} -> "1.0" */
func (expr *ExprDoubleLiteral) ast_to_str_expr() string {
	return fmt.Sprintf("%f", expr.val)
}

/* ExprId("x") -> "x" */
func (expr *ExprId) ast_to_str_expr() string {
	return expr.name
}

/* ExprOp("+" [ExprId("x"); ExprIntLiteral("123")]) -> "x + 123" */
func (expr *ExprOp) ast_to_str_expr() string {
	return concat(fmt.Sprintf(" %s ", expr.op),
		map_array(func(e Expr) string { return e.ast_to_str_expr() }, expr.args))
}

func (expr *ExprRef) ast_to_str_expr() string {
	return fmt.Sprintf("&%s", expr.arg.ast_to_str_expr())
}

func (expr *ExprDeref) ast_to_str_expr() string {
	return fmt.Sprintf("*%s", expr.arg.ast_to_str_expr())
}

/* ExprCast(TypePrimitive("int"), ExprId("x")) -> "(int) x" */
func (expr *ExprCast) ast_to_str_expr() string {
	return fmt.Sprintf("(%s%s) %s", expr.type_name.type_expr.ast_to_str_type(), expr.type_name.ptr, expr.arg.ast_to_str_expr())
}

/* ExprStruct(ExprId("x"), "y") -> "x.y" */
func (expr *ExprStruct) ast_to_str_expr() string {
	return fmt.Sprintf("%s.%s", expr.arg.ast_to_str_expr(), expr.field)
}

/* ExprPointer(ExprId("x"), "y") -> "x->y" */
func (expr *ExprPointer) ast_to_str_expr() string {
	return fmt.Sprintf("%s->%s", expr.arg.ast_to_str_expr(), expr.field)
}

/* ExprArray(ExprId("x"), ExprIntLiteral("123")) -> "x[123]" */
func (expr *ExprArray) ast_to_str_expr() string {
	return fmt.Sprintf("%s[%s]", expr.arg.ast_to_str_expr(), expr.index.ast_to_str_expr())
}

/* ExprPost(ExprId("x"), "++") -> "x++" */
func (expr *ExprPost) ast_to_str_expr() string {
	return fmt.Sprintf("%s%s", expr.arg.ast_to_str_expr(), expr.op)
}

/* ExprCall(ExprId("f"), [ExprId("x"); ExprIntLiteral("123")]) -> "f(x, 123)" */
func (expr *ExprCall) ast_to_str_expr() string {
	fun := expr.fun.ast_to_str_expr()
	args := concat(", ", map_array(func(e Expr) string { return e.ast_to_str_expr() }, expr.args))
	return fmt.Sprintf("%s(%s)", fun, args)
}

/* ExprParen(ExprOp("+", [ExprId("x"); ExprIntLiteral("123")])) -> "(x + 123)" */
func (expr *ExprParen) ast_to_str_expr() string {
	sub_expr := expr.sub_expr.ast_to_str_expr()
	return fmt.Sprintf("(%s)", sub_expr)
}

/* AST for a statement -> C string */

func (stmt *StmtEmpty) ast_to_str_stmt() string {
	return ";"
}

func (stmt *StmtContinue) ast_to_str_stmt() string {
	return "continue;"
}

func (stmt *StmtBreak) ast_to_str_stmt() string {
	return "break;"
}

func (stmt *StmtReturn) ast_to_str_stmt() string {
	return fmt.Sprintf("return %s;", stmt.expr.ast_to_str_expr())
}

func (stmt *StmtExpr) ast_to_str_stmt() string {
	return fmt.Sprintf("%s;", stmt.expr.ast_to_str_expr())
}

func (stmt *StmtCompound) ast_to_str_stmt() string {
	decls_or_stmts := concat("\n", map_array(func(d *DeclStmtUnion) string { return d.ast_to_str_decl_or_stmt() }, stmt.decls_or_stmts))
	return fmt.Sprintf("{\n%s\n}", decls_or_stmts)
}

func (stmt *StmtIf) ast_to_str_stmt() string {
	cond := stmt.cond.ast_to_str_expr()
	then_stmt := stmt.then_stmt.ast_to_str_stmt()
	if stmt.else_stmt == nil {
		return fmt.Sprintf("if (%s) %s", cond, then_stmt)
	} else {
		else_stmt := stmt.else_stmt.ast_to_str_stmt()
		return fmt.Sprintf("if (%s) %s else %s", cond, then_stmt, else_stmt)
	}
}

func (stmt *StmtWhile) ast_to_str_stmt() string {
	cond := stmt.cond.ast_to_str_expr()
	body := stmt.body.ast_to_str_stmt()
	return fmt.Sprintf("while (%s) %s", cond, body)
}

func (stmt *StmtFor) ast_to_str_stmt() string {
	init := stmt.init.ast_to_str_decl_or_stmt()
	cond := stmt.cond.ast_to_str_stmt()
	step := stmt.step.ast_to_str_expr()
	body := stmt.body.ast_to_str_stmt()
	return fmt.Sprintf("for (%s; %s; %s) %s", init, cond, step, body)
}

/* AST for a definition -> C string

   DefFun{TypePrimitive("long"), DeclaratorFunc([], DeclaratorIdentifier("f")), StmtCompound([])} ->
       "long f(void) {}"
*/

func (def *DefFun) ast_to_str_def() string {
	specifier := def.type_expr.ast_to_str_type()
	decl := def.decl.ast_to_str_declarator()
	body := def.body.ast_to_str_stmt()
	return fmt.Sprintf("%s %s %s", specifier, decl, body)
}

func (prog *Program) ast_to_str_program() string {
	return concat("\n", map_array(func(def Def) string { return def.ast_to_str_def() }, prog.defs))
}

type TypeName struct {
	type_expr TypeExpr
	ptr       string
}
