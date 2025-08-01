package main

/* minc_parse

   parsing actually means converting XML into the
   Abstract Syntax Tree (AST) defined in the respective
   language (see minc_ast.{ml,jl,go,rs}).
   XML is made by converting the original source (.c) file
   by parser/minc_to_xml.py.

            minc_to_xml.py
   .c file ----------------> xml file

            str_to_dom (minc_parse)
           --------------------------> xml (DOM) tree

            dom_to_ast_{type,expr,stmt,...}
            (minc_parse)
           ---------------------------------> AST

   str_to_dom : convert a string into an XML tree (called
   Document Object Model, or DOM), using an appropriate
   XML parsing library of your language.

   dom_to_ast_{type,expr,stmt,...} : convert an XML tree representing
   type, expression, statement, etc. into the respective AST.

   str_to_ast : convert a string representing a program
   into AST, by first calling str_to_dom and then dom_to_ast_program

   file_xml_to_ast : read an XML file (get a string) and
   convert it into AST. this is the function called from the main
   function

*/

import (
	"fmt"
	"log"
	"os"
	"strconv"

	"github.com/subchen/go-xmldom"
)

/* --- things related to string -> dom --- */

/*
string -> dom

	<foo>bar</foo> -> Element("foo", [Text("bar")])
*/
func str_to_dom(s string) *xmldom.Node {
	var node *xmldom.Node = xmldom.Must(xmldom.ParseXML(s)).Root
	return node
}

/* xml in file -> dom */
func file_xml_to_dom(file_xml string) *xmldom.Node {
	contentb, err := os.ReadFile(file_xml)
	if err != nil {
		log.Fatal(err)
	}
	return str_to_dom(string(contentb))
}

/* --- parser (XML DOM -> Abstract Syntax Tree) ---

   dom_to_ast_xxxx converts a dom tree supposedly
   representing xxxx into the corresponding abstract
   syntax tree (see minc_ast for its definition).

   e.g., dom_to_ast_expr converts a dom tree supposedly
   representing an expression into a data
   representing an expression.

*/

/* panic when the input XML DOM tree does not have the right structure */
func invalid_xml(elem *xmldom.Node) {
	panic(fmt.Sprintf("%s", elem.XML()))
}

// check if this is a node like <tag/>
func check_singleton(elem *xmldom.Node) {
	if len(elem.Children) != 0 {
		invalid_xml(elem)
	}
	if elem.Text != "" {
		invalid_xml(elem)
	}
}

// check if this is a node like <tag>text</tag> and returns text
func check_get_text(elem *xmldom.Node) string {
	if len(elem.Children) != 0 {
		invalid_xml(elem)
	}
	if elem.Text == "" {
		invalid_xml(elem)
	}
	return elem.Text
}

// check if this is a node like <tag>elem</tag> and return elem
func check_get_child_1(elem *xmldom.Node) *xmldom.Node {
	children := elem.Children
	if len(children) != 1 {
		invalid_xml(elem)
	}
	return children[0]
}

// check if this is a node like <tag><t0>c0</t0><t1>c1</t1></tag> and return c0, c1
func check_get_children_2(elem *xmldom.Node, t0 string, t1 string) (*xmldom.Node, *xmldom.Node) {
	children := elem.Children
	if len(children) != 2 {
		invalid_xml(elem)
	}
	if children[0].Name != t0 {
		invalid_xml(elem)
	}
	if children[1].Name != t1 {
		invalid_xml(elem)
	}
	return children[0], children[1]
}

// check if this is a node like <tag><t0>c0</t0><t1>c1</t1><t2>c2</t2></tag> and return c0, c1, c2
func check_get_children_3(elem *xmldom.Node, t0 string, t1 string, t2 string) (*xmldom.Node, *xmldom.Node, *xmldom.Node) {
	children := elem.Children
	if len(children) != 3 {
		invalid_xml(elem)
	}
	if children[0].Name != t0 {
		invalid_xml(elem)
	}
	if children[1].Name != t1 {
		invalid_xml(elem)
	}
	if children[2].Name != t2 {
		invalid_xml(elem)
	}
	return children[0], children[1], children[2]
}

// check if this is a node like <tag><t0>c0</t0><t1>c1</t1><t2>c2</t2></tag> and return c0, c1, c2
func check_get_children_4(elem *xmldom.Node, t0 string, t1 string, t2 string, t3 string) (*xmldom.Node, *xmldom.Node, *xmldom.Node, *xmldom.Node) {
	children := elem.Children
	if len(children) != 4 {
		invalid_xml(elem)
	}
	if children[0].Name != t0 {
		invalid_xml(elem)
	}
	if children[1].Name != t1 {
		invalid_xml(elem)
	}
	if children[2].Name != t2 {
		invalid_xml(elem)
	}
	if children[3].Name != t3 {
		invalid_xml(elem)
	}
	return children[0], children[1], children[2], children[3]
}

/*
dom tree for type expression -> ast for type expression

	(note: only possible type expression in minc is "long".
	the framework is designed so it is easy to extend it with
	a new type)
*/
func dom_to_ast_type(elem *xmldom.Node) TypeExpr {
	// <primitive_type>long</primitive_type>
	switch elem.Name {
	case "primitive_type":
		name := check_get_text(elem)
		return &TypePrimitive{name}
	case "struct_type":
		name := check_get_text(elem)
		return &TypeStruct{name}
	}
	invalid_xml(elem)
	return nil
}

func dom_to_ast_type_name(elem *xmldom.Node) *TypeName {
	switch elem.Name {
	case "type_name":
		// <type_name><type>...</type><abstract_decl>...</abstract_decl></type_name>
		if len(elem.Children) == 2 {
			type_elem, abstract_decl_elem := check_get_children_2(elem, "type", "abstract_decl")
			type_expr := dom_to_ast_type(check_get_child_1(type_elem))
			abstract_decl := check_get_text(check_get_child_1(check_get_child_1(abstract_decl_elem)))
			return &TypeName{type_expr, abstract_decl}
		} else if len(elem.Children) == 1 {
			type_elem := check_get_child_1(elem)
			type_expr := dom_to_ast_type(check_get_child_1(type_elem))
			return &TypeName{type_expr, ""}
		}
	}
	invalid_xml(elem)
	return nil
}

/* dom tree for expression -> ast for expression */
func dom_to_ast_expr(elem *xmldom.Node) Expr {
	switch elem.Name {
	case "int_literal":
		{ // <int_literal>123</int_literal>
			s := check_get_text(elem)
			val, err := strconv.ParseInt(s, 10, 64)
			if err != nil {
				invalid_xml(elem)
			}
			return &ExprIntLiteral{val}
		}
	case "float_literal":
		{ // <float_literal>1.2</float_literal>
			s := check_get_text(elem)
			val, err := strconv.ParseFloat(s, 32)
			if err != nil {
				invalid_xml(elem)
			}
			return &ExprFloatLiteral{float32(val)}
		}
	case "double_literal":
		{ // <double_literal>1.2</double_literal>
			s := check_get_text(elem)
			val, err := strconv.ParseFloat(s, 64)
			if err != nil {
				invalid_xml(elem)
			}
			return &ExprDoubleLiteral{val}
		}
	case "id_expr":
		{ // <id_expr>x</id_expr>
			name := check_get_text(elem)
			return &ExprId{name}
		}
	case "un_op":
		{ // <un_op><op>-</op><arg>expr</arg></un_op>
			op_elem, arg_elem := check_get_children_2(elem, "op", "arg")
			op := check_get_text(op_elem)
			arg := dom_to_ast_expr(check_get_child_1(arg_elem))
			return &ExprOp{op, []Expr{arg}}
		}
	case "bin_op":
		{ // <bin_op><op>+</op><left>expr</left><right>expr</right></bin_op>
			op_elem, left_elem, right_elem := check_get_children_3(elem, "op", "left", "right")
			op := check_get_text(op_elem)
			left := dom_to_ast_expr(check_get_child_1(left_elem))
			right := dom_to_ast_expr(check_get_child_1(right_elem))
			return &ExprOp{op, []Expr{left, right}}
		}
	case "ternary_expr":
		{ // <ternary_expr><condition>expr</condition><true>expr</true><false>expr</false></ternary_expr>
			condition_elem, true_elem, false_elem := check_get_children_3(elem, "cond", "true", "false")
			condition := dom_to_ast_expr(check_get_child_1(condition_elem))
			true_expr := dom_to_ast_expr(check_get_child_1(true_elem))
			false_expr := dom_to_ast_expr(check_get_child_1(false_elem))
			return &ExprOp{"?", []Expr{condition, true_expr, false_expr}}
		}
	case "array_access":
		{ // <array_access><arg>expr</arg><index>expr</index></array_access>
			arg_elem, index_elem := check_get_children_2(elem, "arg", "index")
			arg := dom_to_ast_expr(check_get_child_1(arg_elem))
			index := dom_to_ast_expr(check_get_child_1(index_elem))
			return &ExprArray{arg, index}
		}
	case "cast":
		{ // <cast><type>type</type><arg>expr</arg></cast>
			type_elem, arg_elem := check_get_children_2(elem, "type", "arg")
			type_name := dom_to_ast_type_name(check_get_child_1(type_elem))
			arg := dom_to_ast_expr(check_get_child_1(arg_elem))
			return &ExprCast{type_name, arg}
		}
	case "struct_access":
		{ // <struct_access><arg>expr</arg><field>x</field></struct_access>
			arg_elem, field_elem := check_get_children_2(elem, "arg", "field")
			arg := dom_to_ast_expr(check_get_child_1(arg_elem))
			field := check_get_text(field_elem)
			return &ExprStruct{arg, field}
		}
	case "pointer_access":
		{ // <pointer_access><arg>expr</arg><field>x</field></pointer_access>
			arg_elem, field_elem := check_get_children_2(elem, "arg", "field")
			arg := dom_to_ast_expr(check_get_child_1(arg_elem))
			field := check_get_text(field_elem)
			return &ExprPointer{arg, field}
		}
	case "post":
		{ // <post><op>++</op><arg>expr</arg></post>
			op_elem, arg_elem := check_get_children_2(elem, "op", "arg")
			op := check_get_text(op_elem)
			arg := dom_to_ast_expr(check_get_child_1(arg_elem))
			return &ExprPost{op, arg}
		}
	case "call":
		{ // <call><fun>expr</fun><args>expr expr expr ...</args></call>
			fun_elem, args_elem := check_get_children_2(elem, "fun", "args")
			fun := dom_to_ast_expr(check_get_child_1(fun_elem))
			args := map_array(dom_to_ast_expr, args_elem.Children)
			return &ExprCall{fun, args}
		}
	case "paren":
		{ // <paren>expr</paren>
			expr := dom_to_ast_expr(check_get_child_1(elem))
			return &ExprParen{expr}
		}
	case "ref":
		{ // <ref>expr</ref>
			expr := dom_to_ast_expr(check_get_child_1(elem))
			return &ExprRef{expr}
		}
	case "deref":
		{ // <deref>expr</deref>
			expr := dom_to_ast_expr(check_get_child_1(elem))
			return &ExprDeref{expr}
		}
	}
	invalid_xml(elem)
	return nil
}

/* dom tree for variable declaration -> ast for variable declaration */
func dom_to_ast_declaration(elem *xmldom.Node) *Declaration {
	// <decl><type>TYPE</type><decls>...</decls></decl>
	switch elem.Name {
	case "declaration":
		var_type_elem, decls_elem := check_get_children_2(elem, "type", "decls")
		var_type := dom_to_ast_type(check_get_child_1(var_type_elem))
		decls := map_array(dom_to_ast_init_declarator, decls_elem.Children)
		return &Declaration{decls, var_type}
	}
	invalid_xml(elem)
	return nil
}

func dom_to_ast_init_declarator(elem *xmldom.Node) *InitDeclarator {
	children := elem.Children
	if len(children) == 2 {
		decl_elem, init_elem := check_get_children_2(elem, "decl", "init")
		decl := dom_to_ast_declarator(check_get_child_1(decl_elem))
		init := dom_to_ast_expr(check_get_child_1(init_elem))
		return &InitDeclarator{decl, init}
	} else if len(children) == 1 {
		decl_elem := check_get_child_1(elem)
		decl := dom_to_ast_declarator(check_get_child_1(decl_elem))
		return &InitDeclarator{decl, nil}
	} else {
		invalid_xml(elem)
	}
	return nil
}

func dom_to_ast_declarator(elem *xmldom.Node) Declarator {
	switch elem.Name {
	case "id_decl":
		{
			return &DeclaratorIdentifier{check_get_text(elem)}
		}
	case "paren_decl":
		{
			return &DeclaratorParenthesis{dom_to_ast_declarator(check_get_child_1(elem))}
		}
	case "pointer_decl":
		{
			ptr_elem, decl_elem := check_get_children_2(elem, "ptr", "decl")
			ptr := check_get_text(ptr_elem)
			decl := dom_to_ast_declarator(check_get_child_1(decl_elem))
			return &DeclaratorPointer{ptr, decl}
		}
	case "array_decl":
		{
			if len(elem.Children) == 2 {
				size_elem, decl_elem := check_get_children_2(elem, "size", "decl")
				size := dom_to_ast_expr(check_get_child_1(size_elem))
				decl := dom_to_ast_declarator(check_get_child_1(decl_elem))
				return &DeclaratorArray{size, decl}
			} else if len(elem.Children) == 1 {
				decl_elem := check_get_child_1(elem)
				decl := dom_to_ast_declarator(check_get_child_1(decl_elem))
				return &DeclaratorArray{nil, decl}
			}
		}
	case "func_decl":
		{
			decl_elem, params_elem := check_get_children_2(elem, "decl", "params")
			decl := dom_to_ast_declarator(check_get_child_1(decl_elem))
			params := map_array(dom_to_ast_param, params_elem.Children)
			return &DeclaratorFunc{params, decl}
		}
	}
	invalid_xml(elem)
	return nil
}

func dom_to_ast_decl_or_stmt(elem *xmldom.Node) *DeclStmtUnion {
	if elem.Name == "declaration" {
		return &DeclStmtUnion{dom_to_ast_declaration(elem), nil}
	}
	return &DeclStmtUnion{nil, dom_to_ast_stmt(elem)}
}

/* dom tree for statement -> ast for statement */
func dom_to_ast_stmt(elem *xmldom.Node) Stmt {
	switch elem.Name {
	case "empty":
		{ // <empty></empty>
			check_singleton(elem)
			return &StmtEmpty{}
		}
	case "continue":
		{ // <continue></continue>
			check_singleton(elem)
			return &StmtContinue{}
		}
	case "break":
		{ // <break></break>
			check_singleton(elem)
			return &StmtBreak{}
		}
	case "return":
		{ // <return>expr</return>
			return_expr := dom_to_ast_expr(check_get_child_1(elem))
			return &StmtReturn{return_expr}
		}
	case "expr_stmt":
		{ // <expr_stmt>expr</expr_stmt>
			expr := dom_to_ast_expr(check_get_child_1(elem))
			return &StmtExpr{expr}
		}
	case "compound":
		{ // <compound><decl>...</decl><stmt>...</stmt><decl>...</decl></compound>
			decls_or_stmts := map_array(dom_to_ast_decl_or_stmt, elem.Children)
			return &StmtCompound{decls_or_stmts}
		}
	case "if":
		{ // <if><cond>expr</cond><then>stmt</then><else>stmt</else></if>
			children := elem.Children
			if len(children) == 3 {
				cond_elem, then_elem, else_elem := check_get_children_3(elem, "cond", "then", "else")
				cond := dom_to_ast_expr(check_get_child_1(cond_elem))
				then_stmt := dom_to_ast_stmt(check_get_child_1(then_elem))
				else_stmt := dom_to_ast_stmt(check_get_child_1(else_elem))
				return &StmtIf{cond, then_stmt, else_stmt}
			} else if len(children) == 2 {
				cond_elem, then_elem := check_get_children_2(elem, "cond", "then")
				cond := dom_to_ast_expr(check_get_child_1(cond_elem))
				then_stmt := dom_to_ast_stmt(check_get_child_1(then_elem))
				return &StmtIf{cond, then_stmt, nil}
			}
			invalid_xml(elem)
		}
	case "while":
		{ // <while><cond>expr</cond><body>stmt</body></while>
			cond_elem, body_elem := check_get_children_2(elem, "cond", "body")
			cond := dom_to_ast_expr(check_get_child_1(cond_elem))
			body := dom_to_ast_stmt(check_get_child_1(body_elem))
			return &StmtWhile{cond, body}
		}
	case "for":
		{ //<for><init>expr</init><cond>expr</cond><?step>expr</step><body>stmt</body></for>
			children := elem.Children
			if len(children) == 4 {
				init_elem, cond_elem, step_elem, body_elem := check_get_children_4(elem, "init", "cond", "step", "body")
				init := dom_to_ast_decl_or_stmt(check_get_child_1(init_elem))
				cond := dom_to_ast_stmt(check_get_child_1(cond_elem)).(*StmtExpr)
				step := dom_to_ast_expr(check_get_child_1(step_elem))
				body := dom_to_ast_stmt(check_get_child_1(body_elem))
				return &StmtFor{init, cond, step, body}
			} else if len(children) == 3 {
				init_elem, cond_elem, body_elem := check_get_children_3(elem, "init", "cond", "body")
				init := dom_to_ast_decl_or_stmt(check_get_child_1(init_elem))
				cond := dom_to_ast_stmt(check_get_child_1(cond_elem)).(*StmtExpr)
				body := dom_to_ast_stmt(check_get_child_1(body_elem))
				return &StmtFor{init, cond, nil, body}
			}
		}
	}
	invalid_xml(elem)
	return nil
}

/* dom tree for a function parameter -> ast for parameter */
func dom_to_ast_param(elem *xmldom.Node) *DeclarationParameter {
	// <param><type>type_expr</type><decl>...</decl></param>
	switch elem.Name {
	case "param":
		{
			param_type_elem, decl_elem := check_get_children_2(elem, "type", "decl")
			param_type := dom_to_ast_type(check_get_child_1(param_type_elem))
			decl := dom_to_ast_declarator(check_get_child_1(decl_elem))
			return &DeclarationParameter{param_type, decl}
		}
	}
	invalid_xml(elem)
	return nil
}

/*
dom tree for a toplevel definition -> ast for a toplevel definition

	(note: only possible toplevel definition in minc is a function definition)
*/
func dom_to_ast_def(elem *xmldom.Node) Def {
	switch elem.Name {
	case "fun_def":
		/* <fun_def>
		   <type>TYPE</type>
		   <decl>...</decl>
		   <body>...</body>
		  </fun_def> */
		type_elem, decl_elem, body_elem := check_get_children_3(elem, "type", "decl", "body")
		type_expr := dom_to_ast_type(check_get_child_1(type_elem))
		decl := dom_to_ast_declarator(check_get_child_1(decl_elem))
		body := dom_to_ast_stmt(check_get_child_1(body_elem))
		return &DefFun{type_expr, decl, body}
	}
	invalid_xml(elem)
	return nil
}

/* dom tree for a program -> ast for a program */
func dom_to_ast_program(elem *xmldom.Node) *Program {
	//* <program>DEF ...</program>
	switch elem.Name {
	case "program":
		defs := map_array(dom_to_ast_def, elem.Children)
		return &Program{defs}
	}
	invalid_xml(elem)
	return nil
}

/* XML string -> abstract syntax tree */
func str_to_ast(s string) *Program {
	dom := str_to_dom(s)
	if dom == nil {
		return nil
	}
	return dom_to_ast_program(dom)
}

/* XML file -> abstract syntax tree */
func file_xml_to_ast(file_xml string) *Program {
	contentb, err := os.ReadFile(file_xml)
	if err != nil {
		log.Fatal(err)
	}
	return str_to_ast(string(contentb))
}
