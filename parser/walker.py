from contextlib import contextmanager
from tatsu.walkers import NodeWalker
from tatsu.model import Node
from tatsu.mixins.indent import IndentPrintMixin
import models_ as models
import xml.dom.minidom as minidom

class XmlPrintMixin():
    _root: minidom.Document
    _current: minidom.Element | minidom.Document
    
    def __init__(self, *args, **kwargs):
        self._root = minidom.getDOMImplementation().createDocument(None, None, None)
        self._current = self._root
    
    @contextmanager
    def node(self, tag_name: str, add_to_current: bool = True):
        elem = self._root.createElement(tag_name)
        if add_to_current:
            self._current.appendChild(elem)
        self._current, parent = elem, self._current
        yield elem
        self._current = parent
    
    def text(self, text: str|int|float):
        elem = self._root.createTextNode(str(text))
        self._current.appendChild(elem) # type: ignore
    
    def add_node(self, node: minidom.Element):
        if node.tagName == "_temp":
            for child in node.childNodes:
                self._current.appendChild(child) # type: ignore
            return
        self._current.appendChild(node)

class XmlGenerator(NodeWalker, XmlPrintMixin):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    
    def walk_object(self, node: Node, *args, **kwargs):
        return node
    
    def walk_Program(self, node: models.Program, *args, **kwargs):
        """Generates XML for the entire program.
        ```xml
        <program>
            <fun_def>...</fun_def>
            <fun_def>...</fun_def>
        </program>
        ```
        """
        with self.node("program"):
            for definition in node.definitions:
                self.walk(definition)
    
    def walk_FuncDefinition(self, node: models.FuncDefinition, *args, **kwargs):
        """Generates XML for a function definition.
        ```xml
        <fun_def>
            <type>...</type>
            <decl>...</decl> // Pointers, parameters, and name
            <body>...</body>
        </fun_def>
        ```
        """
        # Calculates the return type of the function by recursing into the child.
        # Type expressions are expanded from inwards: e.g. int * (x[]) = Array<Pointer<int>>
        with self.node("fun_def"):
            with self.node("type"):
                self.walk(node.type)
            with self.node("decl"):
                self.walk(node.decl)
            with self.node("body"):
                self.walk(node.body)
    
    def walk_ParameterDeclaration(self, node: models.ParameterDeclaration, *args, **kwargs):
        """Generates XML for a function parameter.
        ```xml
        <param>
            <type>...</type>
            <decl>...</decl>
        </param>
        ```
        """
        with self.node("param"):
            with self.node("type"):
                self.walk(node.specifier)
            with self.node("decl"):
                self.walk(node.declarator)
    
    def walk_PrimitiveType(self, node: models.PrimitiveType, *args, **kwargs):
        """Generates XML for a pirimitive type expression.
        ```xml
        <primitive_type>...</primitive_type>
        ```
        """
        with self.node("primitive_type"):
            self.text(node.type)
    
    def walk_StructOrUnionSpecifier(self, node: models.StructOrUnionSpecifier, *args, **kwargs):
        """Generates XML for a struct or union specifier.
        ```xml
        <struct_type>
            <name>...</name>
            <members>
                <struct_decl>...</struct_decl>
            </members>
        </struct_type>
        <union_type>...</union_type>
        ```
        """
        with self.node("struct_type" if node.type == "struct" else "union_type"):
            if node.name is not None:
                with self.node("name"):
                    self.walk(node.name)
            if node.members is not None:
                with self.node("members"):
                    self.walk(node.members) # type: ignore
    
    def walk_PointerDeclarator(self, node: models.PointerDeclarator, *args, **kwargs):
        """Generates XML for a pointer declarator.
        ```xml
        <pointer_decl>
            <ptr>...</ptr>
            <decl>...</decl>
        </pointer_decl>
        ```
        """
        with self.node("pointer_decl"):
            with self.node("ptr"):
                self.text(node.ptr.ptr)
            with self.node("decl"):
                self.walk(node.decl)
        
    def walk_ArrayDeclarator(self, node: models.ArrayDeclarator, *args, **kwargs):
        """Generates XML for an array declarator.
        ```xml
        <array_decl>
            <size>...</size>
            <decl>...</decl>
        </array_decl>
        ```
        """
        with self.node("array_decl"):
            if node.size is not None:
                with self.node("size"):
                    self.walk(node.size)
            with self.node("decl"):
                self.walk(node.decl)

    def walk_FuncDeclarator(self, node: models.FuncDeclarator, *args, **kwargs):
        """Generates XML for a function declarator.
        ```xml
        <func_decl>
            <decl>...</decl>
            <params>...</params>
        </func_decl>
        ```
        """
        with self.node("func_decl"):
            with self.node("decl"):
                self.walk(node.decl)
            with self.node("params"):
                self.walk(node.params) # type: ignore
    
    def walk_ParenthesisDeclarator(self, node: models.ParenthesisDeclarator, *args, **kwargs):
        """Generates XML for a parenthesis declarator.
        ```xml
        <paren_decl>
            <decl>...</decl>
        </paren_decl>
        ```
        """
        with self.node("paren_decl"):
            with self.node("decl"):
                self.walk(node.decl)
    
    def walk_IdentifierDeclarator(self, node: models.IdentifierDeclarator, *args, **kwargs):
        """Generates XML for an identifier declarator.
        ```xml
        <id_decl>...</id_decl>
        ```
        """
        with self.node("id_decl"):
            self.walk(node.name)
    
    def walk_StructDeclaration(self, node: models.StructDeclaration, *args, **kwargs):
        """Generates XML for a struct declaration.
        ```xml
        <struct_decl>
            <type>...</type>
            <decls>...</decls>
        </struct_decl>
        ```
        """
        with self.node("struct_decl"):
            with self.node("type"):
                self.walk(node.specifier)
            with self.node("decls"):
                self.walk(node.declarators) # type: ignore
        
        
    def walk_Declaration(self, node: models.Declaration, *args, **kwargs):
        """Generates XML for a variable declaration.
        ```xml
        <declaration>
            <type>...</type>
            <decls>
                <init_decl>...</init_decl>
            </decls>
        </declaration>
        ```
        """
        with self.node("declaration"):
            with self.node("type"):
                self.walk(node.specifier)
            with self.node("decls"):
                self.walk(node.decls) # type: ignore
    
    def walk_InitDeclarator(self, node: models.InitDeclarator, *args, **kwargs):
        """Generates XML for an initializer declaration.
        ```xml
        <init_decl>
            <decl>...</decl>
            <init>...</init>
        </init_decl>
        ```
        """
        with self.node("init_decl"):
            with self.node("decl"):
                self.walk(node.decl)
            if node.init is not None:
                with self.node("init"):
                    self.walk(node.init)
    
    def walk_BinaryExpr(self, node: models.BinaryExpr, *args, **kwargs):
        """Generates XML for a binary expression.
        ```xml
        <bin_op>
            <op>...</op>
            <left>...</left>
            <right>...</right>
        </bin_op>
        ```
        """
        with self.node("bin_op"):
            with self.node("op"):
                self.text(node.op)
            with self.node("left"):
                self.walk(node.args[0])
            with self.node("right"):
                self.walk(node.args[1])

    def walk_UnaryExpr(self, node: models.UnaryExpr, *args, **kwargs):
        """Generates XML for a unary expression.
        ```xml
        <un_op>
            <op>...</op>
            <arg>...</arg>
        </un_op>
        ```
        """
        with self.node("un_op"):
            with self.node("op"):
                self.text(node.op)
            with self.node("arg"):
                self.walk(node.expr)

    def walk_Reference(self, node: models.Reference, *args, **kwargs):
        """Generates XML for a reference.
        ```xml
        <ref>...</ref>
        ```
        """
        with self.node("ref"):
            self.walk(node.arg)
    
    def walk_Dereference(self, node: models.Dereference, *args, **kwargs):
        """Generates XML for a dereference.
        ```xml
        <deref>...</deref>
        ```
        """
        with self.node("deref"):
            self.walk(node.arg)

    def walk_CommaExpr(self, node: models.CommaExpr, *args, **kwargs):
        raise NotImplementedError("CommaExpr is not implemented in this walker.")

    def walk_CondExpr(self, node: models.CondExpr, *args, **kwargs):
        """Generates XML for a conditional expression.
        ```xml
        <ternary_expr>
            <cond>...</cond>
            <true>...</true>
            <false>...</false>
        </ternary_expr>
        ```
        """
        with self.node("ternary_expr"):
            with self.node("cond"):
                self.walk(node.check_expr)
            with self.node("true"):
                self.walk(node.true_expr)
            with self.node("false"):
                self.walk(node.false_expr)

    def walk_CastExpr(self, node: models.CastExpr, *args, **kwargs):
        """Generates XML for a cast expression.
        ```xml
        <cast>
            <type>...</type>
            <arg>...</arg>
        </cast>
        ```
        """
        with self.node("cast"):
            with self.node("type"):
                self.walk(node.type)
            with self.node("arg"):
                self.walk(node.arg)
    
    def walk_TypeName(self, node: models.TypeName, *args, **kwargs):
        """Generates XML for a type name.
        ```xml
        <type_name>
            <type>...</type>
            <abstract_decl>...</abstract_decl>
        </type_name>
        ```
        """
        with self.node("type_name"):
            with self.node("type"):
                self.walk(node.specifier)
            if node.decl is not None:
                with self.node("abstract_decl"):
                    self.walk(node.decl)
    
    def walk_AbstractDeclarator(self, node: models.AbstractDeclarator, *args, **kwargs):
        """Generates XML for an abstract declarator.
        ```xml
        <abstract_decl>
            <ptr>...</ptr>
        </abstract_decl>
        ```
        """
        with self.node("abstract_decl"):
            with self.node("ptr"):
                self.text(node.ptr.ptr)

    def walk_PostfixListExpr(self, node: models.PostfixListExpr, *args, **kwargs):
        """Generates XML for a postfix list expression.
        ```xml
        <array_access>
            <arg>...</arg>
            <index>...</index>
        </array_access>
        ```
        """
        with self.node("array_access"):
            with self.node("arg"):
                self.walk(node.postfix)
            with self.node("index"):
                self.walk(node.index)

    def walk_PostfixFuncExpr(self, node: models.PostfixFuncExpr, *args, **kwargs):
        """Generates XML for a postfix function expression.
        ```xml
        <call>
            <fun>...</fun>
            <args>...</args>
        </call>
        ```
        """
        with self.node("call"):
            with self.node("fun"):
                self.walk(node.postfix)
            with self.node("args"):
                self.walk(node.args) # type: ignore

    def walk_PostfixStructExpr(self, node: models.PostfixStructExpr, *args, **kwargs):
        """Generates XML for a postfix struct expression.
        ```xml
        <struct_access>
            <arg>...</arg>
            <field>...</field>
        </struct_access>
        ```
        """
        with self.node("struct_access"):
            with self.node("arg"):
                self.walk(node.postfix)
            with self.node("field"):
                self.walk(node.field)

    def walk_PostfixPointerExpr(self, node: models.PostfixPointerExpr, *args, **kwargs):
        """Generates XML for a postfix pointer expression.
        ```xml
        <pointer_access>
            <arg>...</arg>
            <field>...</field>
        </pointer_access>
        ```
        """
        with self.node("pointer_access"):
            with self.node("arg"):
                self.walk(node.postfix)
            with self.node("field"):
                self.walk(node.field)

    def walk_PostfixCrementExpr(self, node: models.PostfixCrementExpr, *args, **kwargs):
        """Generates XML for a postfix crement expression.
        ```xml
        <post>
            <op>...</op>
            <arg>...</arg>
        </post>
        ```
        """
        with self.node("post"):
            with self.node("op"):
                self.text(node.op)
            with self.node("arg"):
                self.walk(node.postfix)

    def walk_ParenthesisExpr(self, node: models.ParenthesisExpr, *args, **kwargs):
        """Generates XML for a parenthesis expression.
        ```xml
        <paren>...</paren>
        ```
        """
        with self.node("paren"):
            self.walk(node.arg)
    
    def walk_IdentifierExpr(self, node: models.IdentifierExpr, *args, **kwargs):
        """Generates XML for an identifier expression.
        ```xml
        <id_expr>...</id_expr>
        ```
        """
        with self.node("id_expr"):
            self.walk(node.name)

    def walk_Identifier(self, node: models.Identifier, *args, **kwargs):
        """Generates XML for an identifier.
        ```xml
        (name)
        ```
        """
        self.text(node.name)

    def walk_Number(self, node: models.Number, *args, **kwargs):
        """Generates XML for a number.
        ```xml
        <int_literal>...</int_literal>
        <float_literal>...</float_literal>
        <double_literal>...</double_literal>
        ```
        """
        match node.type:
            case "int":
                with self.node("int_literal"):
                    self.text(node.value)
            case "float":
                with self.node("float_literal"):
                    self.text(node.value)
            case "double":
                with self.node("double_literal"):
                    self.text(node.value)

    def walk_ExprStmt(self, node: models.ExprStmt, *args, **kwargs):
        """Generates XML for an expression statement.
        ```xml
        <expr_stmt>...</expr_stmt>
        ```
        """
        if node.expression is not None:
            with self.node("expr_stmt"):
                self.walk(node.expression)
        else:
            with self.node("empty"):
                pass

    def walk_CompoundStmt(self, node: models.CompoundStmt, *args, **kwargs):
        """Generates XML for a compound statement.
        ```xml
        <compound>
            <decl>...</decl>
            <stmt>...</stmt>
            ...
        </compound>
        ```
        """
        with self.node("compound"):
            self.walk(node.items_) # type: ignore

    def walk_IfStmt(self, node: models.IfStmt, *args, **kwargs):
        """Generates XML for an if statement.
        ```xml
        <if>
            <cond>...</cond>
            <then>...</then>
            <?else>...</else>
        </if>
        ```
        """
        with self.node("if"):
            with self.node("cond"):
                self.walk(node.cond)
            with self.node("then"):
                self.walk(node.then)
            if node.else_ is not None:
                with self.node("else"):
                    self.walk(node.else_)

    def walk_ForStmt(self, node: models.ForStmt, *args, **kwargs):
        """Generates XML for a for statement.
        ```xml
        <for>
            <init>...</init>
            <cond>...</cond>
            <?step>...</step>
            <body>...</body>
        </for>
        ```
        """
        with self.node("for"):
            with self.node("init"):
                self.walk(node.init)
            with self.node("cond"):
                self.walk(node.cond)
            if node.update_ is not None:
                with self.node("step"):
                    self.walk(node.update_)
            with self.node("body"):
                self.walk(node.loop)

    def walk_WhileStmt(self, node: models.WhileStmt, *args, **kwargs):
        """Generates XML for a while statement.
        ```xml
        <while>
            <cond>...</cond>
            <body>...</body>
        </while>
        ```
        """
        with self.node("while"):
            with self.node("cond"):
                self.walk(node.cond)
            with self.node("body"):
                self.walk(node.loop)

    def walk_BreakStmt(self, node: models.BreakStmt, *args, **kwargs):
        """Generates XML for a break statement.
        ```xml
        <break />
        ```
        """
        with self.node("break"):
            pass

    def walk_ContinueStmt(self, node: models.ContinueStmt, *args, **kwargs):
        """Generates XML for a continue statement.
        ```xml
        <continue />
        ```
        """
        with self.node("continue"):
            pass

    def walk_ReturnStmt(self, node: models.ReturnStmt, *args, **kwargs):
        """Generates XML for a return statement.
        ```xml
        <return>...</return>
        ```
        """
        with self.node("return"):
            self.walk(node.value)

