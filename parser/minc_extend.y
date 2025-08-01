@@grammar::minC

# The grammar for this C subset is dereived from https://www.quut.com/c/ANSI-C-grammar-y.html

# the whole thing is a program, followed by EOF
start =
  program $ ;

# program is zero or more definitions
program::Program =
  definitions:{definition}*
  ;

# definition is a function definition (no global vars or type definitions)
definition =
  | @:fun_definition
  | @:declaration
  ;

# function definition is like "long f(long x, long y) { ... }"
fun_definition::FuncDefinition =
  type:declaration_specifiers decl:declarator body:compound_stmt
  ;

# int a, b[], *c, f(...)
declaration::Declaration =
  specifier:declaration_specifiers decls:",".{init_declarator}+ ";"
  ;

# int, float, long, etc.
# In reality, declaration_specifiers can consist of multiple type_specifiers (`unsigned int`)
declaration_specifiers =
  @:type_specifier
  ;

type_specifier =
  | @:primitive_type
  | @:struct_or_union_specifier
  ;

primitive_type::PrimitiveType =
  type:("long"|"int"|"short"|"char"|"void"|"float"|"double"|"bool"|"string")
  ;

# struct _mystruct { ...}
struct_or_union_specifier::StructOrUnionSpecifier =
  | type:("struct"|"union") "{" members:{struct_declaration}* "}"
  | type:("struct"|"union") name:identifier "{" members:{struct_declaration}* "}"
  | type:("struct"|"union") name:identifier
  ;

# int a, b[]; (inside structs)
struct_declaration::StructDeclaration =
  specifier:specifier_qualifier_list declarators:",".{struct_declarator}+ ";"
  ;

specifier_qualifier_list =
  @:type_specifier
  ;

struct_declarator =
  @:declarator
  ;

init_declarator::InitDeclarator =
  | decl:declarator "=" ~ init:initializer
  | decl:declarator
  ;

# Initializer is an expression or struct initializer
initializer =
  @:assignment_expr
  ;

# x, *x, * x[], etc.
declarator =
  | @:pointer_declarator
  | @:direct_declarator
  ;

# *x, **x, **x[], etc.
pointer_declarator::PointerDeclarator =
  ptr:pointer decl:declarator
  ;

# **
pointer::Pointer =
  ptr:/[*]+/
  ;

# x, x[], (*x), etc.
direct_declarator =
  | @:direct_array_declarator
  | @:direct_func_declarator
  | @:direct_parenthesis_declarator
  | @:identifier_declarator
  ;

identifier_declarator::IdentifierDeclarator =
  name:identifier
  ;

# (*x)[]
direct_parenthesis_declarator::ParenthesisDeclarator =
  "(" decl:declarator ")"
  ;

# x[], x[y]
direct_array_declarator::ArrayDeclarator =
  | decl:direct_declarator "[" "]"
  | decl:direct_declarator "[" size:assignment_expr "]"
  ;

# f(long x, long y)
direct_func_declarator::FuncDeclarator =
  decl:direct_declarator "(" params:",".{parameter_declaration}* ")"
  ;

# long x (inside function parameters)
parameter_declaration::ParameterDeclaration =
  specifier:declaration_specifiers declarator:declarator
  ;

# statements
stmt =
  | @:continue_stmt                 # continue
  | @:break_stmt                    # break
  | @:return_stmt                   # return
  | @:compound_stmt                 # { ... }
  | @:if_stmt                       # if
  | @:while_stmt                    # while
  | @:for_stmt                      # for
  | @:expr_stmt                     # expression (e.g., f(x))
  ;


expr_stmt::ExprStmt =
  | ";"
  | expression:expr ";"
  ;

continue_stmt::ContinueStmt =
  "continue" ";"
  ;

break_stmt::BreakStmt =
  "break" ";"
  ;

return_stmt::ReturnStmt =
  "return" ~ [value:expr] ";"
  ;

compound_stmt::CompoundStmt =
  "{" items_:{declaration|stmt}* "}"
  ;

if_stmt::IfStmt =
  "if" "(" cond:expr ")" then:stmt [ "else" else_:stmt ]
  ;

while_stmt::WhileStmt =
  "while" "(" cond:expr ")" loop:stmt
  ;

for_stmt::ForStmt =
  | "for" "(" init:expr_stmt cond:expr_stmt ")" loop:stmt
  | "for" "(" init:expr_stmt cond:expr_stmt update_:expr ")" loop:stmt
  | "for" "(" init:declaration cond:expr_stmt ")" loop:stmt
  | "for" "(" init:declaration cond:expr_stmt update_:expr ")" loop:stmt
  ;

# definition of expression reflects operator precedence
# and left-/right-associativity
# the entire expression
# the operator having the weakest precedence is =
expr =
  | @:comma_expr
  | @:assignment_expr
  ;

comma_expr::CommaExpr =
  args+:expr op:"," args+:assignment_expr
  ;

assignment_expr = 
  | @:assignment
  | @:cond_expr
  ;

assignment::BinaryExpr =
  args+:unary_expr op:("="|"+="|"-="|"*="|"/="|"%=") args+:assignment_expr
  ;

cond_expr =
  | @:cond
  | @:logical_or_expr
  ;

cond::CondExpr =
  check_expr:logical_or_expr "?" true_expr:expr ":" false_expr:expr
  ;

logical_or_expr =
  | @:logical_or
  | @:logical_and_expr
  ;

logical_or::BinaryExpr =
  args+:logical_or_expr op:"||" args+:logical_and_expr
  ;

logical_and_expr =
  | @:logical_and
  | @:inclusive_or_expr
  ;

logical_and::BinaryExpr =
  args+:logical_and_expr op:"&&" args+:equality_expr
  ;

inclusive_or_expr =
  | @:inclusive_or
  | @:exclusive_or_expr
  ;

inclusive_or::BinaryExpr =
  args+:inclusive_or_expr op:"|" args+:exclusive_or_expr
  ;

exclusive_or_expr =
  | @:exclusive_or
  | @:and_expr
  ;

exclusive_or::BinaryExpr =
  args+:exclusive_or_expr op:"^" args+:and_expr
  ;

and_expr =
  | @:and
  | @:equality_expr
  ;

and::BinaryExpr =
  args+:and_expr op:"&" args+:equality_expr
  ;

equality_expr =
  | @:equality
  | @:relational_expr
  ;

equality::BinaryExpr =
  args+:equality_expr op:("=="|"!=") args+:relational_expr
  ;

relational_expr =
  | @:relational
  | @:shift_expr
  ;

relational::BinaryExpr =
  args+:relational_expr op:("<="|">="|"<"|">") args+:additive_expr
  ;

shift_expr =
  | @:shift
  | @:additive_expr
  ;

shift::BinaryExpr =
  args+:shift_expr op:("<<"|">>") args+:additive_expr
  ;

# the operators having the weakest precedence in additive_expr are
# + and -
additive_expr =
  | @:additive
  | @:multiplicative_expr
  ;

additive::BinaryExpr =
  args+:additive_expr op:("+"|"-") args+:multiplicative_expr
  ;

# the operators having the weakest precedence in multiplicative_expr are
# *, / and %
multiplicative_expr =
  | @:multiplicative
  | @:cast_expr
  ;

multiplicative::BinaryExpr =
  args+:multiplicative_expr op:("*"|"/"|"%") args+:cast_expr
  ;

cast_expr =
  | @:cast
  | @:unary_expr
  ;

cast::CastExpr =
  "(" type:type_name ")" arg:cast_expr
  ;

type_name::TypeName = 
  specifier:type_specifier [decl:abstract_declarator]
  ;

# Only supports pointer for now, but can be extended to support array casts (int [])
abstract_declarator::AbstractDeclarator =
  ptr:pointer
  ;

unary_expr =
  | @:unary
  | @:postfix_expr
  | @:dereference
  | @:reference
  ;
# sizeof and alignof are not supported in this grammar

unary::UnaryExpr =
  | op:("++"|"--") expr:unary_expr
  | op:("+"|"-"|"!"|"~") expr:cast_expr
  ;

# *x
dereference::Dereference =
  "*" arg:cast_expr
  ;

# &x
reference::Reference =
  "&" arg:cast_expr
  ;

postfix_expr =
  | @:postfix_list
  | @:postfix_func
  | @:postfix_struct
  | @:postfix_pointer
  | @:postfix_increment_or_decrement
  | @:primary_expr
  ;

postfix_list::PostfixListExpr =
  postfix:postfix_expr "[" index:expr "]"
  ;

postfix_func::PostfixFuncExpr =
  postfix:postfix_expr "(" args:",".{assignment_expr}* ")"
  ;

postfix_struct::PostfixStructExpr =
  postfix:postfix_expr "." field:identifier
  ;

postfix_pointer::PostfixPointerExpr =
  postfix:postfix_expr "->" field:identifier
  ;

postfix_increment_or_decrement::PostfixCrementExpr =
  postfix:postfix_expr op:("++"|"--")
  ;

primary_expr =
  | @:parenthesis_expr
  | @:identifier_expr
  | @:number
  ;

parenthesis_expr::ParenthesisExpr =
  "(" arg:expr ")"
  ;

# Identifiers inside expressions
identifier_expr::IdentifierExpr =
  name:identifier
  ;

number::Number =
  | value:float type:`float`
  | value:double type:`double`
  | value:int type:`int`
  ;

int::int = /\d+/ ;
double::float = /\d+\.\d+/ ;
float::float = /(\d+\.\d+)f/ ;
identifier::Identifier = name:/[A-Za-z_][A-Za-z_0-9]*/ ;
