package main

import (
	"cmp"
	"fmt"
)

func Align(addr int, alignSize int) int {
	if addr%alignSize == 0 {
		return addr
	} else {
		return addr + alignSize - addr%alignSize
	}
}

func Any[T any](slice []T, callback func(obj T) bool) bool {
	for _, obj := range slice {
		if callback(obj) {
			return true
		}
	}
	return false
}

func Max[T cmp.Ordered](values ...T) T {
	if len(values) == 0 {
		panic("Max: no values provided")
	}
	max := values[0]
	for _, v := range values[1:] {
		if v > max {
			max = v
		}
	}
	return max
}

// Check if a statement has a function call inside
func HasFunctionCallStmt(stmt Stmt) bool {
	switch s := stmt.(type) {
	case *StmtCompound:
		return Any(s.decls_or_stmts, func(item *DeclStmtUnion) bool {
			if item.decl != nil {
				return Any(item.decl.decls, func(item *InitDeclarator) bool {
					return HasFunctionCallExpr(item.init)
				})
			}
			return HasFunctionCallStmt(item.stmt)
		})
	case *StmtWhile:
		return HasFunctionCallExpr(s.cond) || HasFunctionCallStmt(s.body)
	case *StmtExpr:
		return HasFunctionCallExpr(s.expr)
	case *StmtIf:
		return HasFunctionCallExpr(s.cond) || HasFunctionCallStmt(s.then_stmt) || HasFunctionCallStmt(s.else_stmt)
	case *StmtReturn:
		return HasFunctionCallExpr(s.expr)
	default:
		return false
	}
}

func HasFunctionCallExpr(expr Expr) bool {
	switch e := expr.(type) {
	case *ExprCall:
		return true
	case *ExprOp:
		return Any(e.args, HasFunctionCallExpr)
	case *ExprParen:
		return HasFunctionCallExpr(e.sub_expr)
	default:
		return false
	}
}

func GetAllFunctionCalls(stmt Stmt) []*ExprCall {
	calls := []*ExprCall{}
	var recurseStmt func(stmt Stmt)
	var recurseExpr func(expr Expr)
	recurseStmt = func(stmtRecurse Stmt) {
		switch s := stmtRecurse.(type) {
		case *StmtCompound:
			for _, childStmt := range s.decls_or_stmts {
				if childStmt.decl != nil {
					for _, init_decl := range childStmt.decl.decls {
						if init_decl.init != nil {
							recurseExpr(init_decl.init)
						}
					}
				} else {
					recurseStmt(childStmt.stmt)
				}
			}
		case *StmtIf:
			recurseExpr(s.cond)
			recurseStmt(s.then_stmt)
			recurseStmt(s.else_stmt)
		case *StmtWhile:
			recurseExpr(s.cond)
			recurseStmt(s.body)
		case *StmtReturn:
			recurseExpr(s.expr)
		case *StmtExpr:
			recurseExpr(s.expr)
		}
	}
	recurseExpr = func(exprRecurse Expr) {
		switch e := exprRecurse.(type) {
		case *ExprCall:
			calls = append(calls, e)
		case *ExprOp:
			for _, arg := range e.args {
				recurseExpr(arg)
			}
		case *ExprParen:
			recurseExpr(e.sub_expr)
		}
	}
	recurseStmt(stmt)
	return calls
}

func CopyMap[K comparable, V any](m map[K]V) map[K]V {
	newMap := make(map[K]V)
	for k, v := range m {
		newMap[k] = v
	}
	return newMap
}

func GetTypeAndIdentifier[T Declarator](specifier TypeExpr, decl Declarator) (TypeExpr, T) {
	typeExpr := specifier
	for {
		d, ok := decl.(T)
		if ok {
			return typeExpr, d
		}
		switch d := decl.(type) {
		case *DeclaratorPointer:
			for i := 0; i < len(d.ptr); i++ {
				typeExpr = &TypePointer{typeExpr}
			}
			decl = d.decl
		case *DeclaratorArray:
			typeExpr = &TypeArray{typeExpr, d.size}
			decl = d.decl
		case *DeclaratorParenthesis:
			decl = d.decl
		default:
			panic(fmt.Sprintf("Unsupported declarator type %T", decl))
		}
	}
}

func GetTypeExprFromTypeName(typeName *TypeName) TypeExpr {
	typeExpr := typeName.type_expr
	for i := 0; i < len(typeName.ptr); i++ {
		typeExpr = &TypePointer{typeExpr}
	}
	return typeExpr
}
