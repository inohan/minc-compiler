package main

import (
	"fmt"
	"reflect"
)

func GetTypeSize(typeExpr TypeExpr) int {
	switch t := typeExpr.(type) {
	case *TypePrimitive:
		switch t.name {
		case "long":
			return 8
		case "int":
			return 4
		case "short":
			return 2
		case "char":
			return 1
		case "double":
			return 8
		case "float":
			return 4
		default:
			panic(fmt.Sprintf("Unsupported primitive type %s", t.name))
		}
	case *TypePointer:
		return 8
	case *TypeArray:
		return GetTypeSize(t.type_) * int(t.size.(*ExprIntLiteral).val) //TODO: Handle dynamic array sizes
	default:
		panic(fmt.Sprintf("Unsupported type %s", reflect.TypeOf(t)))
	}
}

func GetNaturalAlignment(typeExpr TypeExpr) int {
	switch t := typeExpr.(type) {
	case *TypePrimitive:
		switch t.name {
		case "long":
			return 8
		case "int":
			return 4
		case "short":
			return 2
		case "char":
			return 1
		case "double":
			return 8
		case "float":
			return 4
		default:
			panic(fmt.Sprintf("Unsupported primitive type %s", t.name))
		}
	case *TypePointer:
		return 8
	case *TypeArray:
		return GetNaturalAlignment(t.type_)
	default:
		panic(fmt.Sprintf("Unsupported type %s", reflect.TypeOf(t)))
	}
}

func IsIntegerType(typeExpr TypeExpr) bool {
	t, ok := typeExpr.(*TypePrimitive)
	if !ok {
		return false
	}
	switch t.name {
	case "long", "int", "short", "char":
		return true
	default:
		return false
	}
}

func IsFloatingPointType(typeExpr TypeExpr) bool {
	t, ok := typeExpr.(*TypePrimitive)
	if !ok {
		return false
	}
	switch t.name {
	case "float", "double":
		return true
	default:
		return false
	}
}

func IsPointerType(typeExpr TypeExpr) bool {
	_, ok := typeExpr.(*TypePointer)
	return ok
}

func GetCommonType(typeExpr1 TypeExpr, typeExpr2 TypeExpr) (TypeExpr, bool) {
	primitive1, ok1 := typeExpr1.(*TypePrimitive)
	primitive2, ok2 := typeExpr2.(*TypePrimitive)
	if ok1 && ok2 {
		if primitive1.name == "double" || primitive2.name == "double" {
			return &TypePrimitive{"double"}, true
		} else if primitive1.name == "float" || primitive2.name == "float" {
			return &TypePrimitive{"float"}, true
		} else if primitive1.name == "long" || primitive2.name == "long" {
			return &TypePrimitive{"long"}, true
		} else if primitive1.name == "int" || primitive2.name == "int" {
			return &TypePrimitive{"int"}, true
		} else if primitive1.name == "short" || primitive2.name == "short" {
			return &TypePrimitive{"short"}, true
		} else if primitive1.name == "char" || primitive2.name == "char" {
			return &TypePrimitive{"char"}, true
		}
	}
	return nil, false
}

func IsEqualType(typeExpr1 TypeExpr, typeExpr2 TypeExpr) bool {
	switch t1 := typeExpr1.(type) {
	case *TypePrimitive:
		t2, ok := typeExpr2.(*TypePrimitive)
		if !ok {
			return false
		}
		return t1.name == t2.name
	case *TypePointer:
		t2, ok := typeExpr2.(*TypePointer)
		if !ok {
			return false
		}
		return IsEqualType(t1.type_, t2.type_)
	default:
		return false
	}
}
