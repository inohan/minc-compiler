package main

import (
	"fmt"
	"math"
	"reflect"
)

func ast_to_asm_program(program *Program) string {
	asmGlobal := ""
	labelNum := 2
	globalEnv := &GlobalEnv{make(map[string]any), make(map[string]*FuncData), &labelNum}
	asm := ""
	for _, c := range program.defs {
		switch v := c.(type) {
		case *DefFun:
			asm += ast_to_asm_def(v, globalEnv) + "\n"
		default:
			panic(fmt.Sprintf("Unsupported type %s", reflect.TypeOf(v)))
		}
	}
	// Add .globl tag for all functions
	for name, _ := range globalEnv.Funcs {
		asmGlobal += fmt.Sprintf("\t.globl %s\n", name)
	}
	return asmGlobal + "\n" + asm
}

func ast_to_asm_def(def *DefFun, globalEnv *GlobalEnv) string {
	asm := ""
	insts := Def2IR(def, globalEnv)
	for _, inst := range insts {
		asm += inst.ToASM() + "\n"
	}
	return asm
}

func Def2IR(def *DefFun, globalEnv *GlobalEnv) []Inst {
	// Register function in global env
	typeExpr, declarator := GetTypeAndIdentifier[*DeclaratorFunc](def.type_expr, def.decl)
	name := declarator.decl.(*DeclaratorIdentifier).name
	globalEnv.Funcs[name] = &FuncData{map_array(func(param *DeclarationParameter) TypeExpr {
		typeExpr, _ := GetTypeAndIdentifier[*DeclaratorIdentifier](param.specifier, param.decl)
		return typeExpr
	}, declarator.params), typeExpr}
	funcCalls := GetAllFunctionCalls(def.body)
	hasFunction := len(funcCalls) != 0
	maxStack := 0
	funcCallStackSize := 0
	stack := StackManager{0, &maxStack}
	// Create a return label
	labelEnv := LabelEnv{nil, nil, nil}
	labelEnv.Return = &InstLabel{globalEnv.NewLabel()}
	env := Env{make(map[string]ParamData), labelEnv, typeExpr}

	insts := []Inst{&InstLabel{name}}
	if hasFunction { // If has function call, need space in stack to save x29 and x30
		// Check if any of the function calls need stack to pass arguments
		for _, funcCall := range funcCalls {
			// Get original definition of the function
			funcDef, ok := globalEnv.Funcs[funcCall.fun.(*ExprId).name]
			if !ok {
				panic(fmt.Sprintf("Function %s could not be found", funcCall.fun.(*ExprId).name))
			}
			// Don't directly modify stack manager, instead work with a copy
			stackCopy := stack
			NGRN := 0
			for _, paramType := range funcDef.ParamTypes {
				if NGRN < 8 {
					NGRN++
				} else {
					stackCopy.ExtendCustom(Max(GetTypeSize(paramType), 8), Max(GetNaturalAlignment(paramType), 8))
				}
			}
		}
		// stack = {0, &int(whatever size necessary to pass arguments)}
		funcCallStackSize = Align(*stack.MaxStack, 16)
		stack.SetCurrent(funcCallStackSize)  // stack = {multiple of 16, multiple of 16} to utilize ldp efficiency
		stack.Extend(&TypePrimitive{"long"}) //x29
		stack.Extend(&TypePrimitive{"long"}) //x30

	}
	instsParam := []Inst{}
	// Load params
	NGRN := 0 // Next General-purpose Register Number
	NSRN := 0 // Next SIMD and Floating Point Register Number
	NSAA := 0 // Next Stacked Argument Address
	for _, param := range declarator.params {
		paramType, identifier := GetTypeAndIdentifier[*DeclaratorIdentifier](param.specifier, param.decl)
		paramName := identifier.name
		if IsFloatingPointType(paramType) {
			if NSRN < 8 {
				/* If the argument is a Half-, Single-, Double- or Quad- precision Floating-point or
				 * short vector type and the NSRN is less than 8, then the argument is allocated to the
				 * least significant bits of register v[NSRN]. The NSRN is incremented by one. The
				 * argument has now been allocated. */
				storeAddr := &InstMemorySpOffset{stack.Extend(paramType)}
				env.Vars[paramName] = ParamData{paramType, storeAddr}
				instsParam = append(instsParam, &InstStr{GetStoredReg(paramType, NSRN), storeAddr})
				NSRN++
			} else {
				/*If the argument is an HFA, an HVA, a Half-, Single-, Double- or Quad- precision
				 * Floating-point or short vector type, then the argument is copied to memory at the
				 * adjusted NSAA. The NSAA is incremented by the size of the argument. The
				 * argument has now been allocated. */
				NSAA = Align(NSAA, Max(GetNaturalAlignment(paramType), 8))
				env.Vars[paramName] = ParamData{paramType, &InstMemoryFrameEndOffset{NSAA, &maxStack}}
				NSAA += Max(GetTypeSize(paramType), 8)
			}
		} else if IsIntegerType(paramType) || IsPointerType(paramType) {
			if NGRN < 8 {
				env.Vars[paramName] = ParamData{paramType, &InstMemorySpOffset{stack.Extend(paramType)}}
				instsParam = append(instsParam, &InstStr{&InstValueReg{fmt.Sprintf("x%d", NGRN)}, env.Vars[paramName].MemoryOffset})
				NGRN++
			} else {
				NSAA = Align(NSAA, Max(GetNaturalAlignment(paramType), 8))
				env.Vars[paramName] = ParamData{paramType, &InstMemoryFrameEndOffset{NSAA, &maxStack}}
				NSAA += Max(GetTypeSize(paramType), 8)
			}
		} else {
			panic(fmt.Sprintf("Unsupported parameter type %s", paramType.ast_to_str_type()))
		}
	}
	instsBody := Stmt2IR(def.body, stack, env, globalEnv) // Get all instructions of body statement. At the same time, calculate maximum stack size
	// Calculates the stack size, aligned to a multiple of 16.
	maxStackAligned := Align(maxStack, 16)
	insts = append(insts, &InstSub{&InstValueReg{"sp"}, &InstValueReg{"sp"}, &InstValueInt{maxStackAligned}}) // sub sp, sp, -XX
	if hasFunction {
		insts = append(insts, &InstStp{&InstValueReg{"x29"}, &InstValueReg{"x30"}, &InstMemorySpOffset{funcCallStackSize}}) // stp x29, x30, [sp, YY]
		if funcCallStackSize == 0 {
			insts = append(insts, &InstMov{&InstValueReg{"x29"}, &InstValueReg{"sp"}}) // mov x29, sp
		} else {
			insts = append(insts, &InstAdd{&InstValueReg{"x29"}, &InstValueReg{"sp"}, &InstValueInt{funcCallStackSize}}) // add x29, sp, (size of function stack)
		}
	}
	insts = append(insts, instsParam...) // Save parameters passed through registers to stack
	insts = append(insts, instsBody...)
	// Return label
	insts = append(insts, labelEnv.Return)
	if hasFunction {
		insts = append(insts, &InstLdp{&InstValueReg{"x29"}, &InstValueReg{"x30"}, &InstMemorySpOffset{funcCallStackSize}})
	}
	insts = append(insts, &InstAdd{&InstValueReg{"sp"}, &InstValueReg{"sp"}, &InstValueInt{maxStackAligned}}) // add sp, sp, XX
	insts = append(insts, &InstRet{})
	return insts
}

func Stmt2IR(stmt Stmt, stack StackManager, env Env, globalEnv *GlobalEnv) []Inst {
	insts := []Inst{}
	switch s := stmt.(type) {
	case *StmtCompound:
		// Make a shallow copy of parent env variables, since new variables will be added & should not be reflected onto the parent
		newVars := CopyMap(env.Vars)
		env.Vars = newVars
		for _, decl_or_stmt := range s.decls_or_stmts {
			if decl_or_stmt.decl != nil { // Declaration
				decl := decl_or_stmt.decl
				for _, init_decl := range decl.decls {
					varType, identifier := GetTypeAndIdentifier[*DeclaratorIdentifier](decl.specifier, init_decl.decl)
					newVars[identifier.name] = ParamData{varType, &InstMemorySpOffset{stack.Extend(varType)}}
					if init_decl.init != nil { // Assign also exists
						initInsts, initType := Expr2IR(init_decl.init, stack, env, globalEnv)
						insts = append(insts, TypeCast(initInsts, initType, varType, 0)...) // Cast to the type of the variable
						insts = append(insts, &InstStr{GetStoredReg(varType, 0), newVars[identifier.name].MemoryOffset})
					}
				}
			} else if decl_or_stmt.stmt != nil { // Statement
				asmChild := Stmt2IR(decl_or_stmt.stmt, stack, env, globalEnv)
				insts = append(insts, asmChild...)
			} else {
				panic(fmt.Sprintf("Invalid statement in compound statement: %s", stmt.ast_to_str_stmt()))
			}
		}
	case *StmtExpr:
		instsExpr, _ := Expr2IR(s.expr, stack, env, globalEnv)
		insts = instsExpr
	case *StmtIf:
		instsCond, typeCond := Expr2IR(s.cond, stack, env, globalEnv)
		insts = append(insts, instsCond...)                                          // Evaluate condition expression
		insts = append(insts, &InstCmp{GetStoredReg(typeCond, 0), &InstValueInt{0}}) // cmp x0, 0 (eq: else, ne: then) #FIXME: Fails with floating points probably
		elseLabel := &InstLabel{globalEnv.NewLabel()}
		afterLabel := &InstLabel{globalEnv.NewLabel()}
		insts = append(insts, &InstBeq{elseLabel})                            // b.eq .LElse
		insts = append(insts, Stmt2IR(s.then_stmt, stack, env, globalEnv)...) // Then statement
		insts = append(insts, &InstB{afterLabel})                             // b .LAfter
		insts = append(insts, elseLabel)                                      // .LElse:
		insts = append(insts, Stmt2IR(s.else_stmt, stack, env, globalEnv)...) // Else statement
		insts = append(insts, afterLabel)                                     // .LAfter:
	case *StmtBreak:
		if env.Labels.Finish == nil { // No spot to jump to for finish
			panic("break statement not within a loop")
		}
		insts = append(insts, &InstB{env.Labels.Finish})
	case *StmtContinue:
		if env.Labels.Condition == nil {
			panic("continue statement not within a loop")
		}
		insts = append(insts, &InstB{env.Labels.Finish})
	case *StmtWhile:
		statementLabel := &InstLabel{globalEnv.NewLabel()}
		conditionLabel := &InstLabel{globalEnv.NewLabel()}
		afterLabel := &InstLabel{globalEnv.NewLabel()}
		// Set labels for break and continue statements
		env.Labels.Condition = conditionLabel
		env.Labels.Finish = afterLabel
		insts = append(insts, &InstB{conditionLabel})                    // b .LC
		insts = append(insts, statementLabel)                            // .LS:
		insts = append(insts, Stmt2IR(s.body, stack, env, globalEnv)...) // Statement body
		insts = append(insts, conditionLabel)                            // .LC:
		instsCond, typeCond := Expr2IR(s.cond, stack, env, globalEnv)
		insts = append(insts, instsCond...)                                          // Condition evaluation
		insts = append(insts, &InstCmp{GetStoredReg(typeCond, 0), &InstValueInt{0}}) // cmp x0, 0 (eq: break loop, ne: continue)
		insts = append(insts, &InstBne{statementLabel})                              // b.ne .LS
		insts = append(insts, afterLabel)
	case *StmtFor:
		statementLabel := &InstLabel{globalEnv.NewLabel()}
		conditionLabel := &InstLabel{globalEnv.NewLabel()}
		afterLabel := &InstLabel{globalEnv.NewLabel()}
		// Set labels for break and continue statements
		env.Labels.Condition = conditionLabel
		env.Labels.Finish = afterLabel
		newVars := CopyMap(env.Vars)
		env.Vars = newVars
		// Initialize variables
		if s.init.decl != nil { // Declaration: the scope is inside the body only
			decl := s.init.decl
			for _, init_decl := range decl.decls {
				varType, identifier := GetTypeAndIdentifier[*DeclaratorIdentifier](decl.specifier, init_decl.decl)
				newVars[identifier.name] = ParamData{varType, &InstMemorySpOffset{stack.Extend(varType)}}
				if init_decl.init != nil { // Assign also exists
					initInsts, initType := Expr2IR(init_decl.init, stack, env, globalEnv)
					insts = append(insts, TypeCast(initInsts, initType, varType, 0)...) // Cast to the type of the variable
					insts = append(insts, &InstStr{GetStoredReg(varType, 0), newVars[identifier.name].MemoryOffset})
				}
			}
		} else if s.init.stmt != nil { // Statement
			insts = append(insts, Stmt2IR(s.init.stmt, stack, env, globalEnv)...)
		} else {
			panic(fmt.Sprintf("Invalid initialization in for statement: %s", stmt.ast_to_str_stmt()))
		}
		insts = append(insts, &InstB{conditionLabel})                    // b .LC
		insts = append(insts, statementLabel)                            // .LS:
		insts = append(insts, Stmt2IR(s.body, stack, env, globalEnv)...) // Statement body
		instsStep, _ := Expr2IR(s.step, stack, env, globalEnv)
		insts = append(insts, instsStep...)
		insts = append(insts, conditionLabel)                                  // .LC:
		insts = append(insts, Stmt2IR(s.cond, stack, env, globalEnv)...)       // Condition evaluation
		insts = append(insts, &InstCmp{&InstValueReg{"x0"}, &InstValueInt{0}}) // cmp x0, 0 (eq: break loop, ne: continue) //FIXME:
		insts = append(insts, &InstBne{statementLabel})                        // b.ne .LS
		insts = append(insts, afterLabel)
	case *StmtReturn:
		instsExpr, returnType := Expr2IR(s.expr, stack, env, globalEnv)
		insts = append(insts, TypeCast(instsExpr, returnType, env.ReturnType, 0)...) // Cast to the type of the return statement
		insts = append(insts, &InstB{env.Labels.Return})
	}
	return insts
}

// Converts expressions. Evaluated expressions will be stored in the x0 register for convenience.
func Expr2IR(expr Expr, stack StackManager, env Env, globalEnv *GlobalEnv) ([]Inst, TypeExpr) {
	insts := []Inst{}
	var returnType TypeExpr
	switch e := expr.(type) {
	case *ExprCall:
		funcName := e.fun.(*ExprId).name
		returnType = globalEnv.Funcs[funcName].ReturnType
		instsLdr := []Inst{}
		NGRN := 0
		NSRN := 0
		NSAA := 0
		for i, arg := range e.args {
			argInsts, argInputType := Expr2IR(arg, stack, env, globalEnv)
			argExpectedType := globalEnv.Funcs[funcName].ParamTypes[i]
			insts = append(insts, TypeCast(argInsts, argInputType, argExpectedType, 0)...) // Cast and evaluate each arguments
			if IsFloatingPointType(argExpectedType) {
				if NSRN < 8 {
					memOffset := &InstMemorySpOffset{stack.Extend(argExpectedType)}
					insts = append(insts, &InstStr{GetStoredReg(argExpectedType, NSRN), memOffset})
					instsLdr = append(instsLdr, &InstLdr{GetStoredReg(argExpectedType, NSRN), memOffset})
					NSRN++
				} else { // floating argument 8~
					memOffset := &InstMemorySpOffset{NSAA}
					NSAA = Align(NSAA, Max(GetNaturalAlignment(argExpectedType), 8))
					insts = append(insts, &InstStr{GetStoredReg(argExpectedType, 0), memOffset})
					NSAA += Max(GetTypeSize(argExpectedType), 8)
				}
			} else if IsIntegerType(argExpectedType) || IsPointerType(argExpectedType) {
				if NGRN < 8 {
					memOffset := &InstMemorySpOffset{stack.Extend(argExpectedType)}                       // Allocate stack for each argument. This is because f(expr1, expr2) requires evaluation of expr1, expr2, ..., which might contain functions that destroy register
					insts = append(insts, &InstStr{GetStoredReg(argExpectedType, 0), memOffset})          // str x0, [sp, XX]
					instsLdr = append(instsLdr, &InstLdr{GetStoredReg(argExpectedType, NGRN), memOffset}) // After evaluation of all args, ldr xN, [sp, XX]
					NGRN++
				} else {
					memOffset := &InstMemorySpOffset{NSAA}
					NSAA = Align(NSAA, Max(GetNaturalAlignment(argExpectedType), 8))
					insts = append(insts, &InstStr{GetStoredReg(argExpectedType, 0), memOffset}) // str x0, [sp, YY] (directly store to passing memory)
					NSAA += Max(GetTypeSize(argExpectedType), 8)
				}
			} else {
				panic(fmt.Sprintf("Unsupported argument type %s", argExpectedType.ast_to_str_type()))
			}
		}
		insts = append(insts, instsLdr...) // After computing all arg expressions, align results to x0~x7, w0~w7
		insts = append(insts, &InstBl{funcName})
	case *ExprId:
		varData, ok := env.Vars[e.name]
		if !ok {
			panic(fmt.Sprintf("Unknown variable %s", e.name))
		}
		returnType = varData.VarType
		insts = []Inst{&InstLdr{GetStoredReg(returnType, 0), varData.MemoryOffset}}
	case *ExprIntLiteral:
		returnType = &TypePrimitive{"int"}
		// Check if the immediate value is too large for mov instruction
		// ARM64 mov immediate can only handle 16-bit values
		if e.val > 65535 || e.val < -65535 {
			// Use ldr with literal pool for large immediate values
			insts = []Inst{&InstLdr{GetStoredReg(returnType, 0), &InstMemoryPool{Int: int(e.val)}}}
		} else {
			// Use mov for small immediate values
			insts = []Inst{&InstMov{GetStoredReg(returnType, 0), &InstValueInt{int(e.val)}}}
		}
	case *ExprFloatLiteral:
		returnType = &TypePrimitive{"float"}
		insts = []Inst{
			&InstLdr{GetStoredReg(&TypePrimitive{"int"}, 0), &InstMemoryPool{Int: int(math.Float32bits(e.val))}},
			&InstMov{GetStoredReg(returnType, 0), GetStoredReg(&TypePrimitive{"int"}, 0)},
		}
	case *ExprDoubleLiteral:
		returnType = &TypePrimitive{"double"}
		insts = []Inst{
			&InstLdr{GetStoredReg(&TypePrimitive{"long"}, 0), &InstMemoryPool{Int: int(math.Float64bits(e.val))}},
			&InstMov{GetStoredReg(returnType, 0), GetStoredReg(&TypePrimitive{"long"}, 0)},
		}
	case *ExprParen:
		insts, returnType = Expr2IR(e.sub_expr, stack, env, globalEnv)
	case *ExprOp:
		if len(e.args) == 2 { // Binary operators
			insts, returnType = BinOp2IR(e, stack, env, globalEnv)
		} else if len(e.args) == 1 {
			arg := e.args[0]
			switch e.op {
			case "+":
				insts, returnType = Expr2IR(arg, stack, env, globalEnv)
			case "-":
				insts, returnType = Expr2IR(arg, stack, env, globalEnv)
				insts = append(insts, &InstNeg{&InstValueReg{"x0"}, &InstValueReg{"x0"}})
			case "!":
				insts, returnType = Expr2IR(arg, stack, env, globalEnv)
				insts = append(insts, &InstCmp{&InstValueReg{"x0"}, &InstValueInt{0}})
				insts = append(insts, &InstCset{&InstValueReg{"x0"}, "eq"})
			case "~":
				insts, returnType = Expr2IR(arg, stack, env, globalEnv)
				if !IsIntegerType(returnType) {
					panic(fmt.Sprintf("~ operator is not supported for type %s", returnType.ast_to_str_type()))
				}
				insts = append(insts, &InstMvn{&InstValueReg{"x0"}, &InstValueReg{"x0"}})
			case "++":
				equivalentExpr := &ExprOp{"+=", []Expr{arg, &ExprIntLiteral{1}}} // ++x is identical to x+=1
				insts, returnType = Expr2IR(equivalentExpr, stack, env, globalEnv)
			case "--":
				equivalentExpr := &ExprOp{"-=", []Expr{arg, &ExprIntLiteral{1}}} // --x is identical to x-=1
				insts, returnType = Expr2IR(equivalentExpr, stack, env, globalEnv)
			default:
				panic(fmt.Sprintf("Unknown unary operator %s", e.op))
			}
		}
	case *ExprPost:
		switch e.op {
		case "++":
			// x++ is identical to (x+=1)-1
			equivalentExpr := &ExprOp{"-", []Expr{&ExprOp{"+=", []Expr{e.arg, &ExprIntLiteral{1}}}, &ExprIntLiteral{1}}}
			insts, returnType = Expr2IR(equivalentExpr, stack, env, globalEnv)
		case "--":
			// x-- is identical to (x-=1)+1
			equivalentExpr := &ExprOp{"+", []Expr{&ExprOp{"-=", []Expr{e.arg, &ExprIntLiteral{1}}}, &ExprIntLiteral{1}}}
			insts, returnType = Expr2IR(equivalentExpr, stack, env, globalEnv)
		default:
			panic(fmt.Sprintf("Unknown post-increment/decrement operator %s", e.op))
		}
	case *ExprCast:
		instsInnerExpr, typeInnerExpr := Expr2IR(e.arg, stack, env, globalEnv)
		returnType = GetTypeExprFromTypeName(e.type_name)
		insts = append(insts, TypeCastExplicit(instsInnerExpr, typeInnerExpr, returnType, 0)...)
	case *ExprDeref:
		instsDeref, typeDeref := Expr2IR(e.arg, stack, env, globalEnv)
		insts = append(insts, instsDeref...)
		insts = append(insts, &InstLdr{GetStoredReg(typeDeref, 0), &InstMemoryOffset{&InstValueReg{"x0"}, 0}}) // ldr r0, [x0]
		typePtr, ok := typeDeref.(*TypePointer)
		if !ok {
			panic(fmt.Sprintf("Cannot dereference non-pointer type %s", typeDeref.ast_to_str_type()))
		}
		returnType = typePtr.type_
	case *ExprRef:
		instsRef, typeRef := GetModifiableLValueAddrInsts(e.arg, stack, env, globalEnv)
		insts = append(insts, instsRef...)
		returnType = &TypePointer{typeRef}
	}
	if returnType == nil {
		panic(fmt.Sprintf("Unknown expression type: %s", expr.ast_to_str_expr()))
	}
	return insts, returnType
}

func BinOp2IR(expr *ExprOp, stack StackManager, env Env, globalEnv *GlobalEnv) ([]Inst, TypeExpr) {
	insts := []Inst{}
	// Evaluate left side
	left := expr.args[0]
	right := expr.args[1]
	// Assignment (=, +=, -=, *=, /=, %=)
	if expr.op == "=" || expr.op == "+=" || expr.op == "-=" || expr.op == "*=" || expr.op == "/=" || expr.op == "%=" {
		leftInsts, leftType := GetModifiableLValueAddrInsts(left, stack, env, globalEnv)
		leftResultAddrOffset := &InstMemoryOffset{&InstValueReg{"sp"}, stack.Extend(&TypePointer{})}
		rightInsts, rightType := Expr2IR(right, stack, env, globalEnv)
		insts = append(insts, leftInsts...)                                        // x0 = (left side address)
		insts = append(insts, &InstStr{&InstValueReg{"x0"}, leftResultAddrOffset}) // str x0, [sp, XX]; x0 is always a pointer
		// Evaluate right side and cast
		insts = append(insts, TypeCast(rightInsts, rightType, leftType, 0)...) // (d/s/x/w)0 = (left type) (right value)
		// Load left side address to x1
		insts = append(insts, &InstLdr{&InstValueReg{"x3"}, leftResultAddrOffset}) // ldr x3, [sp, XX]; x0~x2 can be contaminated with modulos
		if len(expr.op) == 2 {                                                     // +=, -=, *=, /=, %=
			insts = append(insts, &InstLdr{GetStoredReg(leftType, 1), &InstMemoryOffset{&InstValueReg{"x3"}, 0}})                        // ldr (d/s/x/w)1, [x3]; (dsxw)1 = (value of left side)
			op := string(expr.op[0])                                                                                                     // +, -, *, /, %
			insts = append(insts, BinOpTemplate(op, GetStoredReg(leftType, 1), GetStoredReg(leftType, 0), GetStoredReg(leftType, 0))...) // r0 = r1 op r0
		}
		insts = append(insts, &InstStr{GetStoredReg(leftType, 0), &InstMemoryOffset{&InstValueReg{"x3"}, 0}}) // str r0, [x3]
		return insts, leftType
	}
	leftInsts, leftType := Expr2IR(left, stack, env, globalEnv)
	leftResultAddrOffset := &InstMemoryOffset{&InstValueReg{"sp"}, stack.Extend(leftType)} // Store left side value AS ORIGINAL TYPE; type conversion must be done later
	rightInsts, rightType := Expr2IR(right, stack, env, globalEnv)
	if IsPointerType(leftType) || IsPointerType(rightType) { // Pointer arithmetic
		switch {
		case (expr.op == "+" || expr.op == "-") && IsPointerType(leftType) && IsIntegerType(rightType): // pointer ± integer
			insts = append(insts, leftInsts...)                                                                             // Evaluate left side
			insts = append(insts, &InstStr{&InstValueReg{"x0"}, leftResultAddrOffset})                                      // str x0, [sp, XX]
			insts = append(insts, TypeCast(rightInsts, rightType, &TypePrimitive{"long"}, 0)...)                            // Cast right side to long (may be char, short, int, long)
			insts = append(insts, &InstLdr{&InstValueReg{"x1"}, leftResultAddrOffset})                                      // ldr x1, [sp, XX]
			insts = append(insts, &InstMov{&InstValueReg{"x2"}, &InstValueInt{GetTypeSize(leftType.(*TypePointer).type_)}}) // ldr x2, [sp, XX]
			if expr.op == "+" {
				insts = append(insts, &InstMAdd{&InstValueReg{"x0"}, &InstValueReg{"x0"}, &InstValueReg{"x2"}, &InstValueReg{"x1"}}) // x0 = x1 + x0 * size of pointer type content
			} else {
				insts = append(insts, &InstMSub{&InstValueReg{"x0"}, &InstValueReg{"x0"}, &InstValueReg{"x2"}, &InstValueReg{"x1"}}) // x0 = x1 + x0 * size of pointer type content
			}
			return insts, leftType
		case (expr.op == "+" || expr.op == "-") && IsIntegerType(leftType) && IsPointerType(rightType): // integer ± pointer
			insts = append(insts, leftInsts...)                                                                                                        // Evaluate left side
			insts = append(insts, &InstStr{GetStoredReg(leftType, 0), leftResultAddrOffset})                                                           // str r0, [sp, XX]
			insts = append(insts, rightInsts...)                                                                                                       // Right side (pointer)
			insts = append(insts, TypeCast([]Inst{&InstLdr{GetStoredReg(leftType, 1), leftResultAddrOffset}}, leftType, &TypePrimitive{"long"}, 1)...) // ldr r1, [sp, XX]; Cast left side to long (may be char, short, int, long)
			insts = append(insts, &InstMov{&InstValueReg{"x2"}, &InstValueInt{GetTypeSize(leftType.(*TypePointer).type_)}})                            // ldr x2, [sp, XX]
			if expr.op == "+" {
				insts = append(insts, &InstMAdd{&InstValueReg{"x0"}, &InstValueReg{"x1"}, &InstValueReg{"x2"}, &InstValueReg{"x0"}}) // x0 = x0 (right) + x1 (left) * size of pointer type content
			} else {
				insts = append(insts, &InstMSub{&InstValueReg{"x0"}, &InstValueReg{"x1"}, &InstValueReg{"x2"}, &InstValueReg{"x0"}}) // x0 = x0 = x0 (right) - x1 (left) * size of pointer type content
			}
			return insts, rightType
		case expr.op == "-" && IsPointerType(leftType) && IsPointerType(rightType) && IsEqualType(leftType, rightType):
			insts = append(insts, leftInsts...) // Evaluate left side
			leftResultAddrOffset := &InstMemoryOffset{&InstValueReg{"sp"}, stack.Extend(leftType)}
			insts = append(insts, &InstStr{&InstValueReg{"x0"}, leftResultAddrOffset})                                      // str x0, [sp, XX]
			insts = append(insts, rightInsts...)                                                                            // Evaluate right side
			insts = append(insts, &InstLdr{&InstValueReg{"x1"}, leftResultAddrOffset})                                      // ldr x1, [sp, XX]
			insts = append(insts, &InstSub{&InstValueReg{"x0"}, &InstValueReg{"x1"}, &InstValueReg{"x0"}})                  // x0 = x1 - x0
			insts = append(insts, &InstMov{&InstValueReg{"x1"}, &InstValueInt{GetTypeSize(leftType.(*TypePointer).type_)}}) // mov x1, size of pointer type content
			insts = append(insts, &InstSdiv{&InstValueReg{"x0"}, &InstValueReg{"x0"}, &InstValueReg{"x1"}})                 // x0 = x0 / x1
			return insts, &TypePrimitive{"int"}                                                                             // Return int
		default:
			panic(fmt.Sprintf("Invalid pointer arithmetic operation: %s", expr.op))
		}
	}
	// Comparisons (==, !=, <, <=, >, >=) are returned as int
	if expr.op == "==" || expr.op == "!=" || expr.op == "<" || expr.op == "<=" || expr.op == ">" || expr.op == ">=" {
		returnType := &TypePrimitive{"int"}
		commonType, ok := GetCommonType(leftType, rightType)
		if !ok {
			panic(fmt.Sprintf("Cannot get common type for %s and %s", leftType.ast_to_str_type(), rightType.ast_to_str_type()))
		}
		insts = append(insts, leftInsts...)                                                                                             // Evaluate left side
		insts = append(insts, &InstStr{GetStoredReg(leftType, 0), leftResultAddrOffset})                                                // str r0, [sp, XX]
		insts = append(insts, TypeCast(rightInsts, rightType, commonType, 0)...)                                                        // Evaluate right side
		insts = append(insts, TypeCast([]Inst{&InstLdr{GetStoredReg(rightType, 1), leftResultAddrOffset}}, leftType, commonType, 1)...) // ldr r1, [sp, XX]; cast to common
		insts = append(insts, &InstCmp{GetStoredReg(commonType, 1), GetStoredReg(commonType, 0)})                                       // cmp r1, r0
		var cmpOp string
		switch expr.op {
		case "==":
			cmpOp = "eq"
		case "!=":
			cmpOp = "ne"
		case "<":
			cmpOp = "lt"
		case "<=":
			cmpOp = "le"
		case ">":
			cmpOp = "gt"
		case ">=":
			cmpOp = "ge"
		}
		insts = append(insts, &InstCset{&InstValueReg{"w0"}, cmpOp}) // cset x0, cmpOp
		return insts, returnType
	}
	// All other binary operators
	commonType, ok := GetCommonType(leftType, rightType) // For example, int + double -> double
	if !ok {
		panic(fmt.Sprintf("Cannot get common type for %s and %s", leftType.ast_to_str_type(), rightType.ast_to_str_type()))
	}
	if expr.op == "%" && IsFloatingPointType(commonType) {
		panic(fmt.Sprintf("Invalid operands to binary %s (have %s and %s)", expr.op, leftType.ast_to_str_type(), rightType.ast_to_str_type()))
	}
	if (expr.op == ">>" || expr.op == "<<") && IsFloatingPointType(commonType) {
		panic(fmt.Sprintf("Invalid operands to binary %s (have %s and %s)", expr.op, leftType.ast_to_str_type(), rightType.ast_to_str_type()))
	}
	insts = append(insts, leftInsts...)                                                                                                     // Evaluate left side
	insts = append(insts, &InstStr{GetStoredReg(leftType, 0), leftResultAddrOffset})                                                        // str r0, [sp, XX]
	insts = append(insts, TypeCast(rightInsts, rightType, commonType, 0)...)                                                                // Evaluate right side
	insts = append(insts, TypeCast([]Inst{&InstLdr{GetStoredReg(leftType, 1), leftResultAddrOffset}}, leftType, commonType, 1)...)          // ldr r1, [sp, XX]; cast into common type
	insts = append(insts, BinOpTemplate(expr.op, GetStoredReg(commonType, 1), GetStoredReg(commonType, 0), GetStoredReg(commonType, 0))...) // Return to d0/s0/x0/w0
	return insts, commonType
}

func BinOpTemplate(op string, leftReg *InstValueReg, rightReg *InstValueReg, returnReg *InstValueReg) []Inst {
	switch op {
	case "+":
		return []Inst{
			&InstAdd{returnReg, leftReg, rightReg},
		}
	case "-":
		return []Inst{
			&InstSub{returnReg, leftReg, rightReg},
		}
	case "*":
		return []Inst{
			&InstMul{returnReg, leftReg, rightReg},
		}
	case "/":
		return []Inst{
			&InstSdiv{returnReg, leftReg, rightReg},
		}
	case "%":
		quotientReg := &InstValueReg{string(returnReg.Name[0]) + "2"}
		return []Inst{
			&InstSdiv{quotientReg, leftReg, rightReg},
			&InstMSub{returnReg, rightReg, quotientReg, leftReg},
		}
	case "&":
		return []Inst{
			&InstAnd{returnReg, leftReg, rightReg},
		}
	case "|":
		return []Inst{
			&InstOrr{returnReg, leftReg, rightReg},
		}
	case "^":
		return []Inst{
			&InstEor{returnReg, leftReg, rightReg},
		}
	case ">>":
		return []Inst{
			&InstAsr{returnReg, leftReg, rightReg},
		}
	case "<<":
		return []Inst{
			&InstLsl{returnReg, leftReg, rightReg},
		}
	default:
		panic(fmt.Sprintf("Unknown binary operator %s", op))
	}
}

// Load address of modifiable lvalue into x0 and return the type of the lvalue
func GetModifiableLValueAddrInsts(expr Expr, stack StackManager, env Env, globalEnv *GlobalEnv) ([]Inst, TypeExpr) {
	insts := []Inst{}
	switch e := expr.(type) { // FIXME: Add later
	case *ExprId:
		memOffset := env.Vars[e.name].MemoryOffset
		typeLvalue := env.Vars[e.name].VarType
		var memOffsetInt int
		switch m := memOffset.(type) {
		case *InstMemorySpOffset:
			memOffsetInt = m.Offset
		default:
			panic(fmt.Sprintf("Unknown memory offset type: %s", reflect.TypeOf(m)))
		}
		insts = append(insts, &InstAdd{&InstValueReg{"x0"}, &InstValueReg{"sp"}, &InstValueInt{memOffsetInt}})
		return insts, typeLvalue
	case *ExprParen:
		return GetModifiableLValueAddrInsts(e.sub_expr, stack, env, globalEnv)
	case *ExprDeref:
		instsPtr, typePtr := Expr2IR(e.arg, stack, env, globalEnv)
		typeLvalue := typePtr.(*TypePointer).type_ //FIXME: Array dereferences can also be accepted in the future
		insts = append(insts, instsPtr...)         // Pulls the value of the pointer directly to x0
		return insts, typeLvalue
	}
	panic(fmt.Sprintf("Not a modifiable lvalue: %s", expr.ast_to_str_expr()))
}

// Implicit type cast
func TypeCast2IR(typeOriginal TypeExpr, typeTarget TypeExpr, regNumber int) []Inst {
	if IsEqualType(typeOriginal, typeTarget) {
		return []Inst{}
	}
	switch orig := typeOriginal.(type) {
	case *TypePrimitive:
		switch target := typeTarget.(type) {
		case *TypePrimitive:
			if IsIntegerType(orig) && IsIntegerType(target) {
				largerType, ok := GetCommonType(orig, target)
				if !ok {
					panic(fmt.Sprintf("Cannot get common type for %s and %s", orig.ast_to_str_type(), target.ast_to_str_type()))
				}
				if IsEqualType(largerType, orig) { // If conversion from broader to narrower type (long -> int, int -> char), no cast needed
					return []Inst{}
				}
				if orig.name == "char" { // char is unsigned half-word in ARM, so replace the higher digits with 0 (applicable for shorts and ints also)
					return []Inst{
						&InstAnd{GetStoredReg(target, regNumber), GetStoredReg(target, regNumber), &InstValueInt{0xFF}},
					}
				} else if orig.name == "short" { // sign extend half-word to signed (short -> int/long)
					return []Inst{
						&InstSxth{GetStoredReg(target, regNumber), GetStoredReg(orig, regNumber)},
					}
				} else { // Sign extend word to signed (int -> long)
					return []Inst{
						&InstSxtw{GetStoredReg(target, regNumber), GetStoredReg(orig, regNumber)},
					}
				}
			} else if IsIntegerType(orig) && IsFloatingPointType(target) { // (integer) -> long -> float/double
				return append(TypeCast2IR(orig, &TypePrimitive{"long"}, regNumber), &InstScvtf{GetStoredReg(target, regNumber), GetStoredReg(orig, regNumber)})
			} else if IsFloatingPointType(orig) && IsIntegerType(target) { // (float/double) -> (integer)
				return []Inst{
					&InstFcvtzs{GetStoredReg(target, regNumber), GetStoredReg(orig, regNumber)},
				}
			} else { // float/double -> float/double
				return []Inst{
					&InstFcvt{GetStoredReg(target, regNumber), GetStoredReg(orig, regNumber)},
				}
			}
		}
	}
	panic(fmt.Sprintf("Unknown type cast: %s to %s", typeOriginal.ast_to_str_type(), typeTarget.ast_to_str_type()))
}

func TypeCastExplicit2IR(typeOriginal TypeExpr, typeTarget TypeExpr, regNumber int) []Inst {
	if IsEqualType(typeOriginal, typeTarget) {
		return []Inst{}
	}
	switch orig := typeOriginal.(type) {
	case *TypePrimitive:
		switch target := typeTarget.(type) {
		case *TypePrimitive:
			return TypeCast2IR(orig, target, regNumber)
		case *TypePointer: // primitive -> pointer cast
			if !IsIntegerType(orig) {
				panic(fmt.Sprintf("Cannot cast non-integer type %s to pointer", orig.ast_to_str_type()))
			}
			return TypeCast2IR(orig, &TypePrimitive{"long"}, regNumber) // long = pointer
		}
	case *TypePointer:
		switch target := typeTarget.(type) {
		case *TypePrimitive:
			if !IsIntegerType(target) {
				panic(fmt.Sprintf("Cannot cast pointer to non-integer type %s", target.ast_to_str_type()))
			}
			return []Inst{}
		case *TypePointer:
			return []Inst{}
		}
	}
	panic(fmt.Sprintf("Unknown type cast: %s to %s", typeOriginal.ast_to_str_type(), typeTarget.ast_to_str_type()))
}

func TypeCast(insts []Inst, typeOriginal TypeExpr, typeTarget TypeExpr, regNumber int) []Inst {
	insts = append(insts, TypeCast2IR(typeOriginal, typeTarget, regNumber)...)
	return insts
}

func TypeCastExplicit(insts []Inst, typeOriginal TypeExpr, typeTarget TypeExpr, regNumber int) []Inst {
	insts = append(insts, TypeCastExplicit2IR(typeOriginal, typeTarget, regNumber)...)
	return insts
}

func GetStoredReg(typeExpr TypeExpr, regNum int) *InstValueReg {
	var r string
	switch t := typeExpr.(type) {
	case *TypePrimitive:
		switch t.name {
		case "double":
			r = "d"
		case "float":
			r = "s"
		case "long":
			r = "x"
		case "int", "short", "char":
			r = "w"
		}
	case *TypePointer:
		r = "x"
	}
	return &InstValueReg{fmt.Sprintf("%s%d", r, regNum)}
}

type Env struct {
	Vars       map[string]ParamData
	Labels     LabelEnv
	ReturnType TypeExpr
}

type ParamData struct {
	VarType      TypeExpr
	MemoryOffset InstMemoryAddr
}

type FuncData struct {
	ParamTypes []TypeExpr
	ReturnType TypeExpr
}

// Env used for managing labels
type LabelEnv struct {
	Return    *InstLabel
	Condition *InstLabel
	Finish    *InstLabel
}

type GlobalEnv struct {
	CustomTypes   map[string]any
	Funcs         map[string]*FuncData
	NameGenerator *int
}

// Generates new label name from DefInfo
func (d *GlobalEnv) NewLabel() string {
	s := fmt.Sprintf(".L%d", *d.NameGenerator)
	*d.NameGenerator += 1
	return s
}
