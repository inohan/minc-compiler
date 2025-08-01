# Extended MinC compiler
This is the extension of the min-C compiler

## Members
- Satoshi Inoue  (03240403): solo, no one else

## Usage
1. Compile with `~/notebooks/pl08_minc/extra/parser`
    ```bash
    cd ~/notebooks/pl08_minc/extra/parser
    source .venv/bin/activate
    python3 main.py file_in.c > file_out.xml
    ```

2. Run `minc`
    ```bash
    cd ~/notebooks/pl08_minc/extra/compiler
    go build
    ./minc file_out.xml file_asm.s
    ```

## References
Below are the (major) references I used for the expansion of the compiler:
- [ANSI C YACC Grammar](https://www.quut.com/c/ANSI-C-grammar-y.html): was helpful in creating the ebnf file for the parser. Implementations like multi-variable declarations couldn't have been achieved without this.
- [Procedure Call Standard for the Arm® 64-bit Architecture (AArch64)](https://student.cs.uwaterloo.ca/~cs452/docs/rpi4b/aapcs64.pdf): was helpful in implementing function calls with more than 8 variables
- [Compiler Explorer](https://godbolt.org/): Experimenting on how expressions are compiled to assembly.

## Modifications
Not all modifications made to the [parser](./parser/) is implemented in the compiler.

- Different binary operators, such as `>>`, `<<`, `&`, `^`, `|`, `+=`, `*=`, etc.
    - Logical AND/OR and ternary operators were not implemented, as these operators with side effects must be evaluated using branch; I was too lazy to do so. For example, `x && y++` increments `y` if `x`, but does not if not. This is known as side effects.
- Implementation of different types including `char`, `short`, `int`, `float`, and `double`. 
    - `unsigned` versions were not implemented.
- Type casting (both implicit and explicit) of different data types: For example, `double / int` would automatically cast `int` to `double`. The result of this is visible at test case `f106.c` and `f107.c`.
- `for` loops
- Declaration of variables
    - Can be done at any time inside a compound statement
    - Can be done while assigning (`long a = ...`)
    - Multiple declarations can be done at once (`int a = 0, b = 2`)
    - Can be done within `for` initializer
- Increments and decrements (`++x`, `x++`, `--x`, `x--`)
- Pointers
    - Pointer arithmetics are supported: `pointer + int`, `int + pointer`, `pointer - pointer`
    - References (`&`) and dereferences (`*`) are supported, along with assignments
- Breaks and continues

## Future work
- The structure of the project should make it easy to implement structs in the future; in fact, the implementation for struct is already done in the parser side.
- Static sized arrays should also be fairly easy to implement
- Dynamic sized arrays, like `int y[x]`, on the other hand, is much more difficult to implement, as this changes the size of the stack at runtime.

## Implementations
### Stack
Stacks are implemented using the [`StackManager` class](./compiler/stack.go). It holds the current stack size and the maximum stack size, which ultimately decides the size of the stack. Note that the `StackManager` is passed as a **copy**, and not as a **reference** in most cases, especially within statements. This is to reduce stack size overall. For example,
```c
int a = x + y; // StmtExpr
return a + 2; // StmtReturn
```
When evaluating the first `StmtExpr`, `x` is allocated to the stack while `y` is being calculated. However, the stack space where `x` was stored is unnecessary after the `StmtExpr` ends, and can be reused for storing `a` in `a+2` (2nd line). In order to do so, `Stmt2IR` passes a **copy** of `StackManager` so that the original `StackManager` does not get changed after the `StmtExpr` is done evaluating.

### Intermediate Expressions
The expressions and statements aren't directly translated into assembly, but instead translated into `Inst` interfaces. The `Inst` interface is responsible for distinguishing general-purpose- and SIMD/floating-point-registers - for example, `InstAdd` converts into `add x0, x0, x1` or `fadd d0, d0, d1` depending on the type of register used. This reduces the code needed for type coersion.

### Declarators
Due to the possibility of multiple variables being declared in a single declaration (e.g. `int a = 0, *b, c[]` is valid), declarations are parsed into the type specifier `int` and multiple initial declarations (`a = 0`, `*b`, `c[]`). This makes identifying the type difficult, because it requires both the information of `int` and the name `*b`.

This is done by a function called [`GetTypeAndIdentifier`](./compiler/utils.go), which uses generic typing to recurse through the declarator until the wanted identifier is found (declarators are nested into structures when parsing - for example, declarator `* x[]` would be interpreted as `*((x)[])`, consisting of a pointer, array, and identifier). Then the type is interpreted **from inward**- for example, `int *x[]` -> `int (*((x)[]))` -> `Array<Pointer<int>>`.

### Type Inferences
Types are propagated upwards along with `[]Inst`. For example, in a expression of `int + (double + float)`, `double + float` is casted into `double + double`, and returned as a `double`. Then, `int + double` is casted to `double + double`, and as `double`. This also applies for function calls - calling `f(2.2)` on `f(int)` would automatically cast `2.2` to an `int`.