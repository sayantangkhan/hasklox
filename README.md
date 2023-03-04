# hasklox
Haskell implementation of the Lox interpreter from 'Crafting Interpreters'.

## TODOs

- [ ] Reimplement `for` to be desugared to `while` instead of being directly evaluated by the interpreter.
- [ ] Figure out how to store values, functions, and classes in memory other than as AST objects.
- [ ] Set up tests for the code already written.
- [x] Add some code to benchmark performance as well.
- [x] Implement proper metadata for all nodes of the AST.
- [x] Figure out strictness vs. laziness in the data structures. (Made all nodes of the AST strict)
- [x] Figure out how to enter and exit scopes in a balanced fashion. (Defined a new function `inBlock`)
- [x] Set up more pedantic hlints. (Actually, the defaults seem just fine)

### Proper metadata for all AST nodes
- [x] Literals
- [x] Identifiers
- [x] Identifier assignments
- [x] Unary operations
- [x] Binary operations
- [x] Grouping
- [x] Logical OR
- [x] Logical AND
- [x] Expressions
- [x] Statements
- [x] Expression statement
- [x] Print statement
- [x] If statement
- [x] While statement
- [x] For statement
- [x] Block
- [x] Declarations
- [x] Variable declarations
- [x] Inner statements
- [ ] Program (won't implement metadata for this since it's unnecessary)

## Further reading (and possible ideas)

- [Series](https://blog.josephmorag.com/posts/mcc2/) on writing a compiler in Haskell using LLVM. Could use this to make lox a compiled language.
- [Course](https://www.cs.cmu.edu/~btitzer/cs17-670/fall2022/) that follows the structure of _Crafting Interpreters_ to write an JIT compiler to WASM.
