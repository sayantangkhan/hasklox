# hasklox
Haskell implementation of the Lox interpreter from 'Crafting Interpreters'.

## TODOs

- [ ] Implement proper metadata for all nodes of the AST.
- [ ] Reimplement `for` to be desugared to `while` instead of being directly evaluated by the interpreter.
- [ ] Figure out how to store values, functions, and classes in memory other than as AST objects.
- [ ] Set up tests for the code already written.
- [ ] Add some code to benchmark performance as well.
- [ ] Figure out strictness vs. laziness in the data structures.
- [x] Figure out how to enter and exit scopes in a balanced fashion. (Defined a new function `inBlock`)
- [x] Set up more pedantic hlints. (Actually, the defaults seem just fine)

## Further reading (and possible ideas)

- [Series](https://blog.josephmorag.com/posts/mcc2/) on writing a compiler in Haskell using LLVM. Could use this to make lox a compiled language.
- [Course](https://www.cs.cmu.edu/~btitzer/cs17-670/fall2022/) that follows the structure of _Crafting Interpreters_ to write an JIT compiler to WASM.
