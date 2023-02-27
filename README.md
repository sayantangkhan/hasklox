# hasklox
Haskell implementation of the Lox interpreter from 'Crafting Interpreters'.

## TODOs

- [ ] Implement proper metadata for all nodes of the AST.
- [ ] Reimplement `for` to be desugared to `while` instead of being directly evaluated by the interpreter.
- [ ] Figure out how to enter and exit scopes in a balanced fashion.

### Parser

Rather than using a parser combinator library like `megaparsec`, we decided to go with `happy`, a `yacc` style parser generator. The pros of this choice were a simplified parser, and avoiding ambiguity in the grammar, but it comes with a major downside of having really poor error messages. A possible future change would be to rewrite the parser using `megaparsec`, and add better error messages. However, to do that, we'll need to stratify the grammar to get rid of left recursion.