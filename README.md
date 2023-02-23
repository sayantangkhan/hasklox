# hasklox
Haskell implementation of the Lox interpreter from 'Crafting Interpreters'.

## Notes on progress

### Feb 23, 2023
Currently working through Chapter 8 (*Statements and state*) of *Crafting Interpreters*.
It turns out once we add variables, evaluating statements and binary operations involves interacting with the interpreter state a little more, since the variables refer to memory locations.

#### Things to update
- [x] Parser: The program should parse a list of declarations, and each declaration is a variable declaration, or a statement.
- [x] Parser: Also add an identifier case for expression.
- [x] Abstract syntax tree: Add corresponding types for identifiers, and variable declarations.
- [ ] Interpreter: Update evaluator to store declared variables.
- [ ] Interpreter: Update evaluator to load stored values of declared variables.

### Parser

Rather than using a parser combinator library like `megaparsec`, we decided to go with `happy`, a `yacc` style parser generator. The pros of this choice were a simplified parser, and avoiding ambiguity in the grammar, but it comes with a major downside of having really poor error messages. A possible future change would be to rewrite the parser using `megaparsec`, and add better error messages. However, to do that, we'll need to stratify the grammar to get rid of left recursion.