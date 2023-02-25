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
- [x] Interpreter: Update evaluator to store declared variables.
- [x] Interpreter: Update evaluator to load stored values of declared variables.

### Feb 24, 2023

#### How much should we evaluate?
Consider the following example

```
var a = 5;
var b = 6;
var c = a + b;
print c;
```

This piece of code prints the following.

```
Plus
  a
  b
```

Clearly, the expression a variable is assigned is not evaluated, and the actual AST node is stored.
There are some ways to fix this.

1. Evaluate the right hand side during the assignment as much as possible. This will work fine for variable declarations, but mutually recursive functions (or even just recursive functions will pose a problem).
2. Be as lazy as possible, evaluating only when needed. This can lead to subtle bugs. Consider the following situation.
```
var a = 4;
var b = a;
{
    var a = 5;
    print b;
}
```
This should print 4, but instead will print 5, which is incorrect.
3. Consider a mixture of lazy and eager. Eagerly evaluate the RHS for variable declaration, but exercise the right amount of laziness to make function declarations work.

### Parser

Rather than using a parser combinator library like `megaparsec`, we decided to go with `happy`, a `yacc` style parser generator. The pros of this choice were a simplified parser, and avoiding ambiguity in the grammar, but it comes with a major downside of having really poor error messages. A possible future change would be to rewrite the parser using `megaparsec`, and add better error messages. However, to do that, we'll need to stratify the grammar to get rid of left recursion.