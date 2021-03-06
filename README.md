# boolean
Boolean is a simple CAS for boolean logic, and a small language to manipulate boolean expressions.

[Live Demo - compiled to WebAssembly](https://boolean-repl.netlify.com/)

## Syntax

Boolean can parse the following expressions and operations:
- Literals (`T` for true, `F` for false)
- Symbolic variables (e.g. `a`, `thing`)
- Negation (e.g. `!x`, `!(a & b)`)
- Conjunction (AND) (`a & b`)
- Disjunction (OR) (`a | b`)
- Implication (`p => q`)
- Biconditional implication (`p <=> q`)

It also understands some of its own syntax:
- Commands (`Table (a & b | c)`, `Satisfy (p => q)`)
- Strings (`"Some text"`)

## Evaluation

The language allows commands to control the evaluation of their own arguments. This mechanism is 
somewhat similar to the `Hold*` family of function attributes in the Wolfram Language--each function gets to 
control how its children in the AST are handled.

Boolean understands the following functions:

### With
Usage: `With expr assumptions`

`With` substitutes a set of concrete `assumptions` into `expr`.

For example:
```
> With (a | b) a
T

> With (a & b) (!a)
F

> With (a & b & !c & d) (b & !c)
a & d
```

### Table
Usage: `Table expr [var...]`

`Table` generates truth tables for `expr`, in disjunctive normal form (e.g. `a | !b`).

For example: 
```
> Table (a | b)
a & !b | !a & b | b & a

> Table ( (a & b) => q ) a b
!a & !b | b & !a | !b & a
```

### Satisfy
Usage: `Satisfy expr`

`Satisfy` finds a set of bindings which satisfy `expr`. 

For example: 
```
> Satisfy (a & b & (!a | b))
b & a
```

# Running it

Boolean has both a CLI (in `./cli`) and a web UI (in `./www`). For the web UI, a server isn't 
required--the core is compiled to WebAssembly, and shipped directly to the browser.

```bash
  # running the CLI
$ cd cli && cargo run

  # build the web UI (requires cargo-web and cross)
$ cd www && yarn install && make
```
