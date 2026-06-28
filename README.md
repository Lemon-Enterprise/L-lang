# L-lang

A modern programming language built from scratch in OCaml.

## Features

- Custom manual Lexer and recursive-descent Pratt Parser.
- Strict semicolon-free syntax (line terminators are completely free).
- Native exclusive **Lists** (`[1; 2; 3]`) using semicolons as internal delimiters.
- Conditional blocks (`if`, `elseif`, `else`).

## How to Install

Make sure you have OCaml and Dune installed, then run:

```bash
dune build
dune install
```

## How to Use

Run the interactive REPL:

```bash
l-lang
```

Or execute a file:

```bash
l-lang script.l
```
