# Plato

**Plato** is a minimal, statically-typed, compiled programming language designed to be fast, safe, and simple. It is an educational and experimental language project, aiming to explore compiler design, type systems, and low-level code generation.

## Goals

- Strong static typing
- Compiled directly to machine code (no C backend)
- Simple and readable syntax
- Fast compilation and execution
- Designed to grow gradually in complexity

## Building

You need [OCaml](https://ocaml.org) and [Dune](https://dune.build) installed.

```sh
dune build
```

This will produce the `plato` compiler binary in `_build/default/src/main.exe`.

To install it globally (inside your opam sandbox):

```sh
dune install
```

## Contributing

Contributions are welcome! Feel free to open issues, suggest features, or submit pull requests.

Whether you're interested in language design, compiler internals, or want to build tools for Plato — you're invited to help shape the project.

## Project Structure

```
src/        → Compiler source code (lexer, parser, AST, etc.)
test/       → Tests and examples
dune-project → Dune build configuration
```

## License

This project is MIT licensed.
