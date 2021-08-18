
# MiniML

This is a project I completed as part of Harvard's CSCI E-51 "Abstraction and Design in Computation".
MiniML is a metacircular interpreter for a subset of OCaml. Simply put, it is an interpreter written in OCaml that interprets a small part of the OCaml programming language.


Given a lexer and parser, I implemented both substitution semantics and dynamic environment semantics for evaluation. In addition, I wrote multiple unit tests to ensure correctness for the functions I wrote. I also extended the parser to allow for parsing of floating point values, and added evaluation for floats as well.

To run the program, first compile it with `ocamlbuild -use-ocamlfind miniml.byte`, and then use the command `./miniml.byte` to run the MiniML REPL. Now, you can input some valid OCaml expressions and evaluate them. See tests.ml for examples of OCaml expressions that the interpreter can evaluate.