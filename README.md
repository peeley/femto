# femto
A minimal Lisp interpreter.

## About
This project is a Lisp interpreter built in Haskell from the ground up
without Parsec/Attoparsec. The project is roughly based on
[Write a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
, and also some of [Stephen
Diehl](http://dev.stephendiehl.com/fun/index.html)'s blog posts on
parsers/compilers. The Lisp dialect this project implements does not meet any
specification for Scheme, Common Lisp, or any other major dialect.

## Current Features
- Define and evaluate lists and basic data types
- Apply and evaluate arithmetic, boolean logic, and list functions
- Define variables
- Define functions
- Error handling, errors do not crash REPL
- Import code from other source files
- IO functions for input/output to CLI

## TODO
- Define macros
- Compilation and code generation with LLVM
