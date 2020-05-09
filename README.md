# SIMPL in OCaml

## Interpreter for SIMPL
* Implement an interpreter for the language SIMPL, whose syntax and operational semantics are defined
* An interpreter executes an arbitrary program step-by-step according to the programming language’s operational semantics
* Our interpreter will consist of three modules: a lexer, a parser, and an evaluator
* The job of the lexer and parser is to read a SIMPL program from a text file and convert it into an OCaml data structure
* The evaluator simulates the SIMPL program and returns the final memory state that results

## Type-Checker for SIMPL
* Implement a type-checker for the typed SIMPL language defined
* A type-checker verifies that a program is well-typed according to the programming language’s typing rules
* The typing rules are designed so that well-typed programs never “go wrong” at runtime. Thus, passing programs through a type-checker before running them ensures that all behavior is well-defined in terms of the language’s operational semantics
* Our implementation consists of four parts: a lexer, a parser, an evaluator, and a type-checker

## Extending Type-Checker for functions in SIMPL
* Functions in SIMPL will be first- class, which means that programmers can pass them as parameters to other functions or assign them to variables. We will also allow recursion and mutual recursion
* Our language won’t include explicit support for currying or partial evaluation the way OCaml does, but programmers will be able to simulate both by writing functions that return functions. We will not support polymorphism in our language.
* Extend the type-checker solution above to also type-checks function definitions and function calls
