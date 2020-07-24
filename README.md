# A simple compiler for basic mathematical expressions

Inspired by Graham Hutton's stack-based compiler for mathematical expressions in the book, "Programming in Haskell (2nd Edition)".
The idea is to contrast a direct evaluation mechanism for such expressions against separating code and data into separate stacks,
thereby simulating a more realistic compiler.

## Build and 

Note that this project is a Common Lisp system written from scratch. To load the system, run from the REPL:

```
	> (require "asdf")
	> (asdf:load-system "expr-compiler")
```

Note that this project has a dependency on [cl-algebraic-data-type](http://quickdocs.org/cl-algebraic-data-type/). If you are using 
QuickLisp, you must install it ahead of loading the system:

```
	> (ql:quickload "cl-algebraic-data-type")

```

In any case, ensure that this dependency is available (and visible) before running the project.

## Sample run

```
CL-USER> (in-package #:com-tzj-expr-compiler)
#<PACKAGE "COM-TZJ-EXPR-COMPILER">

COM-TZJ-EXPR-COMPILER> (test)
#.(ADD #.(VAL 1) #.(VAL 2)) has the value: (3)
#.(ADD #.(ADD #.(VAL 1) #.(VAL 2)) #.(ADD #.(VAL 3) #.(VAL 4))) has the value: (10)
#.(MUL #.(SUB #.(VAL 10) #.(VAL 2)) #.(ADD #.(VAL 1) #.(VAL 2))) has the value: (24)
#.(DIV #.(ADD #.(SUB #.(VAL 9) #.(VAL 8)) #.(VAL 100)) #.(VAL 2)) has the value: (101/2)
#.(DIV #.(ADD #.(VAL 1) #.(VAL 3)) #.(SUB #.(MUL #.(VAL 2) #.(VAL 1)) #.(DIV #.(VAL 4) #.(VAL 2)))) has the value: (0)
NIL

COM-TZJ-EXPR-COMPILER> (main)
#. (Add #. (Val 1) #. (Val 2))
For the expression #.(ADD #.(VAL 1) #.(VAL 2))
result of eval = 3
result of exec = (3)
```

# LICENCE

See [LICENCE](LICENSE.md).
