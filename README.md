# P423-523
Srikanth Kanuri (srkanuri)

Yang Zhang (zhang505)


If there are some problems when do the `git pull`, please `git clone`

Important Commands:

To compile runtime.c : gcc -c -g -std=c99 runtime.c
To run ASM code      : gcc -g runtime.o <filename>.s -> followed by another command after this one -> ./a.out

Compiler Tests:
(require "interp.rkt")
(require "utilities.rkt")
(require "compiler.rkt")

(compiler-tests "assignment1" infer-program r5-passes "rf" (range 0 10))


