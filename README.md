# P423-523
Srikanth Kanuri (srkanuri)

Yang Zhang (zhang505)


If there are some problems when do the `git pull`, please `git clone`

Important Commands:

To compile runtime.c : gcc -c -g -std=c99 runtime.c
To run ASM code      : gcc -g runtime.o <filename>.s -> followed by another command after this one -> ./a.out

Compiler Tests:
(compiler-tests “assignment1" typechecker r3-passes "r1" (range 1 12))
(compiler-tests “assignment2" typechecker r3-passes "r1a" (range 1 9))
(compiler-tests “assignment3" typechecker r3-passes "r2" (range 1 22))
(compiler-tests “assignment4" typechecker r3-passes "r3" (range 1 16))

Mandatory require statements:
(require "interp.rkt")
(require "utilities.rkt")
(require "compiler.rkt")

