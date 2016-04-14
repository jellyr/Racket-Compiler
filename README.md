# P423-523
Srikanth Kanuri (srkanuri)

Yang Zhang (zhang505)


If there are some problems when do the `git pull`, please `git clone`

Important Commands:

To compile runtime.c : gcc -c -g -std=c99 runtime.c
To run ASM code      : gcc -g runtime.o <filename>.s -> followed by another command after this one -> ./a.out

Compiler Tests:
(compiler-tests "assignment1" typechecker r5-passes "r1" (range 1 22))
(compiler-tests "assignment2" typechecker r5-passes "r1a" (range 1 9))
(compiler-tests "assignment3" typechecker r5-passes "r2" (range 1 23))
(compiler-tests "assignment4" typechecker r5-passes "r3" (range 1 16))
(compiler-tests "assignment5" typechecker r5-passes "r4" (range 1 20))
(compiler-tests "assignment6" typechecker r5-passes "r5" (range 1 13))
(compiler-tests "assignment7" typechecker r5-passes "r7" (range 0 12))

Mandatory require statements:
(require "interp.rkt")
(require "utilities.rkt")
(require "compiler.rkt")

Interpreter Tests:
(interp-tests "r1" typechecker r5-passes interp-scheme "r1" (range 1 22))
(display "r1 tests passed!") (newline)

(interp-tests "r1a" typechecker r5-passes interp-scheme "r1a" (range 1 9))
(display "r1a tests passed!") (newline)

(interp-tests "r2" typechecker r5-passes interp-scheme "r2" (range 1 23))
(display "r2 tests passed!") (newline)

(interp-tests "r3" typechecker r5-passes interp-scheme "r3" (range 1 16))
(display "r3 tests passed!") (newline)

(interp-tests "r4" typechecker r5-passes interp-scheme "r4" (range 1 20))
(display "r4 tests passed!") (newline)

(interp-tests "r5" typechecker r5-passes interp-scheme "r5" (range 1 13))
(display "r5 tests passed!") (newline)

(interp-tests "r7" typechecker r5-passes interp-scheme "r7" (range 0 12))
(display "r7 tests passed!") (newline)

