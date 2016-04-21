#lang racket
(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")




(interp-tests "assignment4 r1" typechecker
                        r5-passes interp-scheme "r1" (range 1 22))
(display "r1 tests passed!") (newline)

(interp-tests "r1a" typechecker
                        r5-passes interp-scheme "r1a" (range 1 9))
(display "r1a tests passed!") (newline)

(interp-tests "assignment4 r2" typechecker
                        r5-passes interp-scheme "r2" (range 1 23))

(display "r2 tests passed!") (newline)

(interp-tests "assignment4 r3" typechecker
                        r5-passes interp-scheme "r3" (range 1 16))

(display "r3 tests passed!") (newline)

(interp-tests "assignment5 r4" typechecker
                        r5-passes interp-scheme "r4" (range 1 20))


(display "r4 tests passed!") (newline)

(interp-tests "assignment5 r4" typechecker
                        r5-passes interp-scheme "r5" (range 1 12))


(display "r5 tests passed!") (newline)

