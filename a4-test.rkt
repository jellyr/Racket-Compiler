#lang racket
(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")





(interp-tests "assignment4 r3" typechecker
                        r3-passes interp-scheme "r2" (range 1 23))
(display "assignemnt4 r3 tests passed!") (newline)
