#lang racket
(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")


;; 0 12
(interp-tests "assignment6 r7" typechecker
                        r7-passes interp-scheme "r7" (range 0 1))

(display "r6 tests passed!") (newline)