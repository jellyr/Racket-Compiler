#lang racket

(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")


(interp-tests "assignment2 r1" (curry typecheck-R2 '()) r2-passes interp-scheme "r1" (range 1 22))
(display "assignemnt2 r1 tests passed!") (newline)

(interp-tests "assignment2 r1a" (curry typecheck-R2 '()) r2-passes interp-scheme "r1a" (range 1 9))
(display "assignemnt2 r1a tests passed!") (newline)
