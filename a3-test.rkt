#lang racket
(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")




; (define expr '(if #f 0 42))
; (define expr1 '(if #f (if #f #t #t) #t))


; (test r2-passes expr)
; (test r2-passes expr1)

(interp-tests "assignment3 r2" (curry typecheck-R2 '())
                        r2-passes interp-scheme "r2" (range 1 23))
(display "assignemnt3 r2 tests passed!") (newline)
