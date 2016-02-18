#lang racket
(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")




; (interp-tests "assignment4 r1" typechecker
;                         r3-passes interp-scheme "r1" (range 1 22))
; (display "r1 tests passed!") (newline)

(interp-tests "r1a" typechecker
                        r3-passes interp-scheme "r1a" (range 7 8))
(display "r1a tests passed!") (newline)

; (interp-tests "assignment4 r2" typechecker
;                         r3-passes interp-scheme "r2" (range 1 23))


; (display "r2 tests passed!") (newline)
