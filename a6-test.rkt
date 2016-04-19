#lang racket
(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")
(require "dynamic-interp.rkt")

;; 0 12
(interp-tests "assignment6 r7" #f r7-passes (interp-r7 '()) "r7" (range 0 1))

(display "r6 tests passed!") (newline)