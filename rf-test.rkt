#lang racket
(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")

;;; for the final project test cases

(interp-tests "final" infer-program
                        r5-passes interp-scheme "rf" (range 0 1))


(display "final project tests passed!") (newline) 