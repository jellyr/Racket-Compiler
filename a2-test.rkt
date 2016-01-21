#lang racket

(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")

(define data1 
  '(program (v w x y z)
            (movq (int 1) (var v))
            (movq (int 46) (var w))
            (movq (var v) (var x))
            (addq (int 7) (var x))
            (movq (var x) (var y))
            (addq (int 4) (var y))
            (movq (var x) (var z))
            (addq (var w) (var z))
            (movq (var z) (reg rax))
            (subq (var y) (reg rax))))

(interp-tests "assignment2" r1-passes interp-scheme "r1" (range 1 20))

(display "assignemnt2 tests passed!") (newline)
