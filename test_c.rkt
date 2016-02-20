#lang racket
(require "utilities.rkt")
(require "compiler.rkt")

(compiler-tests "compiler r" typechecker
                        r3-passes "r" (range 0 1))
