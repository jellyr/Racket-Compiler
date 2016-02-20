#lang racket
(require "utilities.rkt")
(require "compiler.rkt")

(compiler-tests "compiler r" (curry typecheck-R2 '())
                        r2-passes interp-scheme "r" (range 0 1))
