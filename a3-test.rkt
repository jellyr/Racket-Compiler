#lang racket
(require "inspect.rkt")
(require "compiler.rkt")




(define expr '(program (if #f 0 42)))

(test r2-passes expr)
