#lang racket

(require "utilities.rkt")
(require "interp.rkt")
(require "compiler.rkt")


(interp-tests "assignment2 r1" (curry typecheck-R2 '()) r2-passes interp-scheme "r1" (range 1 22))
(display "assignemnt2 r1 tests passed!") (newline)

(interp-tests "assignment2 r1a" (curry typecheck-R2 '()) r2-passes interp-scheme "r1a" (range 1 9))
(display "assignemnt2 r1a tests passed!") (newline)


'(program
  (g185325 g190181 g190184 g190183)
  (type Integer)
  (defines
   (define (lam185326 (clos185327 : _) (x181516 : Integer))
     :
     Integer
     ()
     (return x181516)))
  (assign g190181 (has-type (function-ref lam185326) _))
  (assign g185325 (has-type (vector (has-type g190181 _)) (Vector _)))
  (assign
   g190183
   (has-type
    (vector-ref (has-type g185325 (Vector _)) (has-type 0 Integer))
    _))
  (assign
   g190184
   (has-type
    (app
     (has-type g190183 _)
     (has-type g185325 (Vector _))
     (has-type 42 Integer))
    Integer))
  (return g190184))

'(program
  (type Integer)
  (defines
   (define (lam185326 (clos185327 : _) (x181516 : Integer))
     :
     Integer
     (return x181516)))
  (has-type (initialize 10000 10000) Void)
  (assign g190181 (has-type (function-ref lam185326) _))
  (if (collection-needed? 16) ((collect 16)) ())
  (assign g185325 (has-type (allocate 1) Void))
  (has-type
   (assign
    void198839
    (has-type (vector-set! g185325 0 (has-type g190181 _)) Void))
   Void)
  (assign
   g190183
   (has-type
    (vector-ref (has-type g185325 (Vector _)) (has-type 0 Integer))
    _))
  (assign
   g190184
   (has-type
    (app
     (has-type g190183 _)
     (has-type g185325 (Vector _))
     (has-type 42 Integer))
    Integer))
  (return g190184))
