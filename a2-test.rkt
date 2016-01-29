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

(define data2 '(program
  ((v w x y z)
   ((v) (w v) (x w) (x w) (x w y) (x w y) (z w y) (z y) (rax y) ()))
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

(define data3 (list
 'program
 (list
  '(v w x y z)
  (hash
   'x
   (set 'y 'w)
   'w
   (set 'v 'y 'z 'x)
   'y
   (set 'w 'z 'x 'rax)
   'z
   (set 'y 'w)
   'rax
   (set 'y)
   'v
   (set 'w)))
 '(movq (int 1) (var v))
 '(movq (int 46) (var w))
 '(movq (var v) (var x))
 '(addq (int 7) (var x))
 '(movq (var x) (var y))
 '(addq (int 4) (var y))
 '(movq (var x) (var z))
 '(addq (var w) (var z))
 '(movq (var z) (reg rax))
 '(subq (var y) (reg rax))))

(define graph1
      (make-hash '()))
(add-edge graph1 'a 'b)
(add-edge graph1 'a 'c)
(add-edge graph1 'a 'd)
(add-edge graph1 'b 'd)




(interp-tests "assignment2" r1-passes interp-scheme "r1" (range 1 20))

(display "assignemnt2 tests passed!") (newline)
