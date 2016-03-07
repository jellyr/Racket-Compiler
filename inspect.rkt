#lang racket
(require "interp.rkt")
(require "compiler.rkt")

(provide test)

;;;; some helpful dev code
;;;; see: https://piazza.com/class/ij8uxz86huctk?cid=46

(define (test passes exp)
  (display "--------------------------") (newline)
  (pretty-print exp)
  (display "--------------------------") (newline)
  (let loop ([ls passes] [prog `(program ,@exp)])
    (if (null? ls) (begin (display "done") (newline))
        (match (car ls)
          (`(,name ,func ,_)
           (display (string-append name ":")) (newline)
           (define new-prog (func prog))
           (if (string=? name "print-x86")
               (begin (display new-prog) (newline))
               (begin (pretty-print new-prog) (newline))) 
           (loop (cdr ls) new-prog))))))

(define expr '(
 (define (mult [x : Integer] [y : Integer]) : Integer
  (if (eq? 0 x)
      0
      (+ y (mult (+ (- 1) x) y))))
(mult 6 7)))

(test `(("typechecker1" ,typechecker ,interp-scheme) ,@r3-passes) expr)
