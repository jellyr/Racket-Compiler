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
  (let loop ([ls passes] [prog `(program ,exp)])
    (if (null? ls) (begin (display "done") (newline))
        (match (car ls)
          (`(,name ,func ,_)
           (display (string-append name ":")) (newline)
           (define new-prog (func prog))
           (if (string=? name "print-x86")
               (begin (display new-prog) (newline))
               (begin (pretty-print new-prog) (newline))) 
           (loop (cdr ls) new-prog))))))

(define expr '(vector-ref
  (let ([t1 (vector 1)])
  (let ([t2 (vector 1 2)])
  (let ([t3 (vector 1 2 3)])
  (let ([t4 (vector 1 2 3 4)])
  (let ([t5 (vector 1 2 3 4 5)])
  (let ([t6 (vector 1 2 3 4 5 6)])
  (let ([t7 (vector 1 2 3 4 5 6 7)])
  (let ([t8 (vector 1 2 3 4 5 6 7 8)])
  (let ([t9 (vector 1 2 3 4 5 6 7 8 9)])
  (let ([t0 (vector 1 2 3 4 5 6 7 8 9 10)])
    t0))))))))))
  0)
)

(test `(("typechecker1" ,typechecker ,interp-scheme) ,@r3-passes) expr)
