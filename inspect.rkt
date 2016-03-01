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

(define expr '(typechecker '(define (map-vec [f : (Integer -> Integer)][v : (Vector Integer Integer)]): (Vector Integer Integer)(vector(f (vector-ref v 0)) (f (vector-ref v 1))))(define (add1 [x : Integer]) : Integer(+ x 1))(vector-ref (map-vec add1 (vector 0 41)) 1))
)

(test `(("typechecker1" ,typechecker ,interp-scheme) ,@r3-passes) expr)
