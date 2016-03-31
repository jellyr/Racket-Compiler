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

(define expr '( (define (even? [x : Integer]) : Boolean 
  (if (eq? x 0)
      #t
      (odd? (+ (- 1) x))))
(define (odd? [x : Integer]) : Boolean 
  (if (eq? x 0)
      #f
      (even? (+ (- 1) x))))
(let ([vec (vector odd?)])
  (let ([dummy (vector-set! vec 0 even?)])
    (if ((vector-ref vec 0) 21) 999 42)))
)
)

(test `(("typechecker1" ,typechecker ,interp-scheme) ,@r5-passes) expr)


