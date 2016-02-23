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

(define expr '(let ([v1 (vector 0)])
  (let ([g1 (vector 1 2 3 4 5)])
    (let ([dummy
           (if (eq? (read) 0)
               (vector-set! v1 0 42)
               (vector-set! g1 0 42))])
      (vector-ref v1 0))))
)

(test `(("typechecker1" ,typechecker ,interp-scheme) ,@r3-passes) expr)
