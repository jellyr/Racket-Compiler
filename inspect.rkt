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

(define expr '((define (big [a : Integer]
                            [b : Integer]
                            [c : Integer]
                            [d : Integer]
                            [e : Integer] 
                            [f : Integer]
                            [g : Integer]
                            [h : Integer]
                            [i : Integer]
                            [j : Integer]) : Integer
        (+ d j))
(big 1 2 3 20 5 6 7 8 9 22)))

(test `(("typechecker1" ,typechecker ,interp-scheme) ,@r4-passes) expr)
