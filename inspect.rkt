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

(define expr '((define (mapint [fun : (Integer -> Integer)] [v : (Vector Integer Integer Integer)])
  : (Vector Integer Integer Integer)
  (vector (fun (vector-ref v 0)) (fun (vector-ref v 1)) (fun (vector-ref v 2))))
(define (add1 [x : Integer]) : Integer
  (+ x 1))
(let ([vec (vector 1 2 3)])
  (let ([vec2 (mapint add1 vec)])
    (+ (vector-ref vec2 0) 
       (+ (vector-ref vec2 1) 
          (+ (vector-ref vec2 2) 33)))))))

(test `(("typechecker1" ,typechecker ,interp-scheme) ,@r4-passes) expr)
