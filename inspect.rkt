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

(define expr '( (define (make-wrapper [in : (Integer -> Integer)] [out : (Integer -> Integer)])
  : ((Integer -> Integer) -> (Integer -> Integer))
  (lambda: ([fn : (Integer -> Integer)]) : (Integer -> Integer)
           (lambda: ([x : Integer]) : Integer
                    (out (fn (in x))))))

(define (add1 [x : Integer]) : Integer (+ x 1))
(define (sub1 [x : Integer]) : Integer (+ x (- 1)))

(define (constfun [x : Integer]) : Integer 42)
(define (double [x : Integer]) : Integer (+ x x))

(let ([wrapper (make-wrapper add1 sub1)])
  (let ([wrapconst (wrapper constfun)])
    (let ([wrapdub (wrapper double)])
      (let ([a (wrapdub 11)])
        (constfun 777)))))
)
)

(test `(("typechecker1" ,typechecker ,interp-scheme) ,@r5-passes) expr)


