#lang racket
(require "../common.rkt")

(provide lower-conditionals)


(define (lower-conditionals-helper e)
  (define elselabel (gensym 'else))
  (define thenlabel (gensym 'then))
  (define endlabel (gensym 'ifend))
  (match e
    [`(if (eq? ,e1 ,e2) ,thn ,els) `((cmpq ,e1 ,e2)
                                        (je ,thenlabel)
                                        ,@(lower-conditionals-helper els)
                                        (jmp ,endlabel)
                                        (label ,thenlabel)
                                        ,@(lower-conditionals-helper thn)
                                        (label ,endlabel))]
    [`(()) '()]
    [else e]))

(define (lower-conditionals e)
  (define insts (foldr (lambda (x r)
                       (define x^ (lower-conditionals-helper x))
                       (if (eq? 'if (car x))
                           (append x^ r)
                           (cons x^ r))
                       )  '() (cdddr e)))
  ;(define t (if (eq? 1 (length insts)) (car insts) insts))
  `(,(car e) ,(cadr e) ,(caddr e) ,@insts))