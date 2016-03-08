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

(define (folder-helper intrs)
  (foldr (lambda (x r)
           (define x^ (lower-conditionals-helper x))
           (if (eq? 'if (car x))
               (append x^ r)
               (cons x^ r))
           )  '() intrs))

(define (def-helper instr)
  (match-define `(define ,paras ,st-arg ,st-var . ,instrs) instr)
  `(define ,paras ,st-arg ,st-var . ,(folder-helper instrs)))

(define (lower-conditionals e)
  (match-define `(program ,len ,ret (defines . ,defs) . ,instrs) e)
  (define insts (folder-helper instrs))
  `(program ,len ,ret (defines . ,(map def-helper defs)) . ,insts))
