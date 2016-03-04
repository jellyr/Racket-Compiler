#lang racket
(require "../common.rkt")
(provide expose-allocation)


(define (expose-helper instr types)
  (match instr
    [`(assign ,lhs (vector . ,ve)) (let* ([len (length ve)]
                                          [bytes^ (* 8 (add1 len))])
                                     `((if (collection-needed? ,bytes^)
                                           ((collect ,bytes^))
                                           ())
                                       (assign ,lhs (allocate ,len ,(lookup lhs types)))
                                       ,@(map (lambda (val idx)
                                                (let ([voidvar (gensym 'void)])
                                                  `(assign ,voidvar (vector-set! ,lhs ,idx ,val))))
                                              ve
                                              (build-list (length ve) values))))]
    [else  `(,instr)]))

(define (expose-func-helper instr types)
       (match-define `(define (,fname . ,params) : ,ret ,local-vars . ,body) instr)
       `((define (,fname . ,params) : ,ret ,types . ,(append-map (curryr expose-helper types) body))))

(define (expose-allocation e)
  (match-define `(,ftypes ,fvartypes ,types) (uncover-types e))
  (match-define `(program ,mvars ,ret (defines . ,def) . ,body) e)
  (append  `(program ,(append ftypes types) ,ret)
           `((defines ,@(append-map expose-func-helper def fvartypes)))
           `((initialize 10000 ,HEAP-LEN))
           (append-map (curryr expose-helper types) body)))
