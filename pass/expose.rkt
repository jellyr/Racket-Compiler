#lang racket
(require "../common.rkt")
(provide expose-allocation)


(define (expose-helper instr)
  (match instr
    [`(has-type ,instr ,t) `((has-type ,@(expose-helper instr) ,t))]
    [`(assign ,lhs (has-type (vector . ,ve) ,vector-types)) (let* ([len (length ve)]
                                            [bytes^ (* 8 (add1 len))])
                                       `((has-type (if (collection-needed? ,bytes^)
                                                       ((collect ,bytes^))
                                                       ()) Void)
                                         (assign ,lhs (has-type (allocate ,len ,vector-types) ,vector-types))
                                       ,@(map (lambda (val idx)
                                                (let ([voidvar (gensym 'void)])
                                                  `(has-type
                                                    (assign ,voidvar (has-type (vector-set! ,lhs ,idx ,val) Void))
                                                    Void)))
                                              ve
                                              (build-list (length ve) values))))]
    [else  `(,instr)]))

(define (expose-func-helper instr)
       (match-define `(define (,fname . ,params) : ,ret ,local-vars . ,body) instr)
       `((define (,fname . ,params) : ,ret . ,(append-map expose-helper body))))

(define (expose-allocation e)
  ;(match-define `(,ftypes ,fvartypes ,types) (uncover-types e))
  (match-define `(program ,mvars ,ret (defines . ,def) . ,body) e)
  (append  `(program ,mvars ,ret) ;`(program ,types ,ret)
           `((defines ,@(append-map expose-func-helper def)))
           `((has-type (initialize 10000 ,HEAP-LEN) Void))
           (append-map expose-helper body)))
