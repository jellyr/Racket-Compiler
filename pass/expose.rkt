#lang racket
(require "../common.rkt")
(provide expose-allocation)


(define (expose-helper instr)
  (match instr
    [`(has-type ,instr ,t) `((has-type ,@(expose-helper instr) ,t))]
    [`(assign ,lhs (has-type (vector . ,ve) ,vector-types))
     (let* ([len (length ve)]
            [bytes^ (* 8 (add1 len))])
       `((has-type (if (collection-needed? ,bytes^)
                       ((collect ,bytes^))
                       ()) Void)
         (assign ,lhs (has-type (allocate ,len ,vector-types) ,vector-types))
         ,@(map (lambda (val idx)
                  (let ([voidvar (gensym 'void)])
                    `(assign ,voidvar (has-type (vector-set! (has-type ,lhs ,vector-types)
                                                             (has-type ,idx Integer)
                                                             ,val) Void)))) ve (build-list (length ve) values))))]
    [`(if ,cnd ,thn ,els) `((if ,@(expose-helper cnd) ,(append-map expose-helper thn) ,(append-map expose-helper els)))]
    [else  `(,instr)]))

(define (expose-func-helper instr)
       (match-define `(define (,fname . ,params) : ,ret ,local-vars . ,body) instr)
       `((define (,fname . ,params) : ,ret ,local-vars . ,(append-map expose-helper body))))

(define (expose-allocation e)
  ;(match-define `(,ftypes ,fvartypes ,types) (uncover-types e))
  (match-define `(program ,mvars ,ret (defines . ,def) . ,body) e)
  (append  `(program ,mvars ,ret) ;`(program ,types ,ret)
           `((defines ,@(append-map expose-func-helper def)))
           `((has-type (initialize 10000 ,HEAP-LEN) Void))
           (append-map expose-helper body)))
