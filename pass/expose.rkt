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

(define (expose-allocation e)
  (let ([ut (uncover-types e)])
    (append  `(,(car e) ,ut ,(caddr e) (initialize 10000 ,HEAP-LEN)) (append-map (curryr expose-helper ut) (cdddr e)))))
