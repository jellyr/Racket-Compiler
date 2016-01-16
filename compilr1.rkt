#lang racket
(require racket/fixnum)
(require "interp.rkt")
(require "utilities.rkt")

(define uniquify
  (lambda (alist)
    (lambda (e)
      (match e
        [(? symbol?) (lookup e alist)]
        [(? integer?) e]
        [`(let ([,x ,e]) ,body)
         (let* ([newx (gensym x)]
               [newlist (cons `(,x . ,newx) alist)])
           `(let ([,newx ,((uniquify alist) e)])
              ,((uniquify newlist) body)))]
        [`(program ,e) `(program ,((uniquify alist) e))]
        [`(,op ,es ...)
          `(,op ,@(map (uniquify alist) es))]))))


(define (flatten e)
  (match e
    [(or (? fixnum?) (? symbol?)) (values e '() '())]
    [`(read) (let [(newvar (gensym))]
               (values newvar  `((assign ,newvar (read))) `(,newvar)))]
    [`(- ,e1) (let [(newvar (gensym))]
                (let-values ([(e^ statements^ alist) (flatten e1)])
                  (values newvar (append statements^ `((assign ,newvar (- ,e^)))) (cons newvar alist))))]
    [`(+ ,e1 ,e2) (let [(newvar (gensym))]
                    (let-values (((e1^ stmt1^ alist1^) (flatten e1))
                                 ((e2^ stmt2^ alist2^) (flatten e2)))
                      (values newvar
                              (append stmt1^ (append stmt2^ `((assign ,newvar (+ ,e1^ ,e2^)))))
                              (append (cons newvar alist1^) alist2^))))]
    [`(program ,e) (let-values ([(e^ stmt^ alist^) (flatten e)])
                     `(program ,alist^ ,@stmt^ (return ,e^)))]
    [`(let ([,x ,e]) ,body) (let-values
                                ([(xe^ stmtx^ alistx^) (flatten e)]
                                 [(be^ stmtb^ alistb^) (flatten body)])
                              (let* [(xe^ (if (null? stmtx^) xe^ (last (last stmtx^))))
                                     (alistx^ (cons x (if (null? alistx^) alistx^ (cdr alistx^))))
                                     (stmtx^ (if (null? stmtx^) '() (take stmtx^ (sub1 (length stmtx^)))))]
                                (values be^
                                        (append stmtx^ (append `((assign ,x ,xe^)) stmtb^))
                                        (append alistx^ alistb^))))]))



(define (select-instructions-assign e)
  (match e
    [(? fixnum?) `(int ,e)]
    [(? symbol?) `(var ,e)]
    [`(assign ,var (- ,e1)) `((movq ,(select-instructions-assign e1) (var ,var)) . (neq (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e1) __]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e2) __]
    [`(assign ,var (+ ,e1 ,e2)) `((movq ,(select-instructions-assign e1) (var ,var)) . (addq (select-instructions-assign e2) (var ,var)))]
    ))

(define (select-instructions e)
  (match e
    
    
    ;[`(read) __]
    
    
    ;[`(assign ,var ,e^) (select-instrctions e^)]
    [`(program ,e) __]
    []
    [`(let ([,x ,e]) ,body) __]))

