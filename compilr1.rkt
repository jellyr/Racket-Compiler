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
    [(? symbol?) #:when (not (eq? e 'program)) `(var ,e)]
    [`(assign ,var ,e1) `((movq ,(select-instructions-assign e1) (var ,var)))]
    [`(assign ,var (- ,e1)) `((movq ,(select-instructions-assign e1) (var ,var)) (negq (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e1) `((addq ,(select-instructions-assign e2) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e2) `((addq ,(select-instructions-assign e1) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2)) `((movq ,(select-instructions-assign e1) (var ,var)) (addq ,(select-instructions-assign e2) (var ,var)))]
    [`(return ,e1) `((addq ,(select-instructions-assign e1) (reg rax)))]
    [else `(,e)]
    ))

(define (select-instructions e)
  (append-map select-instructions-assign e))

; starti == -1
(define (assign-homes-env alist starti)
  (cond
    [(null? alist) '()]
    [else (append `((,(car alist) . (stack ,(* 8 starti)))) (assign-homes-env (cdr alist) (sub1 starti)))]))

(define (assign-homes-var e env)
  (match e
    [`(var ,e1) (lookup e1 env)]
    [`(movq ,e1 ,e2) `(movq ,(assign-homes-var e1 env) ,(assign-homes-var e2 env))]
    [`(negq ,e1) `(negq ,(assign-homes-var e1 env))]
    [`(addq ,e1 ,e2) `(addq ,(assign-homes-var e1 env) ,(assign-homes-var e2 env))]
    [else e]))

(define (assign-homes e)
  (let ([env (assign-homes-env (cadr e) -1)])
    (map (curryr assign-homes-var env) e)))

