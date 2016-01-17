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
    [`(assign ,var (- ,e1)) `((movq ,(select-instructions-assign e1) (var ,var)) (negq (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e1) `((addq ,(select-instructions-assign e2) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e2) `((addq ,(select-instructions-assign e1) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2)) `((movq ,(select-instructions-assign e1) (var ,var)) (addq ,(select-instructions-assign e2) (var ,var)))]
    [`(return ,e1) `((movq ,(select-instructions-assign e1) (reg rax)))]
    [`(assign ,var ,e1) `((movq ,(select-instructions-assign e1) (var ,var)))]
    [else `(,e)]))

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
  (let ([env (assign-homes-env (cadr e) -1)]
        [prog (car e)])
    `(,prog ,(length env) . ,(cddr (map (curryr assign-homes-var env) e)))))

(define (patch-instr-helper e)
  (match e
    [`(movq (stack ,e1) (stack ,e2)) `((movq (stack ,e1) (reg rax)) (movq (reg rax) (stack ,e2)))]
    [`(addq (stack ,e1) (stack ,e2)) `((movq (stack ,e1) (reg rax)) (addq (reg rax) (stack ,e2)))]
    [else `(,e)]))

(define (patch-instr e)
    (append-map patch-instr-helper e))

(define (print-helper e)
  (match e
    [`(stack ,e1) (format "~a(%rbp)" e1)]
    [`(int ,e1) (format "$~a" e1)]
    [`(reg ,e1) (format "%~a" e1)]
    [`(movq ,e1 ,e2) (format "movq	~s, ~s\n\t" (print-helper e1) (print-helper e2))]
    [`(negq ,e1) (format "negq	~s\n\t" (print-helper e1))]
    [`(addq ,e1 ,e2) (format "addq	~s, ~s\n\t" (print-helper e1) (print-helper e2))]
    ))

(define (print e)
  (string-append
 (format "	.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$~a, %rsp" (* 8 (cadr e)))
 (string-join (map print-helper (cddr e)))
 (format "	movq	%rax, %rdi
	callq	print_int
	addq	$~a, %rsp
	popq	%rbp
	retq" (* 8 (cadr e)))))
