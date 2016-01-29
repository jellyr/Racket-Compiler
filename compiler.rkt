#lang racket
(require racket/fixnum)
(require racket/set)
(require "interp.rkt")
(require "utilities.rkt")

(provide r1-passes)



(define (int? e)
  (eqv? (car e) 'int))

(define (var? e)
  (eqv? (car e) 'var))

(define (reg? e)
  (eqv? (car e) 'reg))

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



(define (select-instructions-assign ret-v e)
  (match e
    [(? fixnum?) `(int ,e)]
    [(? symbol?) #:when (not (eq? e 'program)) `(var ,e)]
    [`(assign ,var (read)) (let ([vare (if (eqv? var ret-v) '(reg rax) `(var ,var))])
                             `((callq read_int) (movq (reg rax) ,vare)))]
    [`(assign ,var (- ,e1)) (let ([vare (if (eqv? var ret-v) '(reg rax) `(var ,var))])
                              `((movq ,(select-instructions-assign ret-v e1) ,vare) (negq ,vare)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e1) (let ([vare (if (eqv? var ret-v) '(reg rax) `(var ,var))])
                                                     `((addq ,(select-instructions-assign ret-v e2) ,vare)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e2) (let ([vare (if (eqv? var ret-v) '(reg rax) `(var ,var))])
                                                     `((addq ,(select-instructions-assign ret-v e1) ,vare)))]
    [`(assign ,var (+ ,e1 ,e2)) (let ([vare (if (eqv? var ret-v) '(reg rax) `(var ,var))])
                                  `((movq ,(select-instructions-assign ret-v e1) ,vare) (addq ,(select-instructions-assign ret-v e2) ,vare)))]
    [`(return ,e1) (if (fixnum? ret-v) `((movq ,(select-instructions-assign ret-v e1) (reg rax))) `())]
    ;; 
    [`(assign ,var ,e1) (let ([vare (if (eqv? var ret-v) '(reg rax) `(var ,var))])
                          `((movq ,(select-instructions-assign ret-v e1) ,vare)))]
    [(? list?) (list (remove ret-v e))]
    [else `(,e)]))


(define (select-instructions e)
  (let ([ret-var (last (last e))])
    (append-map (curry select-instructions-assign ret-var) e)))

;;;;;;
(define (uncover-live-unwrap e)
  (match e
    [`(var ,e1) (set e1)]
    [`(reg ,r) (set r)]
    [else (set)]))

;; lak ---> lak-1
;; lak is a set
(define (uncover-live-helper e lak)
  (match e
    [`(movq ,e1 ,e2) (set-union (set-subtract lak (uncover-live-unwrap e2)) (uncover-live-unwrap e1))]
    ; [`(negq ,e1) ()]
    [`(addq ,e1 ,e2) (set-union (set-union lak (uncover-live-unwrap e2)) (uncover-live-unwrap e1))]
    [`(subq ,e1 ,e2) (set-union (set-union lak (uncover-live-unwrap e2)) (uncover-live-unwrap e1))]
    [else (uncover-live-unwrap e)]))

(define (uncover-live e)
  (let [(setlist (map set->list
                      (foldr (lambda (x r)
                               (cons (uncover-live-helper x (car r)) r))
                             `(,(set))
                             (cddr e))))]
    `(,(car e) ,(list (cadr e) (cdr setlist)) ,@(cddr e))))

;;;;;;;;;;

(define (build-interference-unwrap e)
  (match e
    [`(var ,e1) e1]
   ; [`(reg ,r) r] ;; removed rax from interference graph
    [else e]))

(define (build-interference-helper graph e lak)
  (match e
    [`(movq ,e1 ,e2)#:when (or (var? e2) (reg? e2))
     (let ([s (build-interference-unwrap e1)]
           [d (build-interference-unwrap e2)])
       (map (lambda (v) (cond
                          [(not (or (eqv? s v) (eqv? d v))) (add-edge graph d v)])) lak))]
    [`(addq ,e1 ,e2)#:when (or (var? e2) (reg? e2))
      (let ([s (build-interference-unwrap e1)]
            [d (build-interference-unwrap e2)])
        (map (lambda (v) (cond
                           [(not (eqv? d v)) (add-edge graph d v)])) lak))]
    [`(callq ,label) (map (lambda (v1) (map (lambda (v2) (add-edge graph v1 v2)) (set->list caller-save)))
                          lak)]
    [else '()]))

(define (build-interference e)
  (let* ([lak (cadadr e)]
         [instr (cddr e)]
         [graph (make-graph '())])
    (map (curry build-interference-helper graph) instr lak)
    `(,(car e) (,(caadr e) ,graph) ,@instr)))



;; ol : ommit list
(define (highest-saturation graph ol)
  (foldr (lambda (v r)
           (if (and (> (set-count (cdr v)) (set-count (cdr r)))
                    (not (findf (lambda (val) (eq? (car v) val)) ol)))
               v
               r)) (cons 'none (set)) (hash->list graph)))

;; optimial with cps;;
;; vector->list general-registers
;; we can make colorvars a lazylist if we want
(define (assign-minicolor node constrain-graph)
  (define colorvals (range 13))
  (define contrains (hash-ref! constrain-graph (car node) (set)))
  (car (dropf colorvals (curry set-member? contrains))))




;; make nicerrrrr
(define (allocate-registers-helper graph assign-list constrain-graph)
  (let* ([node (highest-saturation graph (map car assign-list))]
         [minvalue (assign-minicolor node constrain-graph)])
    (cond
      ((eq? 'none (car node)) assign-list)
      (else (allocate-registers-helper graph
                                       `((,(car node) . ,minvalue) . ,assign-list)
                                       (foldl (lambda (gr res)
                                                (hash-set! res gr (set-add (hash-ref! res gr (set)) minvalue))
                                                res) constrain-graph (set->list (cdr node))))))))

;; stacki = -1 ;
(define (allocate-reg-stack graph)
  
  )

(define (allocate-var e env)
  (match e
    [`(var ,e1) (lookup e1 env)]
    [`(movq ,e1 ,e2) `(movq ,(allocate-var e1 env) ,(allocate-var e2 env))]
    [`(negq ,e1) `(negq ,(allocate-var e1 env))]
    [`(addq ,e1 ,e2) `(addq ,(allocate-var e1 env) ,(allocate-var e2 env))]
    [else e]))

(define (allocate-registers e)
  (let ([assign-list (allocate-register-helper (caddr e) '() (make-graph '()))]
        [env (allocate-registers-env assign-list)]
        [prog (car e)])
    `(,prog ,(* 8 (length (cadr e))) . ,(cddr (map (curryr allocate-var env) e)))))

; starti == -1
;; (define (assign-homes-env alist starti)
;;   (cond
;;     [(null? alist) '()]
;;     [else (append `((,(car alist) . (stack ,(* 8 starti)))) (assign-homes-env (cdr alist) (sub1 starti)))]))

(define (patch-instr-helper e)
  (match e
    [`(movq (stack ,e1) (stack ,e2)) `((movq (stack ,e1) (reg rax)) (movq (reg rax) (stack ,e2)))]
    [`(addq (stack ,e1) (stack ,e2)) `((movq (stack ,e1) (reg rax)) (addq (reg rax) (stack ,e2)))]
    [else `(,e)]))

(define (patch-instructions e)
    (append-map patch-instr-helper e))

(define (print-helper e)
  (match e
    [`(stack ,e1) (format "~a(%rbp)" e1)]
    [`(int ,e1) (format "$~a" e1)]
    [`(reg ,e1) (format "%~a" e1)]
    [`(movq ,e1 ,e2) (string-append "movq	" (print-helper e1) ", " (print-helper e2)" \n\t")]
    [`(negq ,e1) (string-append "negq	" (print-helper e1) " \n\t" )]
    [`(callq ,e1) (string-append "callq	" (print-helper e1) " \n\t" )]
    [`(addq ,e1 ,e2) (string-append "addq	" (print-helper e1) ", " (print-helper e2) " \n\t")]
    [else (format "~s" e)]
    ))

(define (print-x86 e)
  (string-append
 (format "	.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$~a, %rsp\n\t" (* 8 (cadr e)))
 (string-join (map print-helper (cddr e)))
 (format "	movq	%rax, %rdi
	callq	print_int
	addq	$~a, %rsp
	popq	%rbp
	retq" (* 8 (cadr e)))))

(define r1-passes `(("uniquify" ,(uniquify '()) ,interp-scheme)
                    ("flatten" ,flatten ,interp-C)
                    ("select instructions" ,select-instructions ,interp-x86)
                    ("uncover-live" ,uncover-live ,interp-x86)
                    ;("assign homes" ,assign-homes ,interp-x86)
                    ("patch instructions" ,patch-instructions ,interp-x86)
                    ("print x86" ,print-x86 #f)))
