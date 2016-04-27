#lang racket
(require "../common.rkt")

(provide partial)

;;; in this file we need to eval the s-expression


;;; define some simple example


;;; let para estimate : the var is not used in let expression

;;; let eval

;; if statement eval: the statement is true or false

;; + -, eval, some case read textbook

;;

;;

(define maxdepth 42)

(define (var-occurs? var instr)
  ;(display "var-occurs instr: ") (displayln instr)
  ;(display "var: ")(displayln var)
  (define recur (lambda (x) (var-occurs? var x)))
  (match instr
    [var^ #:when(equal? var^ var) #t]
    [`(has-type ,e1 ,t) #:when (equal? e1 var) (call/cc (lambda (k) (k #t)))]
    [`(has-type ,e1 ,t) (recur e1)]
    [`(let ((,var (has-type ,para ,var-t))) ,b) (or (recur para) (recur b))]
    [`(vector-set! ,e1 ,e2 ,e3) (or (recur e1) (recur e2) (recur e3))]
    [`(lambda: ,vars : ,ret ,b) (recur b)]
    [`(,op ,e1 ,e2) (or (recur e1) (recur e2))]
    [`(,op ,e1) (recur e1)]
    [`(if ,con ,thn ,els) (or (recur con) (recur thn) (recur els))]
    [else #f]))

;; if this need?
(define (decidable? instr)
  (match instr
    [`(has-type ,e^ ,t) (decidable? e^)]
    [`(read) #f]
    [`(vector-set! ,vec ,i ,var) #f]
    [(? pair?) (foldr (lambda (x r) (and (decidable? x) r)) #t instr)]
    [else #t]))

(define (partial-helper instr env curdepth)
  ;(display "curdepth: ") (displayln curdepth)
  ;(display "env: ") (displayln env)
  ;(display "instr: ") (displayln instr)
  (define recur (lambda (x) (partial-helper x env (add1 curdepth))))
  (if (> curdepth maxdepth)
      (list instr #f)
      (match instr
        [(? fixnum?) (list instr #f)]
        [(? symbol?) (if (lookup instr env #f)
                         (list (lookup instr env) #t)
                         (list instr #f))]
        [`(has-type ,e^ ,t)
         (match-define `(,eval-e ,changed) (recur e^))
         (if (and (pair? eval-e) (equal? (car eval-e) 'has-type))
             (list eval-e changed)
             (list `(has-type ,eval-e ,t) changed))]

        ;; define
        [`(define (,funame . ,var-defs) : ,ret-type ,body)
         (match-define `(,eval-e ,changed) (recur body))
         (list `(define (,funame . ,var-defs) : ,ret-type ,eval-e) changed)]
        
        ;; simple add, minus
        [`(+ (has-type ,e1^ Integer)
             (has-type (+ (has-type (read) Integer)
                 (has-type ,e2^ Integer)) Integer)) #:when (and (fixnum? e1^) (fixnum? e2^))
         (list `(has-type (+ (has-type (read) Integer)
                             (has-type ,(+ e1^ e2^) Integer)) Integer) #t)]
        [`(+ (has-type (+ (has-type (read) Integer)
                 (has-type ,e2^ Integer)) Integer) (has-type ,e1^ Integer)) #:when (and (fixnum? e1^) (fixnum? e2^))
         (list `(has-type (+ (has-type (read) Integer)
                            (has-type ,(+ e1^ e2^) Integer)) Integer) #t)]
        [`(+ (has-type ,e1^ Integer) (has-type ,e2^ Integer)) #:when (and (fixnum? e1^) (fixnum? e2^))
         (list `(has-type ,(+ e1^ e2^) Integer) #t)]
        [`(+ ,e1^ ,e2^)
         (match-define `(,e1 ,changed1) (recur e1^))
         (match-define `(,e2 ,changed2) (recur e2^))
         (list `(+ ,e1 ,e2) (or changed1 changed2))]
        [`(- ,e1)
         (match-define `(,eval-e ,changed) (recur e1))
         (list `(- ,eval-e) changed)
         ]

    ;;; if condition
        [`(if ,conde ,thn ,els)
         (if (decidable? conde)
             (if conde
                 (list thn #t)
                 (list els #t))
             (let ([thnr (recur thn)]
                   [elsr (recur els)])
               (list `(if ,conde ,(car thnr) ,(car elsr)) (and (cadr thnr) (cadr elsr)))))]
        
        
    ;;; let parameter estimate

        [`(let ([,var ,para]) ,body)
         ;(display "(var-occurs? var body): ") (displayln (var-occurs? var body))
         (if (decidable? para)
             (if (var-occurs? var body)
                 (let* ([var-result (recur para)]
                        [eval-var (car var-result)]
                        [var-changed (cadr var-result)]
                        [new-env (cons `(,var . ,eval-var) env)])
                   (match-define `(,eval-e ,changed) (partial-helper body new-env (add1 curdepth)))
                   (list `(let ([,var ,eval-var]) ,eval-e) (or changed var-changed)))
                 (let ([result (recur body)])
                   (list (car result) #t)))
             
             (let ([result (recur body)])
               (list `(let ([,var ,para]) ,(car result)) (cadr result)))
             
             )]
        [`((has-type (lambda: ,vars : ,ret-type ,body) ,lambtype) . ,args)
         ;; decideable?
         (let* ([eval-args (map car (map recur args))]
                [new-env (map (lambda (v^ a^)
                                `(,(car v^) . ,(cadr a^))) vars eval-args)])
           (partial-helper body (append new-env env) (add1 curdepth)))]
        [else (list instr #f)]
        )))


(define (partial-with-level prog level)
  
  
  (match-define `(program ,ret-type . ,exprs) prog)
  ;; (define defs (drop-right exprs 1))
  (define partial-result (map (lambda (x)
                                (partial-helper x '() 1)) exprs))
  ;(display "partial result: ") (displayln partial-result)
  (define result `(program ,ret-type ,@(map car partial-result)))
  (define changed (foldr (lambda (x r) (or x r)) #f (map cadr partial-result)))
  (display "cur-level: ") (displayln level)
  (display "result: ") (displayln result)
  ; (display "changed: ") (displayln changed)
  (if (and (> level 1) changed)
      (partial-with-level result (sub1 level))
      result))


(define (partial prog)
  (partial-with-level prog 5))

'(program (type Integer)
          (if (eq? 0 (read)) 41 42))
