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
    [var^ #:when(equal? var^ var) (call/cc (lambda (k) (k #t)))]
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
(define (decidable? instr env)
  ;(displayln "================================ im in decidable ===============================")
  ;(display "decidable? :") (displayln instr)
  ;(display "env: ") (displayln env)
  (define recur (lambda (x) (decidable? x env)))
  (match instr
    [(? fixnum?) #t]
    [(? boolean?) #t]
    [`(has-type ,e^ ,t) (recur e^)]
    [`(and (has-type #f Boolean) (has-type ,e2^ ,t2)) #t]
    [`(or (has-type #t Boolean) (has-type ,e2^ ,t2))  #t]
    [`(read) #f]
    [`(vector . ,arg) #f]
    [`(vector-set! ,vec ,i ,var) #f]
    [`(vector-ref . ,arg) #f]
    [(? symbol?) (not (equal? 'undecide (lookup instr env 'undecide)))]
    [`(+ ,e1 ,e2) (and (recur e1) (recur e2))]
    [`(- ,e1) (recur e1)]
    [`(eq? ,e1 ,e2) (and (recur e1) (recur e2))]
    [`(if ,c ,t ,e) (and (recur c) (recur t) (recur e))]
    [(? pair?) (foldr (lambda (x r) (and (recur x) r)) #t instr)]
    [else #t]))

(define (partial-helper instr env curdepth def-env in-def)
  ;; (display "curdepth: ") (displayln curdepth)
  ;; (display "env: ") (displayln env)
  ;; (display "in-def:") (displayln in-def)
  ;; (display "instr: ") (displayln instr)
  (define recur (lambda (x) (partial-helper x env (add1 curdepth) def-env in-def)))
  (if (> curdepth maxdepth)
      (list instr #f)
      (match instr
        [(? boolean?) (list instr #f)]
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
         ;; need to rebuild the environment, for now i just put null here, but this is not correct
         (match-define `(,eval-e ,changed) (partial-helper body '() (add1 curdepth) def-env #t))
         (list `(define (,funame . ,var-defs) : ,ret-type ,eval-e) changed)]
        
        ;; simple add, minus
        [`(+ (has-type (- (has-type ,e1^ Integer)) Integer)
             (has-type ,e2^ Integer)) #:when (and (fixnum? e1^) (fixnum? e2^))
         ;(displayln "imhere")
         (list `(has-type ,(- e2^ e1^) Integer) #t)]
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
        [`(+ (has-type ,e1^ Integer)
             (has-type (+ (has-type ,e2^ Integer) ,e3) Integer)) #:when (and (fixnum? e1^) (fixnum? e2^))
         (list `(has-type (+ (has-type ,(+ e1^ e2^) Integer) ,e3) Integer) #t)]
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
         ;(display "env: ") (displayln env)
         (define decide (decidable? conde env))
         ;(display "if (decidable? conde env) :") (displayln decide)
         (if decide
             (let ([c-r (recur conde)])
               ;(display "c-r: ") (displayln c-r)
               ;(display "else :") (displayln els)
               (if (cadar c-r)
                   (let ([t-r (recur thn)])
                     (list (car t-r) #t))
                   (let ([e-r (recur els)])
                     (list (car e-r) #t))))
             (let ([thnr (recur thn)]
                   [elsr (recur els)])
               (list `(if ,conde ,(car thnr) ,(car elsr)) (and (cadr thnr) (cadr elsr)))))]
        [`(eq? (has-type ,e1^ ,t1) (has-type ,e2^ ,t2))
         (let ([e1-r (recur e1^)]
               [e2-r (recur e2^)])
           ;(display "e1-r: ") (displayln e1-r)
           ;(display "e2-r: ") (displayln e2-r)
           (if (equal? (car e1-r) (car e2-r))
               (list #t #f)
               (list #f #f)))]
        [`(and (has-type #f Boolean) (has-type ,e2^ ,t2))
         (list #f #t)]
        [`(or (has-type #t Boolean) (has-type ,e2^ ,t2))
         (list #t #t)]
        
        
    ;;; let parameter estimate

        [`(let ([,var ,para]) ,body)
         ;(display "(var-occurs? var body): ") (displayln (var-occurs? var body))
         (if (decidable? para env)
             (if (var-occurs? var body)
                 (let* ([var-result (recur para)]
                        [eval-var (car var-result)]
                        [var-changed (cadr var-result)]
                        [new-env (cons `(,var . ,eval-var) env)])
                   (match-define `(,eval-e ,changed) (partial-helper body new-env (add1 curdepth) def-env in-def))
                   (list `(let ([,var ,eval-var]) ,eval-e) (or changed var-changed)))
                 (let ([result (recur body)])
                   (list (car result) #t)))
             
             (let ([result (recur body)])
               (list `(let ([,var ,para]) ,(car result)) (cadr result)))
             
             )]
        [`((has-type (lambda: ,vars : ,ret-type ,body) ,lambtype) . ,args)
         (let* ([eval-args (map car (map recur args))]
                [new-env (map (lambda (v^ a^)
                                `(,(car v^) . ,(cadr a^))) vars eval-args)])
           (partial-helper body (append new-env env) (add1 curdepth) def-env in-def))]

        ;;; function call -> function is in def env
        [`((has-type ,funame ,ftype) . ,args)#:when (and (lookup funame def-env #f) (not in-def))
         
         ;(displayln "============== im here ===================")
         
         ;(display "args: ") (displayln args)
         ;(display "(decidable? args env): ")(displayln (decidable? args env))
         ;(display "env: ") (displayln env)
         (define fdata (lookup funame def-env #f))
         (if (and (decidable? args env) (not in-def))
             (let* ([vars (map recur args)]
                    [fargs (car fdata)]
                    [fbody (cdr fdata)]
                    [new-env (map (lambda (x y) `(,x . ,(cadar y))) fargs vars)])
               ;(display "new-env: ") (displayln new-env)
               (match-define `((has-type ,expr ,t) ,changed) (partial-helper fbody (append new-env env) (add1 curdepth) def-env #t))
               ;(display "expr: ")(displayln expr)
               (list expr #t)) ;; then
             (list instr #f))]
        [`((has-type ,funame ,ftype) . ,args) #:when (lookup funame def-env #f)
         (if (decidable? args env)
             (let* ([var-results (map recur args)]
                    [var-instrs (map car var-results)]
                    [var-changed (foldr (lambda (x r) (and (cadr x) r)) #t var-results)])
               ;(displayln "is this wrong?")
               ;(display "var-cahnged: ") (displayln var-changed)
               (list `((has-type ,funame ,ftype) . ,var-instrs) var-changed))
             (list instr #f))]
        
        [else (list instr #f)]
        )))


(define (def-env-gen def)
  ; (display "definline: ") (displayln definline)
  (match def
    [`(define (,funame . ,var-defs) : ,ret-type ,body)
     `(,funame . (,(map car var-defs) . ,body))]))

(define (partial-with-level prog level)
  

  (match-define `(program ,ret-type . ,exprs) prog)
  (define defs (drop-right exprs 1))
  (define def-env (map def-env-gen defs))
  (define partial-result (map (lambda (x)
                                (partial-helper x '() 1 def-env #f)) exprs))
  ;(display "partial result: ") (displayln partial-result)
  (define result `(program ,ret-type ,@(map car partial-result)))
  (define changed (foldr (lambda (x r) (or x r)) #f (map cadr partial-result)))
  ;(display "cur-level: ") (displayln level)
  ;(display "result: ") (displayln result)
  ;(display "changed: ") (displayln changed)
  (if (and (> level 1) changed)
      (partial-with-level result (sub1 level))
      result))


(define (partial prog)
  (partial-with-level prog 5))


