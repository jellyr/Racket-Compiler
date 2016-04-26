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
        [(? symbol?) (list (lookup instr env instr) #f)]
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
        [`(if #f ,thn ,els) (list els #t)]
        [`(if #t ,thn ,els) (list thn #t)]
        
    ;;; let parameter estimate

        [`(let ([,var ,para]) ,body)
         (display "(var-occurs? var body): ") (displayln (var-occurs? var body))
         (if (var-occurs? var body)
             (let* ([var-result (recur para)]
                    [eval-var (car var-result)]
                    [var-changed (cadr var-result)]
                    [new-env (cons `(,var . ,eval-var) env)])
               (match-define `(,eval-e ,changed) (partial-helper body new-env (add1 curdepth)))
               (list `(let ([,var ,eval-var]) ,eval-e) (or changed var-changed)))
             (let ([result (recur body)])
               (list (car result) #t)))]
        [else (list instr #f)]
        )))


(define (partial-with-level prog level)
  
  
  (match-define `(program ,ret-type . ,exprs) prog)
  ;; (define defs (drop-right exprs 1))
  (define partial-result (map (lambda (x)
                                (partial-helper x '() 1)) exprs))
  ;; (display "partial result: ") (displayln partial-result)
  (define result `(program ,ret-type ,@(map car partial-result)))
  (display "cur-level: ") (displayln level)
  (display "result: ") (displayln result)
  (if (> level 1)
      (partial-with-level result (sub1 level))
      result))


(define (partial prog)
  (partial-with-level prog 2))
