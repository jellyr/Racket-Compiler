#lang racket
(require "../utilities.rkt")
(provide infer-program)



;;  - (== type1 type2)
;;  - ('implicit t1 t2 m)
;;  - ('explicit t1 t2)


(define var-cnt 0)
(define fun-env '())
(define (reset-var-cnt) (set! var-cnt 0))
(define (load-fun-env val) (set! fun-env (cons val fun-env)))


(struct infer-record ([assumptions #:mutable]
                      [contraints #:mutable]
                      type
                      type-expr)
  #:transparent
  #:guard (lambda (as con t te type-name)
            (cond
              [(not (set-mutable? as))
               (raise-syntax-error type-name "Assumptions: ~e has to be of the type mutable-set" as)]
              [(not (set-mutable? con))
               (raise-syntax-error type-name "Constraints: ~e has to be of the type mutable-set" con)]
              [else (values as con t te)])))

(define type_var? string?)
(define type_con? symbol?)
(define type_vector? (lambda (x)
                      (match x
                        [`(Vector . ,ls) #t]
                        [`(Vectorof ,ty) #t]
                        [else #f])))
(define (type_fun? type)
  (match type
    [`(-> . ,t1) #t]
    [else #f]))
(define (type_scheme? type) (and (pair? type) (eq? (car type) 'scheme)))


;; To create a fresh type variable for inference
(define (fresh var)
  (set! var-cnt (+ var-cnt 1))
  (string-append (if (symbol? var)
                     (symbol->string var)
                     (if (syntax? var) (symbol->string (syntax->datum var)) var))
                 (number->string var-cnt)))


(define (infer-types e env)
  (match e
    [`(define (,fname . ,fvars) ,b) (infer-def e env)]
    [`(lambda ,x ,b) (infer-abs e env)]
    [`(let ,vars ,b) (infer-let e env)]
    [(? symbol?) (infer-var e)]
    [`(if ,cnd ,thn ,els) (infer-cond e env)]
    [(or (? boolean?) (? fixnum?)) (infer-lit e)]
    [`(,rator . ,rand) (infer-app e env)]
    [else (raise-syntax-error 'infer-types "unhandled syntax: ~a" e)]))


;; (define (infer-array e env syn-table)
;;   (match-define `(acc-array ,ls) e)
;;   (let ([len (length ls)]
;;         [el (foldl (lambda (val res)
;;                    (match-define (infer-record a1 c1 t1 te1) (infer-types val env syn-table))
;;                    (match-define (infer-record a0 c0 t0 te0) res)
;;                    (set-union! a0 a1)
;;                    (set-union! c0 c1)
;;                    (infer-record a0
;;                                  c0
;;                                  (if (eqv? t0 'None) (list t1) (cons t1 t0))
;;                                  (cons te1 te0)))
;;                  (infer-record (mutable-set) (mutable-set) 'None '())
;;                  ls)])
;;     (match-define (infer-record a2 c2 t2 te2) el)
;;     (infer-record a2 c2 `(Array ,len ,(car t2)) `(acc-array ,(reverse te2)))))

(define (infer-cond e env)
  (match-define `(if ,cnd ,thn ,els) e)
  (match-define (infer-record ac cc tc tec) (infer-types cnd env))
  (match-define (infer-record at ct tt tet) (infer-types thn env))
  (match-define (infer-record ae ce te tee) (infer-types els env))
  (set-union! ac at ae)
  (set-union! cc ct ce (set `(== ,tt ,te)))
  (infer-record ac cc tt `(if ,tec ,tet ,tee)))

; [Var]
(define (infer-var x)
  (let ([simple-type (dict-ref environment x #f)]
        [def-type (dict-ref fun-env x #f)])
    (if simple-type
        (infer-record (mutable-set) (mutable-set) simple-type x)
        (if def-type
            (infer-record (mutable-set) (mutable-set) def-type `(has-type ,x ,def-type))
            (let ([var (fresh x)])
              (infer-record (mutable-set (cons x var))
                            (mutable-set)
                            var
                            `(has-type ,x ,var)))))))


; [Lit]
(define (infer-lit exp)
  (let ([t (match exp
             [(? number?) 'Integer]
             [(? boolean?) 'Boolean]
             [`(Array ,n ,el) `(Array n ,@(map infer-lit el))]
             [else (raise-syntax-error 'infer-lit "This literal is not supported yet: ~a " exp)])])
    (infer-record (mutable-set)
                  (mutable-set)
                  t
                  `(has-type ,exp ,t))))


; [App]
(define (infer-app exp env)
  (let ((e1 (car exp))
        (args (cdr exp))
        (typevar (fresh "app")))
    (match-define (infer-record a1 c1 t1 te1) (infer-types e1 env))
    (let ([te1 `(,te1)])
      (define argtypes
        (for/list [(arg args)]
          (match-define (infer-record a2 c2 t2 te2) (infer-types arg env))
          ;(set-add! a1 typevar)
          (set-union! a1 a2)
          (set-union! c1 c2)
          (set! te1 (append te1 `(,te2)))
          t2))
      (set-union! c1 (set `(== (-> ,@argtypes ,typevar) ,t1)))
      (infer-record a1 c1 typevar `(has-type ,te1 ,typevar)))))

(define (get-vars ls vars env)
  (cond
    [(< (length ls) 3) (list (append ls vars) env)]
    [else (if (eq? ': (cadr ls))
              (get-vars (cdddr ls)
                        (cons (car ls) vars)
                        (cons `(,(car ls) . ,(caddr ls)) env))
              (get-vars (cdr ls)
                        (cons (car ls) vars)
                        env))]))

; [Abs]
(define (infer-def exp env)
  (match-define `(define (,fname . ,fargs) ,body) exp)
  (let* ([arg-vals (get-vars fargs '() '())]
         [arg-env (map (lambda (arg)
                         (let ([env-val (assoc arg (last arg-vals))])
                          (cons arg (if env-val
                                        (cdr env-val)
                                        (fresh "arg"))))) (car arg-vals))]
         [arg-vars (map cdr arg-env)]
         [c (mutable-set)]
         [ares (mutable-set)])
    ;(displayln arg-env)
    ;(displayln arg-vars)
    (match-define (infer-record a c t e) (infer-types body (set-union env (list->set fargs))))
    ;(display a)
    ;(displayln c)
    (set-for-each a (lambda (y)
                      (let ((lkp (assoc (car y) arg-env)))
                        (if lkp
                            (set-add! c `(== ,(cdr y) ,(cdr lkp)))
                            (set-add! ares y)))))
    ;(displayln c)
    (set-union! ares a)
    (define ret-type `(-> ,@arg-vars ,t))
    (infer-record ares c ret-type `(define (,fname ,@(foldr (lambda (x res)
                                                              (cons `(,(car x) : ,(cdr x))
                                                                    res))
                                                            `()
                                                            arg-env)) : ,t ,e))))

; [Abs]
(define (infer-abs exp env)
  (match-define `(lambda ,args ,body) exp)
  (let* ([arg-vals (get-vars args '() '())]
         [arg-env (map (lambda (arg)
                         (let ([env-val (assoc arg (last arg-vals))])
                          (cons arg (if env-val
                                        (cdr env-val)
                                        (fresh "arg"))))) (car arg-vals))]
         [arg-vars (map cdr arg-env)]
         [c (mutable-set)]
         [ares (mutable-set)])
    ;(displayln arg-env)
    ;(displayln arg-vars)
    (match-define (infer-record a c t e) (infer-types body (set-union env (list->set args))))
    ;(display a)
    ;(displayln c)
    (set-for-each a (lambda (y)
                      (let ((lkp (assoc (car y) arg-env)))
                        (if lkp
                            (set-add! c `(== ,(cdr y) ,(cdr lkp)))
                            (set-add! ares y)))))
    ;(displayln c)
    (set-union! ares a)
    (define ret-type `(-> ,@arg-vars ,t))
    (infer-record ares c ret-type `(has-type (lambda ,(foldr (lambda (x res)
                                                               (cons `(,(list (car x) ': (cdr x))) res))
                                                             '()
                                                             arg-env) : ,t ,e) ,ret-type))))

; [Let]
(define (infer-let exp env)
  (match-define `(let ,vars ,body) exp)
  (match-define (infer-record a1 c1 t1 te1)
    (foldl (lambda (var res)
             (match var
               [`(,x ,e1)
                (match-define (infer-record a c t te) (infer-types e1 env))
                ;(displayln (list a c t te))
                (match-define (infer-record ares cres tres te-res) res)
                (set-union! ares a)
                (set-union! cres c)
                (infer-record ares cres tres (append `([,(list x ': t) ,te]) te-res))]
               [`(,x : ,t0 ,e1)
                (match-define (infer-record a c t te) (infer-types e1 env))
                (match-define (infer-record ares cres tres te-res) res)
                (set-union! ares a)
                (set-union! cres c (set `(== ,t0 ,t)))
                (infer-record ares cres tres (append `([,(list x ': t) ,te]) te-res))]))
           (infer-record (mutable-set) (mutable-set) 'None '()) vars))
  ;;(match-define (infer-record a1 c1 t1 te1) (infer-types e1 env))
  (match-define (infer-record a2 c2 t2 te2) (infer-types body env))
  ;(set-union! a1 a2)
  (set-union! c1 c2)
  (set-for-each a2 (lambda (a)
                     (let ([aval (assoc (car a) (map (lambda (var)
                                                       (match-let ((`((,x : ,t) ,b) var))
                                                         (list x t))) te1))])
                       (if aval
                           (set-add! c1 `(implicit ,(cdr a) ,(last aval) ,env))
                           (set-add! a1 a)))))
  (infer-record a1 c1 t2 `(has-type (let ,te1 ,te2) ,t2)))



(define (solve constraints)
  (cond
    [(empty? constraints) '()]
    [else (let ((constraint (car constraints)))
            (match constraint
              [`(== ,t1 ,t2) (let ((s (unify t1 t2)))
                               (subs-union (solve (map (curry substitute_const s) (cdr constraints))) s))]
              [`(implicit ,t1 ,t2 ,monos) (if (set-empty? (set-intersect
                                                           (set-subtract (free_vars t2) monos)
                                                           (active_vars (cdr constraints))))
                                              (solve (cons `(explicit ,t1 ,(generalize monos t2))
                                                           (cdr constraints)))
                                              (solve (append (cdr constraints) `(,constraint))))]
              [`(explicit ,t ,s) (solve (cons `(== ,t ,(instantiate s)) (cdr constraints)))]))]))



(define (generalize monos type)
  (display "Generalize :")
  (displayln (list 'scheme (set-subtract (free_vars type) monos) type))
  (list 'scheme (set-subtract (free_vars type) monos) type))


(define (instantiate scheme)
  (match-define `(scheme ,qs ,type) scheme)
  (substitute (for/list ([q qs]) (cons q (fresh "I"))) type))

(define (subs-union subs1 subs2)
  (let ((s (map (lambda (v)
                  (cons (car v) (substitute subs1 (cdr v)))) subs2)))
    (foldl (lambda (v res)
             (when (dict-ref subs2 (car v) #f)
               (error 'subs-union "Substitutions with same type vars"))
             (set! s (cons v s))) '() subs1) s))


(define (substitute s type)
  (cond
    [(type_con? type) type]
    [(type_var? type) (dict-ref s type type)]
    [(type_vector? type) `(,(car type) ,@(map (curry substitute s) (cdr type)))]
    [(type_fun? type) `(-> ,@(map (curry substitute s) (cdr type)))]
    [else (error 'substitute (format "unknown type: ~a" type))]))



(define (substitute_const s constraint)
  (match constraint
    [`(== ,v1 ,v2) `(== ,(substitute s v1) ,(substitute s v2))]
    [`(implicit ,v1 ,v2 ,v3) `(implicit
                               ,(substitute s v1)
                               ,(substitute s v2)
                               ,(for/set ([var v3])
                                  (dict-ref s var var)))]
    [`(explicit ,v1 ,v2) `(explicit ,(substitute s v1) ,(substitute s v2))]))


;; Fetches all the variables in the input
(define (free_vars t)
  (cond [(type_var? t) (set t)]
        [(type_fun? t) (let ([in-types (drop-right (cdr t) 1)]
                             [ret-type (last t)])
                         (set-union (list->set (map free_vars in-types))
                                    (free_vars ret-type)))]
        [(type_con? t) (set)]
        [else (error 'free_vars (format "Unknown type ~s" t))]))


(define (active_vars constraints)
  (foldl (lambda (constraint res)
           (match constraint
             [`(== ,v1 ,v2) (set-union (free_vars v1) (free_vars v2) res)]
             [`(implicit ,v1 ,v2 ,v3) (set-union (free_vars v1) (set-intersect v3 (free_vars v2)) res)]
             [`(explicit ,v1 ,v2) (set-union (free_vars v1) (free_vars v2) res)]))
         (set) constraints))


(define (unify t1 t2)
  (cond
    [(and (pair? t1) (pair? t2))
     (match-let ((`(-> . ,t1-types) t1)
                 (`(-> . ,t2-types) t2))
       (if (not (eq? (length t1-types) (length t2-types)))
           (error "Types ~a and ~a are incompatible" t1 t2)
           (foldl (lambda (p1 p2 s)
                    (set-union (unify (substitute s p1) (substitute s p2)) s))
                  '() t1-types t2-types)))]
    [(equal? t1 t2) '()]
    [(type_var? t1) (occurs-check t1 t2)]
    [(type_var? t2) (occurs-check t2 t1)]
    [else (error 'unify (format "Can not Unify t1: ~s and t2: ~s" t1 t2))]))


(define (occurs-check var type)
  (cond
    [(equal? var type) '()]
    ;This is an infinite type. Send an error back
    [(set-member? (free_vars type) var)
     (error 'occurs-check "Infinite Type. ~a occurs in ~a\n" var type)]
    [else `(,(cons var type))]))

(define environment
  '((+ . (-> Integer Integer Integer))
    (- . (-> Integer Integer))
    (< . (-> "t5" "t5" Boolean))
    (eq? . (-> "t7" "t7" Boolean))))

(define (str->sym val)
  (match val
    [(? string?)
     ;;(string->symbol val)
     'All]
    [(? list?) (map str->sym val)]
    [else val]))
          
(define (annotate-type ty subs)
  (match ty
    [`(-> . ,types) (let ([ty-vals (map (curryr annotate-type subs) types)])
                      `(,@(drop-right ty-vals 1) -> ,(last ty-vals)))]
    [else (let ([f (assoc ty subs)])
            (if f
                ;;(str->sym (cdr f))
                (cdr f)
                ;;(str->sym ty)
                ty))]))

(define (annotate-expr type-expr subs)
  (match type-expr
    [`(has-type ,expr ,ty) `(has-type ,(annotate-expr expr subs) ,(annotate-type ty subs))]
    [x #:when (or (symbol? x) (number? x) (boolean? x)) type-expr]
    [`(,x : ,ty) `(,(annotate-expr x subs) : ,(annotate-type ty subs))]
    [`(define (,fname . ,fvars) : ,ty ,b) `(define (,fname ,@(foldr (lambda (val res)
                                                                     (cons (annotate-expr val subs) res))
                                                                   '() fvars))
                                           : ,(annotate-type ty subs) ,(annotate-expr b subs))]
    [`(lambda ,x : ,ty ,b) `(lambda: ,(foldr (lambda (val res)
                                             (append (annotate-expr val subs) res)) '() x)
                             : ,(annotate-type ty subs)
                             ,(annotate-expr b subs))]
    [`(let ,vars ,b) `(let ,(foldr (lambda (var res)
                                     (match-let ([`(,x ,e) var])
                                       (append `((,@(annotate-expr x subs)
                                                  ,(annotate-expr e subs))) res))) '() vars)
                        ,(annotate-expr b subs))]
    [`(,rator . ,rand) `(,(annotate-expr rator subs)
                         ,@(map (curryr annotate-expr subs) rand))]
    [else (error 'annotate-expr (format "Error with type ~a" type-expr))]))


(define (infer-program exp)
  (reset-var-cnt)
  (match-define (infer-record fun-as fun-con fun-type fun-ty-ex)
    (foldl (lambda (bl res)
             (match-define `(define (,fname . ,vars) ,body) bl)
             (match-let* (((infer-record as con ty ty-ex) (infer-types bl (set)))
                          ((infer-record ra rc rt rex) res))
               (set-union! ra as)
               (set-union! rc con)
               (load-fun-env `(,fname . ,ty))
               (set! rex (cons ty-ex rex))
               (infer-record ra rc 'None rex)))
             (infer-record (mutable-set) (mutable-set) 'None '())
             (drop-right exp 1)))
  ;(displayln fun-ty-ex)
  ;(displayln fun-env)
  (match-define (infer-record assumptions constraints type type-expr) (infer-types (last exp) (set)))
  (set-union! constraints fun-con)
  (set! type-expr `(,@fun-ty-ex ,type-expr))
  (define substitutions (solve (set->list constraints)))
  ;;(displayln type-expr)
  ;;(displayln "--- Input: ------------------------------------------------------")
  ;;(displayln exp)
  ;;(displayln "--- Output: -----------------------------------------------------")  
  ;;(displayln (list (syntax->datum exp) ':= (substitute substitutions type)))
  ;;(displayln "--- Principal type of Expression: -------------------------------")
  ;;(displayln (str->sym (substitute substitutions type)))
  ;;(displayln "--- Type Annotated Expression: ----------------------------------")
  ;;(displayln (map (curryr annotate-expr substitutions) type-expr))
  ;;(displayln "-----------------------------------------------------------------")
  ;; (values (substitute substitutions type)
  ;;         (annotate-expr type-expr substitutions))
  `(program (type ,(substitute substitutions type)) ,@(str->sym (annotate-expr type-expr substitutions)))
  )

;; ========================================================================
;; Test Cases
;; ========================================================================

(define e1 'x)
(define e2 '(lambda (x) x))
(define e3 '(x 2))
(define e4 '(((lambda (x y) (+ x y)) 2 3)))
(define e5 '(let ((x (+ 5 2))) x))
(define e6 '(let ((x 2) (y 5)) (+ x y)))
(define e7 '((lambda (x) (let ((x x)) x)) 2))
(define e8 '(lambda (x) (let ((x 2)) (+ x x))))
(define e9 '((lambda (z) (lambda (x z) (let ((x 2)) (+ x z)))) 5))
(define e10 '(let ((x (lambda (x y) (+ x y)))) (x 5 2)))
(define e11 '(let ((f (lambda (x) (lambda (y) (+ x 1)))))
                (let ((g (f 2))) g)))
(define e12 '(if #f
                 (+ 3 5)
                 (- 5 3)))
(define e13 '((define (id x y) x) (id 42 #f)))



;; (p-infer e1 (box '()))

;; (p-infer e2 (box '()))
;;(infer-program e4)
;; ;;(p-infer #`#,(p-infer e4 (box '())) (box '()))
;;(infer-program e2)
;; ;;(p-infer #`#,(p-infer e6 '()) '())
;; (p-infer e7 (box '()))
;; (p-infer e8 (box '()))
;;(p-infer e9 (box '()))
;; ;;(p-infer #`#,(p-infer e9 '()) '())
;; (p-infer e10 (box '()))
;; (p-infer e11 (box '()))
;; (p-infer e12 (box '()))
;;(infer-program e13)
;; (p-infer e14 (box '()))
;; (p-infer e15 (box '()))
;; (p-infer e16 (box '()))
;; (p-infer e17 (box '()))
;; (p-infer e18 (box '()))
;; (p-infer e19 (box '()))
;; (p-infer e20 (box '()))

;;(display "Feeding back through:\n")
;; (p-infer e2_)
;; Expected output:
;;  (lambda ((x:t1)) x)

