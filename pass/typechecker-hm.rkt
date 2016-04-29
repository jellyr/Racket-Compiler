#lang racket
(require "../utilities.rkt")
(provide infer-program)


(define var-cnt 0)
(define fun-env '())
(define vec-env '())
(define (reset-var-cnt) (set! var-cnt 0))
(define (reset-fun-env) (set! fun-env '()))
(define (reset-vec-env) (set! vec-env '()))
(define (load-fun-env val) (set! fun-env (cons val fun-env)))
(define (load-vec-env val) (set! vec-env (cons val vec-env)))

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
                        [else #f])))
(define (type_fun? type)
  (match type
    [`(-> . ,t1) #t]
    [else #f]))

;; To create a fresh type variable for inference
(define (fresh var)
  (set! var-cnt (+ var-cnt 1))
  (string-append (if (symbol? var)
                     (symbol->string var)
                     (if (syntax? var) (symbol->string (syntax->datum var)) var))
                 (number->string var-cnt)))


(define (infer-types e env)
  (match e
    [`(,def-type (,fname . ,fvars) ,b) #:when (or (eq? def-type 'define)
                                                  (eq? def-type 'define-inline)) (infer-def e env)]
    [`(lambda ,x ,b) (infer-abs e env)]
    [`(let ,vars ,b) (infer-let e env)]
    [`(vector . ,el) (infer-vector e env)]
    [`(vector-ref ,v ,n) (infer-vref e env)]
    [`(vector-set ,v ,n ,el) (infer-vset e env)]
    [`(if ,cnd ,thn ,els) (infer-cond e env)]
    [(? symbol?) (infer-var e)]
    [(or (? boolean?) (? fixnum?)) (infer-lit e)]
    [`(read) (infer-record (mutable-set) (mutable-set) 'Integer `(has-type (read) Integer))]
    [`(void) (infer-record (mutable-set) (mutable-set) 'Integer `(has-type (void) Integer))]
    [`(,rator . ,rand) (infer-app e env)]
    [else (raise-syntax-error 'infer-types "unhandled syntax: ~a" e)]))


(define (infer-vector e env)
  (match-define `(vector . ,el) e)
  (match-define (infer-record a2 c2 t2 te2) (foldr (lambda (val res)
                                                     (match-define (infer-record a1 c1 t1 te1) (infer-types val env))
                                                     (match-define (infer-record a0 c0 t0 te0) res)
                                                     (set-union! a0 a1)
                                                     (set-union! c0 c1)
                                                     (infer-record a0
                                                                   c0
                                                                   (cons t1 t0)
                                                                   (cons te1 te0)))
                                                   (infer-record (mutable-set) (mutable-set) '() '()) el))
  (define vec-type `(Vector ,@t2))
  (infer-record a2 c2 vec-type `(has-type (vector ,@te2) ,vec-type)))

(define (infer-vref e env)
  (match-define `(vector-ref ,vec ,idx) e)
  (match-define (infer-record va vc vt vex) (infer-types vec env))
  (match-define (infer-record ia ic it iex) (infer-types idx env))
  (set-union! va ia)
  (set-union! vc ic (set `(== ,it Integer)))
  (define ret-type (list-ref vt (add1 idx)))
  (infer-record va vc ret-type `(has-type (vector-ref ,vex ,iex) ,ret-type)))

(define (infer-vset e env)
  ;;FIXME : Implement Me
  (infer-record (mutable-set) (mutable-set) '() '()) )

(define (infer-cond e env)
  (match-define `(if ,cnd ,thn ,els) e)
  (match-define (infer-record ac cc tc tec) (infer-types cnd env))
  (match-define (infer-record at ct tt tet) (infer-types thn env))
  (match-define (infer-record ae ce te tee) (infer-types els env))
  (set-union! ac at ae)
  (set-union! cc ct ce (set `(== ,tt ,te)))
  (infer-record ac cc tt `(has-type (if ,tec ,tet ,tee) ,tt)))

; [Var]
(define (infer-var x)
  (let ([simple-type (dict-ref environment x #f)]
        [def-type (dict-ref fun-env x #f)]
        [vec-type (dict-ref vec-env x #f)])
    (cond
      [simple-type (infer-record (mutable-set) (mutable-set) simple-type x)]
      [def-type (infer-record (mutable-set) (mutable-set) def-type `(has-type ,x ,def-type))]
      [vec-type (infer-record (mutable-set) (mutable-set) vec-type `(has-type ,x ,vec-type))]
      [else (let ([var (fresh x)])
              (infer-record (mutable-set (cons x var))
                            (mutable-set)
                            var
                            `(has-type ,x ,var)))])))


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
          (set-union! a1 a2)
          (set-union! c1 c2)
          (set! te1 (append te1 `(,te2)))
          t2))
      (set-union! c1 (set `(== ,t1 (-> ,@argtypes ,typevar))))
      (infer-record a1 c1 typevar `(has-type ,te1 ,typevar)))))


; [Abs]
(define (infer-def exp env)
  (match-define `(,def-type (,fname . ,fargs) ,body) exp)
  (let* ([arg-env (map (lambda (arg)
                         (cons arg (fresh "arg"))) fargs)]
         [arg-vars (map cdr arg-env)]
         [ares (mutable-set)])
    (match-define (infer-record a c t e) (infer-types body (set-union env (list->set fargs))))
    (set-for-each a (lambda (y)
                      (let ((lkp (assoc (car y) arg-env)))
                        (if lkp
                            (set-add! c `(== ,(cdr y) ,(cdr lkp)))
                            (set-add! ares y)))))
    (set-union! ares a)
    (define ret-type `(-> ,@arg-vars ,t))
    (infer-record ares c ret-type `(,def-type (,fname ,@(foldr (lambda (x res)
                                                              (cons `(,(car x) : ,(cdr x))
                                                                    res))
                                                            `()
                                                            arg-env)) : ,t ,e))))

; [Abs]
(define (infer-abs exp env)
  (match-define `(lambda ,args ,body) exp)
  (let* ([arg-env (map (lambda (arg)
                         (cons arg (fresh "arg"))) args)]
         [arg-type (map cdr arg-env)]
         [c (mutable-set)]
         [ares (mutable-set)])
    (match-define (infer-record a c t e) (infer-types body (set-union env (list->set args))))
    (set-for-each a (lambda (y)
                      (let ((lkp (assoc (car y) arg-env)))
                        (if lkp
                            (set-add! c `(== ,(cdr y) ,(cdr lkp)))
                            (set-add! ares y)))))
    (set-union! ares a)
    (define ret-type `(-> ,@arg-type ,t))
    (infer-record ares c ret-type `(has-type (lambda ,(foldr (lambda (x res)
                                                               (cons `(,(car x) : ,(cdr x)) res))
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
                (if (type_vector? t)
                    (load-vec-env `(,x . ,t))
                    (void))
                (match-define (infer-record ares cres tres te-res) res)
                (set-union! ares a)
                (set-union! cres c)
                (set! tres (cons `(,x ,t) tres))
                (infer-record ares cres tres (append `([,x ,te]) te-res))]))
           (infer-record (mutable-set) (mutable-set) '() '()) vars))
  (match-define (infer-record a2 c2 t2 te2) (infer-types body env))
  (set-union! c1 c2)
  (set-for-each a2 (lambda (a)
                     (let ([aval (assoc (car a) t1)])
                       (if aval
                           (set-add! c1 `(== ,(cdr a) ,(last aval)))
                           (set-add! a1 a)))))
  (infer-record a1 c1 t2 `(has-type (let ,te1 ,te2) ,t2)))



(define (solve constraints)
  (cond
    [(empty? constraints) '()]
    [else (let ((constraint (car constraints)))
            (match constraint
              [`(== ,t1 ,t2) (let ((s (unify t1 t2)))
                               (subs-union (solve (map (curry substitute_const s) (cdr constraints))) s))]))]))


(define (subs-union subs1 subs2)
  (let ((s (map (lambda (v)
                  (cons (car v) (substitute subs1 (cdr v)))) subs2)))
    (foldl (lambda (v res)
             (when (dict-ref subs2 (car v) #f)
               (error 'subs-union (format "Substitutions with same type vars ~a ~a" subs1 subs2)))
             (set! s (cons v s))) '() subs1) s))

(define (substitute s type)
  (match type
    [(? type_con?) type]
    [(? type_var?) (walk type s)]
    [(? type_vector?) `(,(car type) ,@(map (curry substitute s) (cdr type)))]
    [(? type_fun?) `(-> ,@(map (curry substitute s) (cdr type)))]
    [else (error 'substitute (format "unknown type: ~a" type))]))


(define (substitute_const s constraint)
  (match constraint
    [`(== ,v1 ,v2) `(== ,(substitute s v1) ,(substitute s v2))]))

;; Fetches all the variables in the input
(define (free_vars t)
  (cond [(type_var? t) (set t)]
        [(type_fun? t) (let ([in-types (drop-right (cdr t) 1)]
                             [ret-type (last t)])
                         (foldr (lambda (v res)
                                  (set-union v res))
                                (free_vars ret-type)
                                (map free_vars in-types)))]
        [(type_con? t) (set)]
        [else (error 'free_vars (format "Unknown type ~s" t))]))


(define (active_vars constraints)
  (foldl (lambda (constraint res)
           (match constraint
             [`(== ,v1 ,v2) (set-union (free_vars v1)
                                       (free_vars v2)
                                       res)]))
         (set) constraints))

(define walk
  (lambda (x s)
    (let ([val (assq x s)])
      (cond
       [(not val) x]
       [(type_var? (cdr val)) (walk (cdr val) s)]
       [else (cdr val)]))))

(define (unify t1 t2)
  (cond
    [(and (pair? t1) (pair? t2))
     (match-let ((`(-> . ,t1-types) t1)
                 (`(-> . ,t2-types) t2))
       (if (not (eq? (length t1-types) (length t2-types)))
           (error (format "Types ~a and ~a are incompatible" (infix-fun-type t1) (infix-fun-type t2)))
           (foldl (lambda (p1 p2 s)
                    (set-union (unify (substitute s p1) (substitute s p2)) s))
                  '() t1-types t2-types)))]
    [(equal? t1 t2) '()]
    [(type_var? t1) (occurs-check t1 t2)]
    [(type_var? t2) (occurs-check t2 t1)]
    [else (error 'unify (format "Can not Unify t1: ~s and t2: ~s" (infix-fun-type t1) (infix-fun-type t2)))]))


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
    (and . (-> Boolean Boolean Boolean))
    (not . (-> Boolean Boolean))
    (eq? . (-> "t7" "t7" Boolean))))

(define (str->sym val)
  (match val
    [(? string?)
     (string->symbol val)
     ]
    [(? list?) (map str->sym val)]
    [else val]))
          
(define (annotate-type ty subs)
  (match ty
    [`(-> . ,types) (let ([ty-vals (map (curryr annotate-type subs) types)])
                      `(,@(drop-right ty-vals 1) -> ,(last ty-vals)))]
    [else (let ([f (assoc ty subs)])
            (if f
                (annotate-type (cdr f) subs)
                ty))]))

(define (annotate-expr type-expr subs)
  (match type-expr
    [`(has-type ,expr ,ty) `(has-type ,(annotate-expr expr subs) ,(annotate-type ty subs))]
    [`(lambda ,x : ,ty ,b) `(lambda: ,(map (curryr annotate-expr subs) x) 
                              : ,(annotate-type ty subs)
                              ,(annotate-expr b subs))]
    [`(let ,vars ,b) `(let ,(foldr (lambda (var res)
                                     (match-let ([`(,x ,e) var])
                                       (append `((,x
                                                  ,(annotate-expr e subs))) res))) '() vars)
                        ,(annotate-expr b subs))]
    [`(,def-type (,fname . ,fvars) : ,ty ,b)
     #:when (or (eq? def-type 'define)
                (eq? def-type 'define-inline))
     `(,def-type (,fname . ,(map (curryr annotate-expr subs) fvars))
        : ,(annotate-type ty subs) ,(annotate-expr b subs))]
    [`(,x : ,ty) `(,(annotate-expr x subs) : ,(annotate-type ty subs))]
    [`(,rator . ,rand) `(,(annotate-expr rator subs)
                         ,@(map (curryr annotate-expr subs) rand))]
    [x #:when (or (symbol? x) (number? x) (boolean? x)) type-expr]
    [else (error 'annotate-expr (format "Error with type ~a" type-expr))]))

(define (infix-fun-type ty)
  (match ty
    [`(-> . ,types) `(,@(infix-fun-type (drop-right types 1)) -> ,(infix-fun-type (last types)))]
    [else ty]))

(define (infer-program exp)
  (reset-var-cnt)
  (reset-fun-env)
  (reset-vec-env)
  (match-define (infer-record fun-as fun-con fun-type fun-ty-ex)
    (foldr (lambda (bl res)
             (match-define `(,def-type (,fname . ,vars) ,body) bl)
             (match-let* (((infer-record as con ty ty-ex) (infer-types bl (set)))
                          ((infer-record ra rc rt rex) res))
               (set-union! ra as)
               (set-union! rc con)              
               (load-fun-env `(,fname . ,ty))
               (set! rex (cons ty-ex rex))
               (infer-record ra rc ty rex)))
             (infer-record (mutable-set) (mutable-set) '() '())
             (drop-right (cdr exp) 1)))
  (match-define (infer-record assumptions constraints type type-expr) (infer-types (last exp) (set)))
  (set-union! constraints fun-con)
  (set! type-expr `(,@fun-ty-ex ,type-expr))
  (define substitutions (solve (set->list constraints)))
  `(program (type ,(str->sym (infix-fun-type (substitute substitutions type))))
            ,@(str->sym (map (curryr annotate-expr substitutions) type-expr)))
  )

#|
-----------------------------------------------------------
                      Test Cases
-----------------------------------------------------------
|#

(define e1 '(program (define (app f x)
                       (f x))
                     (app (lambda (x) x) 42)))
(define e2 '(program ((lambda (x) x) 42)))
(define e3 '(program  (((lambda (x)
                          (lambda (y) x)) 42) 444)))
(define e4 '(program ((lambda (x y) (+ x y)) 2 3)))
(define e5 '(program (let ((x (+ 5 2))) x)))
(define e6 '(program (let ((x 2) (y 5)) (+ x y))))
(define e7 '(program ((lambda (x) (let ((x x)) x)) 2)))
(define e8 '(program ((lambda (x) (let ((x 2)) (+ x x))) #f)))
(define e9 '(program ((lambda (z) (lambda (x z) (let ((x 2)) (+ x z)))) 5)))
(define e10 '(program (let ((x (lambda (x y) (+ x y)))) (x 5 2))))
(define e11 '(program (let ((f (lambda (x) (lambda (y) (+ x 1)))))
                (let ((g (f 2))) g))))
(define e12 '(program (if 1
                          (+ 3 5)
                          (- 5))))
(define e13 '(program (define-inline (id x) x)
                      (let ([fun id])
                        (fun 42))))
(define e14 '(program (define (hopefully-int)
                        (lambda (x)
                          (let ([maybe-int (read)])
                            (if (eq? maybe-int 42) x
                                42))))
                      (define (hopefully-bool)
                        (lambda (x)
                          (and (not x) #t)))
                      (if (hopefully-bool)
                          ((hopefully-int) 42)
                          (+ ((hopefully-int) 42) 0))))
(define e15 '(program (define (doubleid x)
                        ((lambda (x) x) x))
                      (doubleid 42)))
(define e16 '(program (define (f x)
                        (let ([y 4])
                          (lambda(z)
                            (+ x (+ y z)))))
                      (let ([g (f 5)])
                        (let ([h (f 3)])
                          (+ (g 11) (h 15))))))
(define e17 '(program (define (make-wrapper in out)
                        (lambda (fn)
                          (lambda (x)
                            (out (fn (in x))))))                      
                      (define (add1 x) (+ x 1))
                      (define (sub1 x) (+ x (- 1)))
                      (define (constfun x) 42)
                      (define (double x) (+ x x))
                      (let ([wrapper (make-wrapper add1 sub1)])
                        (let ([wrapconst (wrapper constfun)])
                          (let ([wrapdub (wrapper double)])
                            (let ([a (wrapdub 11)])
                              (constfun 777)))))))
(define e20 '(program (let ([v (vector 20 22)])
                        (+ (vector-ref v 0) (vector-ref v 1)))))
(define e21 '(program (let ([v (vector (vector 42) 21)])
                        (vector-ref (vector-ref v 0) 0))))
(define e22 '(program (let ([a (vector 1)])
  (let ([b (vector 1)])
    (let ([c (vector 1)])
      (let ([d (vector 1)])
        (let ([e (vector 1)])
          (let ([f (vector 1)])
            (let ([g (vector 1)])
              (let ([h (vector 1)])
                (let ([i (vector 1)])
                  (let ([j (vector 1)])
                    (let ([k (vector 1)])
                      (let ([l (vector 1)])
                        (let ([m (vector 1)])
                          (let ([n (vector 1)])
                            (let ([o (vector 1)])
                              (let ([p (vector 1)])
                                (let ([q (vector 1)])
                                  (let ([r (vector 1)])
                                    (let ([s (vector 1)])
                                      (let ([t (vector 1)])
                                        (let ([u (vector 1)])
                                          (+ (vector-ref a 0)
                                          (+ (vector-ref b 0) 
                                          (+ (vector-ref c 0) 
                                          (+ (vector-ref d 0) 
                                          (+ (vector-ref e 0) 
                                          (+ (vector-ref f 0) 
                                          (+ (vector-ref g 0) 
                                          (+ (vector-ref h 0) 
                                          (+ (vector-ref i 0) 
                                          (+ (vector-ref j 0) 
                                          (+ (vector-ref k 0) 
                                          (+ (vector-ref l 0) 
                                          (+ (vector-ref m 0) 
                                          (+ (vector-ref n 0) 
                                          (+ (vector-ref o 0)
                                          (+ (vector-ref p 0) 
                                          (+ (vector-ref q 0) 
                                          (+ (vector-ref r 0) 
                                          (+ (vector-ref s 0) 
                                          (+ (vector-ref t 0) 
                                             (+ (vector-ref u 0) 21))))))))))))))))))))))))))))))))))))))))))))
(define e24 '(program (define (big a b c d e f g h i j)
        (+ d j))
(define (big2 a b c d e f g h)
        (+ d h))
(let ([a (big 1 2 3 0 5 6 7 8 9 1)])
  (let ([s (big2 1 2 3 1 5 6 7 0)])
    (+ a s)))))
;; The Omega.
(define e18 '(program ((lambda (x) (x x))
                       (lambda (x) (x x)))))
;; This is a problem.
(define e19 '(program (define (x)
                        (x))
                      (x)))
;;Let polymorphism example
(define e23 '(program (define (f x) x)
                      (if (f #t)
                          (f 2)
                          (f 4))))



;; (infer-program e1)
;; (infer-program e2)
;; (infer-program e3)
;; (infer-program e4)
;; (infer-program e5)
;; (infer-program e6)
;; (infer-program e7)
;; (infer-program e8)
;; (infer-program e9)
;; (infer-program e10)
;; (infer-program e11)
;; (infer-program e12)
;; (infer-program e13)
;; (infer-program e14)
;; (infer-program e15)
;; (infer-program e16)
;; (infer-program e17)
;; (infer-program e20)
;; (infer-program e21)
;; (infer-program e22)
;; (infer-program e24)
;; (infer-program e18)
;; (infer-program e19)
;; (infer-program e23)
