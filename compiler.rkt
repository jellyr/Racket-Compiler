#lang racket
(require racket/fixnum)
(require racket/set)
(require "interp.rkt")
(require "utilities.rkt")
(require "uncover-types.rkt")

(provide r3-passes typechecker)

(define HEAP-LEN 10000) ;; For Debugging GC

(define SI-VARS '()) ;;Global variable to hold all new variables in select instructions pass

(define (int? e)
  (eqv? (car e) 'int))

(define (var? e)
  (eqv? (car e) 'var))

(define (reg? e)
  (eqv? (car e) 'reg))

(define (scalar? e)
  (or (fixnum? e) (symbol? e) (boolean? e)))

;; to read the type check paper
;; see the 521 minikaren one

;; actually it is check R3 language;
;; but i am too lazy to change the name
;; vector
;; var-env --> t1 -> t2
;; use walk up?
;; ( . (t2 .'(Vector Integer Integer)))

(define (typecheck-R2 env e)
  (match e
    [(? fixnum?) 'Integer]
    [(? boolean?) 'Boolean]
    [(? symbol?) (lookup e env)]
    [`(read) 'Integer]
    [`(+ ,e1 ,e2)
     (match `(,(typecheck-R2 env e1) ,(typecheck-R2 env e2))
       ['(Integer Integer) 'Integer]
       [else (error "In +")])]
    [`(- ,e1)
     (match (typecheck-R2 env e1)
       ['Integer 'Integer]
       [else (error "in -")])]
    (`(if ,econd ,ethen ,eelse)
     (match (typecheck-R2 env econd)
       ['Boolean (let ([tthen (typecheck-R2 env ethen)]
                       [telse (typecheck-R2 env eelse)])
                   (if (eqv? tthen telse)
                       tthen
                       (error "in if")))]
       [else (error "in if")]))
    [`(let ([,x ,e]) ,body)
     (define T (typecheck-R2 env e))
     (define new-env (cons (cons x T) env))
     (typecheck-R2 new-env body)]
    [`(not ,e)
     (match (typecheck-R2 env e)
       ['Boolean 'Boolean]
       [else (error "in not")])]
    [`(eq? ,e1 ,e2)
     (match `(,(typecheck-R2 env e1) ,(typecheck-R2 env e2))
       ['(Boolean Boolean) 'Boolean]
       ['(Integer Integer) 'Boolean]
       [else (error "In eq?")])]
    [`(and ,e1 ,e2)
     (match `(,(typecheck-R2 env e1) ,(typecheck-R2 env e2))
       ['(Boolean Boolean) 'Boolean]
       [else (error "In and")])]
    [`(vector . ,expr)
     `(Vector ,@(map (curry typecheck-R2 env) expr))]
    [`(vector-ref ,expr ,number)
     (define vector_t (typecheck-R2 env expr))
     (define erroref (curry error "in vector ref"))
     (match vector_t
       [`(Vector) (error "empty vector")]
       [`(Vector . ,expr) ;; see if we can check index?
        (if (eq? (typecheck-R2 env number) 'Integer)
            (list-ref vector_t (add1 number))
            (erroref))]
       [else (erroref)])]
    [`(vector-set! ,expr1 ,number ,expr2)
     (define vector_t (typecheck-R2 env expr1))
     (define errorset (curry error "in vector set"))
     (if (eq? (typecheck-R2 env number) 'Integer)
         (if (eq? (list-ref vector_t (add1 number)) (typecheck-R2 env expr2))
             'Void
             (errorset))
         (errorset))]
    [`(program ,body)
     (define _type (typecheck-R2 '() body))
     `(program (type ,_type) ,body)]))

(define  typechecker
  (curry typecheck-R2 '()))

(define uniquify
  (lambda (alist)
    (lambda (e)
      (match e
        [(? symbol?) (lookup e alist)]
        [(? boolean?) e]
        [(? integer?) e]
        [`(let ([,x ,e]) ,body)
         (let* ([newx (gensym x)]
               [newlist (cons `(,x . ,newx) alist)])
           `(let ([,newx ,((uniquify alist) e)])
              ,((uniquify newlist) body)))]
        [`(type ,type) e]
        [`(program ,e) `(program ,((uniquify alist) e))]
        [`(,op ,es ...)
          `(,op ,@(map (uniquify alist) es))]))))

(define (if-flatten cnd thn els)
  (match cnd
    [`(not ,cexp) (let-values (((cn tn el op) (if-flatten cexp els thn)))
                    (values cn tn el #f))]
    [`(eq? ,e1 ,e2) #:when (and (scalar? e1) (scalar? e2)) (values cnd thn els #t)]
    [else (values cnd thn els #f)]))


(define (flatten-vector expr)
  (let [(flat-vector   (foldl (lambda (exp res)
                                (let-values ([(e^ stmt^ alist^) (flattens exp)])
                                  `(,(append `(,e^) (car res))
                                    ,(append stmt^ (cadr res))
                                    ,(append alist^ (caddr res))))) '(() () ())  expr))]
      (values (car flat-vector)
              (cadr flat-vector)
              (caddr flat-vector))))

;;0 - Flatten till variables
;;1 - Flatten till expressions

;; 1 st : flattened variable
;; 2 nd : flattened statements
;; list of new vars which will show in the program

;; faltten-unwrap
;; faltten-helper
;; flatten

;; r2_13
;; another prob r2_15 which => allocate


(define (flattens e)
  (match e
    [(or (? scalar?) (? vector?)) (values e '() '())]
    [`(read) (let [(newvar (gensym))]
               (values newvar  `((assign ,newvar (read))) `(,newvar)))]
    ;; be careful here 
    [`(program ,type ,e) (let-values ([(e^ stmt^ alist^) (flattens e)])
                     `(program ,alist^ ,type  ,@stmt^ (return ,e^)))]
    [`(vector . ,e1) (let-values ([(e^ stmt^ alist^) (flatten-vector e1)])
                       (let [(newvar (gensym))]
                         (values newvar
                                 (append stmt^ `((assign ,newvar (vector . ,e^))))
                                 (cons newvar alist^))))]
    [`(vector-set! ,e1 ,e2 ,e3) (let-values ([(e1^ stmt1^ alist1^) (flattens e1)]
                                             [(e2^ stmt2^ alist2^) (flattens e2)]
                                             [(e3^ stmt3^ alist3^) (flattens e3)]
                                             [(newvar) (gensym)])
                                  (values newvar
                                          (append stmt1^
                                                  stmt2^
                                                  stmt3^
                                                  `((assign ,newvar (vector-set! ,e1^ ,e2^ ,e3^))))
                                          (append (cons newvar alist1^) alist2^ alist3^)))]
    [`(if ,cn ,tn ,en) (let-values (((cnd thn els op) (if-flatten cn tn en)))
                         (let-values (((ec stmtc alistc) (flattens cnd))
                                      ((et stmtt alistt) (flattens thn))
                                      ((ee stmte aliste) (flattens els)))
                           (let ([newvar (gensym)])
                             (values newvar
                                     (if op
                                         (append `((if ,cnd
                                                       ,(append stmtt `((assign ,newvar ,et)))
                                                       ,(append stmte `((assign ,newvar ,ee))))))
                                         (append stmtc `((if (eq? #t ,ec)
                                                             ,(append stmtt `((assign ,newvar ,et)))
                                                             ,(append stmte `((assign ,newvar ,ee)))))))
                                     (if op
                                         (append (cons newvar alistt) aliste)
                                         (append (cons newvar alistc) alistt aliste))))))]
    [`(let ([,x ,e]) ,body) (let-values
                                ([(xe^ stmtx^ alistx^) (flattens e)]
                                 [(be^ stmtb^ alistb^) (flattens body)])
                              (match e
                                [`(if ,cnd ,thn ,els) (values be^
                                                              (append stmtx^ `((assign ,x ,xe^)) stmtb^)
                                                              (append alistx^ alistb^))]
                                [else (let* [(alistx^ (cons x (if (null? alistx^) alistx^ (remq xe^ alistx^))))
                                             (xe^ (if (null? stmtx^) xe^ (last (last stmtx^))))
                                             (stmtx^ (if (null? stmtx^) '() (take stmtx^ (sub1 (length stmtx^)))))]
                                        (values be^
                                                (append stmtx^ `((assign ,x ,xe^)) stmtb^)
                                                (append alistx^ alistb^)))]))]
    [`(and ,e1 ,e2) (flattens `(if (eq? ,e1 #t) ,e2 #f))]
    [`(,op ,e1 ,e2) (let-values (((e1^ stmt1^ alist1^) (flattens e1))
                               ((e2^ stmt2^ alist2^) (flattens e2)))
                    (let ([newvar (gensym)])
                      (values newvar
                              (append stmt1^ (append stmt2^ `((assign ,newvar (,op ,e1^ ,e2^)))))
                              (append (cons newvar alist1^) alist2^))))]
    [`(,op ,e1) (let-values ([(e^ statements^ alist) (flattens e1)])
                (let [(newvar (gensym))]
                  (values newvar
                          (append statements^ `((assign ,newvar (,op ,e^))))
                          (cons newvar alist))))]))


;; expose allocation
;; figure out what type is
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

;; =============

(define (live-roots-vector? var type-env)
  (let ([lkp (lookup var type-env #f)])
    (match lkp
      [`(Vector . ,e1) #t]
      [else #f])))


(define (live-if-helper e type-env)
  (foldr (lambda (x r)
           (let* ([lives (if (null? r) (set) (car r))]
                  [newlives (live-analysis x lives type-env)])
             (cons newlives r)))
         '() e))

;; set -> lak
(define (live-analysis instr lak type-env)
  (define vector? (curryr live-roots-vector? type-env))
  (define (vector-unwrap var) (if (vector? var) (set var) (set)))
  (match instr
    [(? vector?) (set instr)]
    [`(allocate ,e) (set)]
    [`(assign ,var ,e) (let ([forsub (vector-unwrap var)]
                             [forunion (live-analysis e (set) type-env)])
                         (set-union forunion (set-subtract lak forsub)))]
    [`(vector-set! ,var ,index ,e) (let ([forunion (live-analysis e lak type-env)])
                                     (set-union lak forunion))]
    [`(vector-ref ,v ,index) (set-union lak (set v))]
    [`(if (eq? ,e1 ,e2) ,thn ,els)  (let ([e1set (live-analysis e1 lak type-env)]
                                          [e2set (live-analysis e2 lak type-env)]
                                          [thenset (live-if-helper thn type-env)]
                                          [elseset (live-if-helper els type-env)])
                                      (set-union e1set e2set (car thenset) (car elseset)))]
    [`(return ,var) (set)]
    [else lak]))

(define (call-live-roots e)
  (define (live-instr-helper instr livea)
    (match instr
      [`(if (collection-needed? ,e1)
            ((collect ,e2))
            ())                      `(if (collection-needed? ,e1)
                                          ((call-live-roots ,(set->list livea) (collect ,e2)))
                                          ())]
      [else instr]))
  (let* ([prog (car e)]
         [types (cadr e)]
         [ret-type (caddr e)]
         [instrs (cdddr e)]
         [livea (foldr (lambda (instr res)
                         (define value (live-analysis instr (car res) types))
                         (cons value res))
                       `(,(set))
                       instrs)])
    `(,prog ,(map car types) ,ret-type ,@(map live-instr-helper instrs (cdr livea)))))


;; =============

(define (calc-pointer-mask v idx res)
  (match v
    [`(Vector ,e1) (+ res (* idx 1))]
    [else (+ res (* idx 0))]))

(define (select-instructions-assign ret-v e)
  (match e
    [(? fixnum?) `(int ,e)]
    ;[(? symbol?) #:when (eq? e ret-v) '(reg rax)]
    [(? boolean?) (if e '(int 1) '(int 0))]
    [(? symbol?) #:when (not (eq? e 'program)) `(var ,e)]
    [`(not ,e1) `(xorq (int 1) ,(select-instructions-assign ret-v e1))]
    [`(initialize ,rootlen ,heaplen) (let ([newvar1 (gensym 'rootstack)])
                                       (set! SI-VARS `(,newvar1))
                                       `((movq (int ,rootlen) (reg rdi))
                                         (movq (int ,heaplen) (reg rsi))
                                         (callq initialize)
                                         (movq (global-value rootstack_begin)
                                               ,(select-instructions-assign ret-v newvar1))))]
    [`(assign ,var (vector-ref ,v1 ,idx)) (let ([v1^ (select-instructions-assign ret-v v1)]
                                                [var^ (select-instructions-assign ret-v var)])
                                            `((movq (offset ,v1^ ,(* 8 (add1 idx))) ,var^)))]
    [`(assign ,var (vector-set! ,v1 ,idx ,arg)) (let ([v1^ (select-instructions-assign ret-v v1)]
                                                      [arg^ (select-instructions-assign ret-v arg)])
                                                  `((movq ,arg^ (offset ,v1^ ,(* 8 (add1 idx))))))]
    [`(assign ,var (allocate ,len (Vector . ,type))) (let* ([var^ (select-instructions-assign ret-v var)]
                                                            [ptrmask (foldr calc-pointer-mask
                                                                            0
                                                                            type
                                                                            (build-list len (curry expt 2)))]
                                                            [tag (bitwise-ior (arithmetic-shift ptrmask 7)
                                                                              (bitwise-ior (arithmetic-shift len 1) 1))])
                                                       `((movq (global-value free-ptr) ,var^)
                                                         (addq (int ,(* 8 (add1 len))) (global-value free_ptr))
                                                         (movq (int ,tag) (offset ,var^ 0))))]
    [`(call-live-roots ,la (collect ,bytes^)) (let* ([n (length la)]
                                                     [nvals (build-list n values)]
                                                     [newvar1 (last SI-VARS)]
                                                     [newvar2 (gensym 'rootstack)]
                                                     [nv1 (select-instructions-assign ret-v newvar1)]
                                                     [nv2 (select-instructions-assign ret-v newvar2)])
                                                (set! SI-VARS (append `(,newvar2) SI-VARS))
                                                `(,@(map (lambda (v idx)
                                                           `(movq (var ,v) (offset ,nv1 ,(* 8 idx))))
                                                         la
                                                         nvals)
                                                  (movq ,nv1 ,nv2)
                                                  (addq ,(select-instructions-assign ret-v n) ,nv2)
                                                  (movq ,nv2 (reg rdi))
                                                  (movq (int ,bytes^) (reg rsi))
                                                  (callq collect)
                                                  ,@(map (lambda (v idx)
                                                           `(movq (offset ,nv1 ,(* 8 idx)) (var ,v)))
                                                         la
                                                         nvals)))]
    [`(if (collection-needed? ,bytes^) ,thn ,els) (let* ([thn^ (map (curry select-instructions-assign ret-v) thn)]
                                                         [els^ (map (curry select-instructions-assign ret-v) els)]
                                                         [newvar1 (gensym 'end-data)]
                                                         [newvar2 (gensym 'lt)]
                                                         [nv1 (select-instructions-assign ret-v newvar1)]
                                                         [nv2 (select-instructions-assign ret-v newvar2)])
                                                    (set! SI-VARS (append `(,newvar1 ,newvar2) SI-VARS))
                                                    `((movq (global-value free_ptr) ,nv1)
                                                      (addq (int ,bytes^) ,nv1)
                                                      (cmpq ,nv1 (global-value fromspace_end))
                                                      (setl (byte-reg al))
                                                      (movzbq (byte-reg al) ,nv2)
                                                      (if (eq? (int 0) ,nv2)
                                                          ,els^
                                                          ,(car thn^))))]
    [`(assign ,var (eq? ,e1 ,e2)) `((cmpq ,(select-instructions-assign ret-v e1)
                                          ,(select-instructions-assign ret-v e2))
                                    (sete (byte-reg al))
                                    (movzbq (byte-reg al) (var ,var)))]
    [`(if (eq? ,e1 ,e2) ,thn ,els) `((if (eq? ,(select-instructions-assign ret-v e1)
                                              ,(select-instructions-assign ret-v e2))
                                         ,(append-map (curry select-instructions-assign ret-v) thn)
                                         ,(append-map (curry select-instructions-assign ret-v) els)))]
    [`(assign ,var (read)) `((callq read_int) (movq (reg rax) (var ,var)))]
    [`(assign ,var (- ,e1)) `((movq ,(select-instructions-assign ret-v e1) (var ,var)) (negq (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e1) `((addq ,(select-instructions-assign ret-v e2) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e2) `((addq ,(select-instructions-assign ret-v e1) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2)) `((movq ,(select-instructions-assign ret-v e1) (var ,var))
                                  (addq ,(select-instructions-assign ret-v e2) (var ,var)))]
    [`(return ,e1) `((movq ,(select-instructions-assign ret-v e1) (reg rax)))]
    [`(assign ,var ,e1) `((movq ,(select-instructions-assign ret-v e1) (var ,var)))]
    ;[(? list?) (list (remove ret-v e))]
    [else `(,e)]))




(define (select-instructions e)
  (let* ([ret-var (last (last e))]
         [prog (car e)]
         [args (cadr e)]
         [ret-type (caddr e)]
         [instrs (cdddr e)]
         [si (append-map (curry select-instructions-assign ret-var) instrs)])
    `(,prog ,(append args SI-VARS) ,ret-type ,@si)))

;;;;;;
(define (uncover-live-unwrap e)
  (match e
    [`(var ,e1) (set e1)]
    [`(xorq (int 1) (var ,s)) (set s)]
    ;[`(reg ,r) (set r)]
    [else (set)]))

(define (uncover-live-helper e lak)
  (define lak^ (list->set lak))
  (match e
    ; [`(callq read_int) (set-add lak^^ 'rax)]
    ;[`(negq ,e1) (set-union lak^^ (uncover-live-unwrap e1))
    [`(if (eq? ,e1 ,e2) ,thn ,els) (let* ([thenexpr (instrs-live-helper thn)]
                                          [elseexpr (instrs-live-helper els)]
                                          [thenexpr (if (null? thenexpr) (list '(()) `(,(set))) thenexpr)]
                                          [elseexpr (if (null? elseexpr) (list '(()) `(,(set))) elseexpr)]
                                          [thenset (if (null? thenexpr) (set) (car (last thenexpr)))]
                                          [elseset (if (null? elseexpr) (set) (car (last elseexpr)))])
                                     (list `(if (eq? ,e1 ,e2) ,@thenexpr ,@elseexpr)
                                           (set-union thenset elseset)))]
    [`(movq ,e1 ,e2) (list e (set-union (set-subtract lak^ (uncover-live-unwrap e2)) (uncover-live-unwrap e1)))]
    [`(cmpq ,e1 ,e2) (list e (set-union lak^ (uncover-live-unwrap e1) (uncover-live-unwrap e2)))]
    [`(movzbq ,e1 ,e2) (list e (set-subtract lak^ (uncover-live-unwrap e2)))]
    [`(addq ,e1 ,e2) (list e (set-union lak^ (uncover-live-unwrap e1) (uncover-live-unwrap e2)))]
    [`(subq ,e1 ,e2) (list e (set-union lak^ (uncover-live-unwrap e1) (uncover-live-unwrap e2)))]
    [else (list e lak)]))


(define (instrs-live-helper e)
  (foldr (lambda (x r)
           (let* ([expr (if (null? r) `() (car r))]
                  [lives (if (null? r) `() (cadr r))]
                  [helpexpr (uncover-live-helper x (if (null? lives) (set) (car lives)))])
             (list (cons (car helpexpr) expr)
                   (cons (cadr helpexpr) lives))))
         '() e))


(define (uncover-live e)
  (let ((setlist (foldr (lambda (x r)
                          (let* ([expr (if (null? r) '() (car r))]
                                 [lives (if (null? r) `(,(set)) (cadr r))]
                                 [helpexpr (uncover-live-helper x (if (null? lives) (set) (car lives)))])
                            (list (cons (car helpexpr) expr)
                                  (cons (cadr helpexpr) lives))))
                        '() (cdddr e))))
    `(,(car e) ,(list (cadr e) (cdadr setlist)) ,(caddr e) ,@(car setlist))))

;;;;;;;;;;

(define (build-interference-unwrap e)
  (match e
    [`(var ,e1) e1]
    [`(reg ,r) r] ;; removed rax from interference graph
    ['(byte-reg al) 'rax]
    [`(xorq (int 1) (var ,e1)) e1]
    [else e]))

(define (build-interference-helper graph e lak)
  (let ([lak (set->list lak)])
    (match e
      [`(,op ,e1 ,e2)#:when (and (or (var? e2) (reg? e2)) (or (eq? op 'movq) (eq? op 'movzbq)))
       (let ([s (build-interference-unwrap e1)]
             [d (build-interference-unwrap e2)])
         (map (lambda (v) (cond
                            [(not (or (eqv? s v) (eqv? d v))) (add-edge graph d v)]
                            [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
      [`(,op ,e1 ,e2)#:when (and (or (var? e2) (reg? e2)) (or (eq? op 'addq) (eq? op 'cmpq)))
       (let ([s (build-interference-unwrap e1)]
             [d (build-interference-unwrap e2)])
         (map (lambda (v) (cond
                            [(not (eqv? d v)) (add-edge graph d v)]
                            [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
      [`(negq ,e2)#:when (or (var? e2) (reg? e2))
       (let ([d (build-interference-unwrap e2)])
         (map (lambda (v) (cond
                            [(not (eqv? d v)) (add-edge graph d v)]
                            [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
      [`(callq ,label) (map (lambda (v1)
                              (map (lambda (v2)
                                     (hash-set! graph v1 (set-add (hash-ref graph v1 (set)) v2)))
                                   (set->list caller-save))) lak)]
      [`(if (eq? ,e1 ,e2) ,thn ,thnlive ,els ,elslive)
       (let ([s (build-interference-unwrap e1)]
             [d (build-interference-unwrap e2)])
         (map (lambda (v1 v2)
                (build-interference-helper graph v1 v2)) thn thnlive)
         (map (lambda (v1 v2)
                (build-interference-helper graph v1 v2)) els elslive)
         (map (lambda (v)
                (cond
                  [(and (symbol? d) (not (eqv? d v))) (add-edge graph d v)]
                  ;[(and (symbol? s) (not (eqv? s v))) (add-edge graph s v)]
                  [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
      [else '()])))

(define (build-interference e)
  (let* ([lak (cadadr e)]
         [instr (cdddr e)]
         [graph (make-graph '())])
    (map (curry build-interference-helper graph) instr lak)
    `(,(car e) (,(caadr e) ,graph) ,(caddr e) ,@instr)))

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
;; phash == prefred hash table
(define (assign-minicolor node graph assign-list constrain-graph phash)
  (define colorvals (range 100))
  (define contrains (hash-ref! constrain-graph (car node) (set)))
  (if (set-member? (hash-ref graph (car node) (set)) 'rcx)
      (set! contrains (set-union contrains (list->set (range 1 9))))
      '()) 
  (define preferlist (dropf (map (lambda (v) (with-handlers ([exn:fail? (lambda (exn) #f)])
                                               (lookup v assign-list)))
                                 (set->list (set-subtract (hash-ref! phash (car node) (set))
                                                          (hash-ref graph (car node) (set)))))
                            false?))
  (if (null? preferlist)
      (car (dropf colorvals (curry set-member? contrains)))
      (car preferlist)))


;; make nicerrrrr
(define (allocate-registers-helper graph assign-list constrain-graph phash)
  (let* ([node (highest-saturation graph (map car assign-list))]
         [minvalue (assign-minicolor node graph assign-list constrain-graph phash)])
    (cond
      ((eq? 'none (car node)) assign-list)
      (else (allocate-registers-helper graph
                                       `((,(car node) . ,minvalue) . ,assign-list)
                                       (foldl (lambda (gr res)
                                                (hash-set! res gr (set-add (hash-ref! res gr (set)) minvalue))
                                                res) constrain-graph (set->list (cdr node)))
                                       phash)))))

;; stacki = -1 ;
(define (allocate-reg-stack assign-list)
  (define k (vector-length general-registers)) ;; (vector-length general-registers)
  (let ([reglist (filter (lambda (v) (< (cdr v) k)) assign-list)]
        [stacklist (filter (lambda (v) (>= (cdr v) k)) assign-list)])
    (cons `(_stacklength . ,(set-count (list->set (map cdr stacklist)))) ;; a hack way
          (append (map (lambda (v) `(,(car v) . (stack ,(* -8 (add1 (- (cdr v) k)))))) stacklist)
                  (map (lambda (v) `(,(car v) . (reg ,(vector-ref general-registers (cdr v))))) reglist)))))

(define (allocate-var e env)
  (match e
    ;;; consider this situation again, testcase: (let ([x 41]) (+ x 1)) 
    [`(var ,e1) (with-handlers ([exn:fail? (lambda (exn) '(reg rax))])
                  (lookup e1 env))]
    [`(,op ,e1 ,e2) `(,op ,(allocate-var e1 env) ,(allocate-var e2 env))]
    [`(,op ,e1) `(,op ,(allocate-var e1 env))]
    [`(if (eq? ,e1 ,e2) ,thn ,thnlive ,els ,elslive) `(if (eq? ,(allocate-var e1 env) ,(allocate-var e2 env))
                                           ,(map (lambda (v)
                                                   (allocate-var v  env)) thn)
                                           ,(map (lambda (v)
                                                   (allocate-var v  env)) els))]
    ;[`(addq ,e1 ,e2) `(addq ,(allocate-var e1 env) ,(allocate-var e2 env))]
    ;[`(subq ,e1 ,e2) `(subq ,(allocate-var e1 env) ,(allocate-var e2 env))]
    [else e]))

;; work for move biasing
;; parameter: the instructions
;; return a hash, for each value it prefer to assgin to, data structure is like{ "a" : ["b", "c"]}
(define (allocate-prefer insts)
  (define phash (make-hash))
  (map (lambda (inst)
         (match inst
           [`(movq (var ,e1) (var ,e2))
            (hash-set! phash e1 (set-add (hash-ref phash e1 (set)) e2))]
           ;; consider if condition
           [else '()])) insts)
  phash)

;;; consider rax
(define (allocate-registers e)
  (let* ([phash (allocate-prefer (cdddr e))]
         ;;; (hash-remove (cadadr e) 'rax)
         [assign-list (allocate-registers-helper (cadadr e) '() (make-graph '()) phash)]
         [env (allocate-reg-stack assign-list)]
         [prog (car e)])
    ;(print assign-list)
    `(,prog ,(lookup '_stacklength env) ,(caddr e) . ,(cdddr (map (curryr allocate-var env) e)))))

(define (lower-conditionals-helper e)
  (define elselabel (gensym 'else))
  (define thenlabel (gensym 'then))
  (define endlabel (gensym 'ifend))
  (match e
    [`(if (eq? ,e1 ,e2) ,thn ,els) `((cmpq ,e1 ,e2)
                                        (je ,thenlabel)
                                        ,@(lower-conditionals-helper els)
                                        (jmp ,endlabel)
                                        (label ,thenlabel)
                                        ,@(lower-conditionals-helper thn)
                                        (label ,endlabel))]
    [`(()) '()]
    [else e]))

(define (lower-conditionals e)
  (define insts (foldr (lambda (x r)
                       (define x^ (lower-conditionals-helper x))
                       (if (eq? 'if (car x))
                           (append x^ r)
                           (cons x^ r))
                       )  '() (cddr e)))
  ;(define t (if (eq? 1 (length insts)) (car insts) insts))
  `(,(car e) ,(cadr e) ,(caddr e) ,@insts))


; starti == -1
;; (define (assign-homes-env alist starti)
;;   (cond
;;     [(null? alist) '()]
;;     [else (append `((,(car alist) . (stack ,(* 8 starti)))) (assign-homes-env (cdr alist) (sub1 starti)))]))

(define (patch-instr-helper e)
  (match e
    [`(movq ,e1 ,e2) #:when (equal? e1 e2) '()]
    [`(,op (stack ,e1) (stack ,e2)) `((movq (stack ,e1) (reg rax)) (,op (reg rax) (stack ,e2)))]
    [`(cmpq ,e1 ,e2) #:when (int? e2) (if (or (var? e1) (reg? e1))
                                          `((cmpq ,e2 ,e1))
                                          `((movq ,e2 (reg rax)) (cmpq ,e1 (reg rax))))]
    [else `(,e)]))

(define (patch-instructions e)
  (append-map patch-instr-helper e))

(define (print-helper e)
  (match e
    [`(stack ,e1) (format "~a(%rbp)" e1)]
    [`(type ,e1) ""]
    [`(int ,e1) (format "$~a" e1)]
    [`(reg ,e1) (format "%~a" e1)]
    [`(global-value ,e1) (format "~a(%rip)" e1)]
    [`(byte-reg ,e1) (format "%~a" e1)]
    [`(label ,label) (format "~a:\n" label)]
    [`(offset ,reg ,index) (format "~a(~a)" index (print-helper reg))]
    [`(,op ,e1) (string-append (format "~a	" op) (print-helper e1) "\n\t")]
    [`(,op ,e1 ,e2) (string-append (format "~a	" op) (print-helper e1) ", " (print-helper e2)" \n\t")]
    ;[`(callq initialize) (format "callq initialize\n")] ;; this can be else
    ;[`(negq ,e1) (string-append "negq	" (print-helper e1) " \n\t" )]
    ;[`(callq ,e1) (string-append "callq	" (print-helper e1) " \n\t" )]
    ;[`(jmp ,e1) (string-append "jmp	" (print-helper e1) "\n\t")]
    ;[`(movq ,e1 ,e2) (string-append "movq	" (print-helper e1) ", " (print-helper e2)" \n\t")]
    ;[`(addq ,e1 ,e2) (string-append "addq	" (print-helper e1) ", " (print-helper e2) " \n\t")]
    ;[`(xorq ,e1 ,e2)]
    [else (format "~s" e)]
    ))


;; patameter, return a func name
(define (callq-helper typexpr)
  (match typexpr
      ['Integer "	callq print_int\n\t"]
      ['Boolean "	callq print_bool\n\t"]
      ['Void "	callq print_void\n\t"]
      [`(Vector . ,typexpr1) (string-append
                              (format "	callq print_vecbegin\n\t")
                              (foldr (lambda (v r)
                                       (string-append (callq-helper v) (format  "	callq print_space\n\t"))) "" (cdr (reverse typexpr1)))
                              (callq-helper (last typexpr1))
                              (format "	callq print_vecend\n\t"))]))

(define (print-x86 e)
  (let ([type (caddr e)])
    (string-append
   (format "	.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$~a, %rsp\n\t" (* 8 (cadr e)))
   (string-join (map print-helper (cdddr e)))
   (format "	movq	%rax, %rdi")
   (callq-helper (cadr type))
   (format "	movq    $0, %rax
	addq	$~a, %rsp
	popq	%rbp
	retq" (* 8 (cadr e))))))

(define r3-passes `(
                    ("uniquify" ,(uniquify '()) ,interp-scheme)
                    ("flattens" ,flattens ,interp-C)
                    ("expose-allocation" ,expose-allocation ,interp-C)
                    ("call-live-roots" ,call-live-roots ,interp-C)
                    ("select instructions" ,select-instructions ,interp-x86)
                    ("uncover-live" ,uncover-live ,interp-x86)
                    ("build interference graph" ,build-interference ,interp-x86)
                    ("register allocation" ,allocate-registers ,interp-x86)
                    ("live" ,lower-conditionals ,interp-x86)
                    ("patch instructions" ,patch-instructions ,interp-x86)
                    ("print x86" ,print-x86 #f)))





