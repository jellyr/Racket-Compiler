#lang racket
(require racket/fixnum)
(require racket/set)
(require "interp.rkt")
(require "utilities.rkt")

(provide r2-passes)



(define (int? e)
  (eqv? (car e) 'int))

(define (var? e)
  (eqv? (car e) 'var))

(define (reg? e)
  (eqv? (car e) 'reg))

;; to read the type check paper
;; see the 521 minikaren one

;; check R2 language
(define (typecheck-R2 env e)
  (match e
    [(? fixnum?) 'Integer]
    [(? boolean?) 'Boolean]
    [(? symbol?) (lookup e env)]
    [`(read) (error "implement me")]
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
       [else (error "In eq?")])]
    [`(and ,e1 ,e2)
     (match `(,(typecheck-R2 env e1) ,(typecheck-R2 env e2))
       ['(Boolean Boolean) 'Boolean]
       [else (error "In and")])]
    [`(program ,body)
     (typecheck-R2 '() body)
     `(program ,body)]))

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
        [`(program ,e) `(program ,((uniquify alist) e))]
        [`(,op ,es ...)
          `(,op ,@(map (uniquify alist) es))]))))

;;0 - Flatten till variables
;;1 - Flatten till expressions
(define (flattens e)
  (match e
    [(or (? fixnum?) (? symbol?) (? boolean?)) (values e '() '())]
    [`(read) (let [(newvar (gensym))]
               (values newvar  `((assign ,newvar (read))) `(,newvar)))]
    [`(program ,e) (let-values ([(e^ stmt^ alist^) (flattens e)])
                     `(program ,alist^ ,@stmt^ (return ,e^)))]
    [`(if ,cnd ,thn ,els) (let-values (((ec stmtc alistc) (flattens cnd))
                                       ((et stmtt alistt) (flattens thn))
                                       ((ee stmte aliste) (flattens els)))
                            (let ([newvar (gensym)])
                              (values newvar
                                      (append stmtc `((if (eq? #t ,ec)
                                                          ,(append stmtt `((assign ,newvar ,et)))
                                                          ,(append stmte `((assign ,newvar ,ee))))))
                                      (append (cons newvar alistc) alistt aliste))))]
    [`(let ([,x ,e]) ,body) (let-values
                                ([(xe^ stmtx^ alistx^) (flattens e)]
                                 [(be^ stmtb^ alistb^) (flattens body)])
                              (let* [(xe^ (if (null? stmtx^) xe^ (last (last stmtx^))))
                                     (alistx^ (cons x (if (null? alistx^) alistx^ (cdr alistx^))))
                                     (stmtx^ (if (null? stmtx^) '() (take stmtx^ (sub1 (length stmtx^)))))]
                                (values be^
                                        (append stmtx^ (append `((assign ,x ,xe^)) stmtb^))
                                        (append alistx^ alistb^))))]
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

(define (select-instructions-assign ret-v e)
  (match e
    [(? fixnum?) `(int ,e)]
    ;[(? symbol?) #:when (eq? e ret-v) '(reg rax)]
    [(? boolean?) (if e '(int 1) '(int 0))]
    [(? symbol?) #:when (not (eq? e 'program)) `(var ,e)]
    [`(not ,e1) `(xorq (int 1) (var ,e1))]
    [`(assign ,var (eq? ,e1 ,e2)) `((cmpq ,(select-instructions-assign ret-v e1)
                                          ,(select-instructions-assign ret-v e2))
                                    (sete (byte-reg al))
                                    (movzbq (byte-reg al) (var ,var)))]
    [`(if (eq? ,e1 ,e2) ,thn ,els) `((if (eq? ,(select-instructions-assign ret-v e1)
                                              ,(select-instructions-assign ret-v e2))
                                         ,(append-map (curry select-instructions-assign ret-v) thn)
                                         ,(append-map (curry select-instructions-assign ret-v) els)))]
    [`(assign ,var (read)) `((callq read_int) (movq (reg rax) (var ,var)))]
    [`(assign ,var (- ,e1)) `((movq ,(select-instructions-assign ret-v e1) `(var ,var)) (negq (var ,var)))]
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
         [si (append-map (curry select-instructions-assign ret-var) e)])
    si))

;;;;;;
(define (uncover-live-unwrap e)
  (match e
    [`(var ,e1) (set e1)]
    [`(xorq (int 1) (var ,s)) (set s)]
    ;[`(reg ,r) (set r)]
    [else (set)]))

(define (uncover-live-helper e lak)
  (match e
    [`(movq ,e1 ,e2) (set-union (set-subtract lak (uncover-live-unwrap e2)) (uncover-live-unwrap e1))]
    ; [`(callq read_int) (set-add lak 'rax)]
    ;[`(negq ,e1) (set-union lak (uncover-live-unwrap e1))
    [`(if (eq? ,e1 ,e2) ,thn ,els) (let ([tlistlak (if (var? (car thn))
                                                          `(,(uncover-live-unwrap thn))
                                                          ;;change to foldr - consider bottom to top processing
                                                          ;; make this foldr a function
                                                          (map (curryr uncover-live-helper lak) thn))]
                                         [elistlak (if (var? (car els))
                                                          `(,(uncover-live-unwrap els))
                                                          (map (curryr uncover-live-helper lak) els))])
                                     (list tlistlak
                                           elistlak
                                           `(,(set-union (uncover-live-unwrap e1)
                                                       (uncover-live-unwrap e2)))))]
    [`(cmpq ,e1 ,e2) (set-union lak (uncover-live-unwrap e1) (uncover-live-unwrap e2))]
    [`(movzbq ,e1 ,e2) (set-subtract lak (uncover-live-unwrap e2))]
    [`(addq ,e1 ,e2) (set-union lak (uncover-live-unwrap e1) (uncover-live-unwrap e2))]
    [`(subq ,e1 ,e2) (set-union lak (uncover-live-unwrap e1) (uncover-live-unwrap e2))]
    [else lak]))

(define (if-stmt-expansion e lak)
  (match e
    [`(if (eq? ,e1 ,e2) ,thn ,els)
                                    (`(if (eq? ,e1 ,e2)
                                          ,(map (lambda (e^ elak^)
                                                  (if-stmt-expansion e^ lak^)) thn (map set->list (car lak^)))
                                          ,(map (lambda (e^ elak^)
                                                  (if-stmt-expansion e^ lak^)) els (map set->list (cadr lak^)))))]))


(define (uncover-live e)
  (define instrs '())
  (let [(setlist (map set->list
                      (foldr (lambda (x r)
                               (let ([lak^ (uncover-live-helper x (car r))])
                                 (match x
                                   [`(if (eq? ,e1 ,e2) ,thn ,els)
                                    (set! instrs `(if (eq? ,e1 ,e2)
                                                      ,(map cons thn (map set->list (car lak^)))
                                                      ,(map cons els (map set->list (cadr lak^)))))
                                     (flatten lak^)]
                                   [else (set! instrs (cons instrs x))])))
                             `(,(set))
                             (cddr e))))]
    `(,(car e) ,(list (cadr e) (cdr setlist)) ,@instrs)))

;;;;;;;;;;

(define (build-interference-unwrap e)
  (match e
    [`(var ,e1) e1]
    [`(reg ,r) r] ;; removed rax from interference graph
    ['(byte-reg al) 'rax]
    [`(xorq (int 1) (var ,e1)) e1]
    [else e]))

(define (build-interference-helper graph e lak)
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
    [`(if (eq? ,e1 ,e2) ,thn ,els)
     (let ([s (build-interference-unwrap e1)]
           [d (build-interference-unwrap e2)])
       (map (lambda (v1)
              (build-interference-helper graph (car v1) (cadr v1))) thn)
       (map (lambda (v1)
              (build-interference-helper graph (car v1) (cadr v1))) els)
       (map (lambda (v)
              (cond
                [(not (eqv? d v)) (add-edge graph d v)]
                [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
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
    [`(if (eq? ,e1 ,e2) (,thn) (,els)) `(if ,e1 (allocate-var e2 env)
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
           [else '()])) insts)
  phash)

;;; consider rax
(define (allocate-registers e)
  (let* ([phash (allocate-prefer (cddr e))]
         ;;; (hash-remove (cadadr e) 'rax)
         [assign-list (allocate-registers-helper (cadadr e) '() (make-graph '()) phash)]
         [env (allocate-reg-stack assign-list)]
         [prog (car e)])
    `(,prog ,(lookup '_stacklength env) . ,(cddr (map (curryr allocate-var env) e)))))

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

(define (lower-conditionals-helper e)
  (define elselabel (gensym 'else))
  (define thenlabel (gensym 'then))
  (define endlabel (gensym 'ifend))
  (match e
    [`(if (eq? ,e1 ,e2) (,thn) (,els)) `((cmpq ,e1 ,e2)
                                        (je ,thenlabel)
                                        ,@(lower-conditionals-helper els)
                                        (jmp ,endlabel)
                                        (label ,thenlabel)
                                        ,@(lower-conditionals-helper thn)
                                        (label ,endlabel))]
    [else e]))

(define (lower-conditionals e)
  (map lower-conditionals-helper e))

(define (print-helper e)
  (match e
    [`(stack ,e1) (format "~a(%rbp)" e1)]
    [`(int ,e1) (format "$~a" e1)]
    [`(reg ,e1) (format "%~a" e1)]
    [`(label ,label) (format "%~a:\n" label)]
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

(define r2-passes `(("typecheck-R2" ,(curry typecheck-R2 '()) ,interp-scheme)
                    ("uniquify" ,(uniquify '()) ,interp-scheme)
                    ("flattens" ,flattens ,interp-C)
                    ("select instructions" ,select-instructions ,interp-x86)
                    ("uncover-live" ,uncover-live ,interp-x86)
                    ("build interference graph" ,build-interference ,interp-x86)
                    ("register allocation" ,allocate-registers ,interp-x86)
                    ("patch instructions" ,patch-instructions ,interp-x86)
                    ("print x86" ,print-x86 #f)))
