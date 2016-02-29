#lang racket
(require racket/fixnum)
(require racket/set)

;; prof's stuff
(require "interp.rkt")
(require "utilities.rkt")
(require "uncover-types.rkt")

;; ours
(require "common.rkt")
;; some passes
(require "pass/typechecker.rkt")
(require "pass/uniquify.rkt")
(require "pass/flatten.rkt")
(require "pass/expose.rkt")
(require "pass/call-live-roots.rkt")
(require "pass/select-instructions.rkt")

(provide r3-passes typechecker)


(define SI-VARS '()) ;;Global variable to hold all new variables in select instructions pass


(define  typechecker
  (curry typecheck-R2 '()))

;;;;;;
(define (uncover-live-unwrap e)
  (match e
    [`(var ,e1) (set e1)]
    [`(xorq (int 1) (var ,s)) (set s)]
    [`(offset ,e1 ,idx) (uncover-live-unwrap e1)]
    ;[`(reg ,r) (set r)]
    [else (set)]))

(define (uncover-live-helper e lak)
  (define lak^ (list->set lak))
  (match e
    ; [`(callq read_int) (set-add lak^^ 'rax)]
    ;[`(negq ,e1) (set-union lak^^ (uncover-live-unwrap e1))
    [`(if (eq? ,e1 ,e2) ,thn ,els) (let* ([thenexpr (instrs-live-helper thn lak^)]
                                          [elseexpr (instrs-live-helper els lak^)]
                                          [thenexpr (if (null? thenexpr) (list '(()) `(,(set))) thenexpr)]
                                          [elseexpr (if (null? elseexpr) (list '(()) `(,(set))) elseexpr)]
                                          [thenset (if (null? thenexpr) (set) (car (last thenexpr)))]
                                          [elseset (if (null? elseexpr) (set) (car (last elseexpr)))]
                                          [thenexpr (list (car thenexpr)
                                                          (if (null? (caar thenexpr)) `(,(set)) (cdr (last thenexpr))))]
                                          [elseexpr (list (car elseexpr)
                                                          (if (null? (caar elseexpr)) `(,(set)) (cdr (last elseexpr))))]
                                          
                                          
                                          )
                                     (list `(if (eq? ,e1 ,e2) ,@thenexpr ,@elseexpr)
                                           (set-union thenset elseset)))]
    [`(movq ,e1 ,e2) #:when(eq? 'offset (car e2)) (list e (set-union lak^
                                                                          (uncover-live-unwrap e1)
                                                                          (uncover-live-unwrap e2)))]
    [`(movq ,e1 ,e2) (list e (set-union (set-subtract lak^ (uncover-live-unwrap e2)) (uncover-live-unwrap e1)))]
    [`(cmpq ,e1 ,e2) (list e (set-union lak^ (uncover-live-unwrap e1) (uncover-live-unwrap e2)))]
    [`(movzbq ,e1 ,e2) (list e (set-subtract lak^ (uncover-live-unwrap e2)))]
    [`(addq ,e1 ,e2) (list e (set-union lak^ (uncover-live-unwrap e1) (uncover-live-unwrap e2)))]
    [`(subq ,e1 ,e2) (list e (set-union lak^ (uncover-live-unwrap e1) (uncover-live-unwrap e2)))]
    [else (list e lak^)]))

;; lak is set
(define (instrs-live-helper e lak)
  (define lak-list (set->list lak))
  (foldr (lambda (x r)
           ;(println (if (null? r) `(,(list->set lak-list)) (cadr r)))
           (let* ([expr (if (null? r) `() (car r))]
                  [lives (if (null? r) `(,(list->set lak-list)) (cadr r))]
                  [helpexpr (uncover-live-helper x (if (null? (cdr lives)) (set->list lak) (car lives)))])
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
    [`(offset ,e1 ,idx) (build-interference-unwrap e1)]
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
                                   (set->list (set-remove caller-save 'r11)))) lak)]
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
  (define preferlist (dropf (map (lambda (v) (lookup v assign-list #f))
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
  (define general-registers (vector 'rbx 'rcx 'rdx 'rsi 'rdi
    				  'r8 'r9 'r10 'r12 
				  'r13 'r14 'r15))
  (define k (vector-length general-registers)) ;; (vector-length general-registers)
  (let ([reglist (filter (lambda (v) (< (cdr v) k)) assign-list)]
        [stacklist (filter (lambda (v) (>= (cdr v) k)) assign-list)])
    (cons `(_stacklength . ,(set-count (list->set (map cdr stacklist)))) ;; a hack way
          (append (map (lambda (v) `(,(car v) . (stack ,(* -8 (add1 (- (cdr v) k)))))) stacklist)
                  (map (lambda (v) `(,(car v) . (reg ,(vector-ref general-registers (cdr v))))) reglist)))))

(define (allocate-var e env)
  (match e
    ;;; consider this situation again, testcase: (let ([x 41]) (+ x 1)) 
    [`(var ,e1) (lookup e1 env '(reg rax))]
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
                       )  '() (cdddr e)))
  ;(define t (if (eq? 1 (length insts)) (car insts) insts))
  `(,(car e) ,(cadr e) ,(caddr e) ,@insts))


; starti == -1
;; (define (assign-homes-env alist starti)
;;   (cond
;;     [(null? alist) '()]
;;     [else (append `((,(car alist) . (stack ,(* 8 starti)))) (assign-homes-env (cdr alist) (sub1 starti)))]))

(define (patch-instr-helper e)
  (match e
    [`(,op (offset (stack ,istack) ,index) ,e2) (append `((movq (stack ,istack) (reg r11)))
                                                          (patch-instr-helper `(,op (offset (reg r11) ,index) ,e2)))]
    ;; e1 if offset stack
    [`(,op ,e1 (offset (stack ,istack) ,index)) (append `((movq (stack ,istack) (reg r11)))
                                                          (patch-instr-helper `(,op ,e1 (offset (reg r11) ,index))))]
    ;; e2 if offset stack
    [`(movq ,e1 (offset ,e2 ,index)) #:when (stack? e1) `((movq ,e1 (reg rax))
                                                          (movq (reg rax) (offset ,e2 ,index)))]
    [`(movq (offset ,e1 ,index) ,e2) #:when (stack? e2) `((movq (offset ,e1 ,index) (reg rax))
                                                          (movq (reg rax) ,e2))]
    [`(movq (global-value ,e1) ,e2) #:when (stack? e2) `((movq (global-value ,e1) (reg r11))
                                                         (movq (reg r11) ,e2))]
    [`(movq ,e1 ,e2) #:when (equal? e1 e2) '()]
    [`(,op (stack ,e1) (stack ,e2)) `((movq (stack ,e1) (reg rax)) (,op (reg rax) (stack ,e2)))]
    [`(movzbq ,e1 ,e2) #:when (stack? e2) `((movzbq ,e1 (reg r11))
                                            (movq (reg r11) ,e2))]
    [`(cmpq ,e1 (global-value ,e2)) #:when (stack? e1) `((movq ,e1 (reg r11))
                                                         (cmpq (reg r11) (global-value ,e2)))]
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
   (format "	movq	%rax, %rdi\n\t")
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





