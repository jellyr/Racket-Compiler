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
(require "pass/uncover-live.rkt")
(require "pass/build-interference.rkt")
(require "pass/allocate-registers.rkt")

(provide r3-passes typechecker)

(define  typechecker
  (curry typecheck-R2 '()))

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





