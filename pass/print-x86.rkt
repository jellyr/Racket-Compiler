#lang racket
(require "../common.rkt")

(provide print-x86)

(define (print-helper e )
  (match e
    [`(stack ,e1) (format "~a(%rbp)" e1)]
    [`(type ,e1) ""]
    [`(int ,e1) (format "$~a" e1)]
    [`(reg ,e1) (format "%~a" e1)]
    [`(global-value ,e1) (format "~a(%rip)" e1)]
    [`(byte-reg ,e1) (format "%~a" e1)]
    [`(label ,label) (format "~a:\n" label)]
    [`(function-ref ,label) (format "~a(%rip)" label)]
    [`(indirect-callq ,arg) (format "callq *~a\n\t" (print-helper arg))]
    [`(movq (stack-arg ,i) ,e2) (string-append (format "movq  ~a(%rbp), " (+ 16 i)) (print-helper e2) " \n\t")]
    [`(movq ,e1 (stack-arg ,i)) (string-append "movq  " (print-helper e1) ", " (format "~a(%rsp)" i) " \n\t")]
    [`(offset ,reg ,index) (format "~a(~a)" index (print-helper reg))]
    [`(,op ,e1) (string-append (format "~a  " op) (print-helper e1) "\n\t")]
    [`(,op ,e1 ,e2) (string-append (format "~a  " op) (print-helper e1) ", " (print-helper e2)" \n\t")]
    ;[`(callq initialize) (format "callq initialize\n")] ;; this can be else
    ;[`(negq ,e1) (string-append "negq  " (print-helper e1) " \n\t" )]
    ;[`(callq ,e1) (string-append "callq    " (print-helper e1) " \n\t" )]
    ;[`(jmp ,e1) (string-append "jmp    " (print-helper e1) "\n\t")]
    ;[`(movq ,e1 ,e2) (string-append "movq  " (print-helper e1) ", " (print-helper e2)" \n\t")]
    ;[`(addq ,e1 ,e2) (string-append "addq  " (print-helper e1) ", " (print-helper e2) " \n\t")]
    ;[`(xorq ,e1 ,e2)]
    [else (format "~s" e)]
    ))

(define (callq-helper typexpr)
  (match typexpr
      ['Integer "   callq print_int\n\t"]
      ['Boolean "   callq print_bool\n\t"]
      ['Void "  callq print_void\n\t"]
      [`(Vector . ,typexpr1) (string-append
                              (format " callq print_vecbegin\n\t")
                              (foldr (lambda (v r)
                                       (string-append (callq-helper v) (format  "   callq print_space\n\t"))) "" (cdr (reverse typexpr1)))
                              (callq-helper (last typexpr1))
                              (format " callq print_vecend\n\t"))]))

;; work on this one
(define (def-helper e)
  (match-define `(define (,funame) ,st-arg ,st-var . ,instrs) e)
  (define len (+ st-arg st-var))
  (string-append
   (format "\n    .globl ~a
~a:
    pushq   %rbp
    movq    %rsp, %rbp
    pushq   %r15
    pushq   %r14
    pushq   %r13
    pushq   %r12
    pushq   %rbx
    subq    $~a, %rsp\n\t" funame funame (* 8 len))
   (string-join (map print-helper instrs))
   (format "    addq    $~a, %rsp
    popq    %rbx
    popq    %r12
    popq    %r13
    popq    %r14
    popq    %r15
    popq    %rbp
    retq" (* 8 len))))

(define (print-x86 e)
  (match-define `(program ,len (type ,type) (defines . ,defs) . ,instrs) e)
  ;(define len (+ st-arg st-var))
  (string-append
   (foldr string-append "\n" (map def-helper defs))
   (format "    .globl main
main:
    pushq   %rbp
    movq    %rsp, %rbp
    pushq   %r15
    pushq   %r14
    pushq   %r13
    pushq   %r12
    pushq   %rbx
    subq    $~a, %rsp\n\t" (* 8 len))
   (string-join (map print-helper instrs))
   (format "    movq    %rax, %rdi\n\t")
   (callq-helper type)
   (format "    movq    $0, %rax
    addq    $~a, %rsp
    popq    %rbx
    popq    %r12
    popq    %r13
    popq    %r14
    popq    %r15
    popq    %rbp   
    retq" (* 8 len))))
