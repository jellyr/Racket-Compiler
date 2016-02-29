#lang racket
(require "../common.rkt")

(provide print-x86)

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

(define (print-x86 e)
  (let ([type (caddr e)])
    (string-append
   (format "    .globl main
main:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $~a, %rsp\n\t" (* 8 (cadr e)))
   (string-join (map print-helper (cdddr e)))
   (format "    movq    %rax, %rdi\n\t")
   (callq-helper (cadr type))
   (format "    movq    $0, %rax
    addq    $~a, %rsp
    popq    %rbp
    retq" (* 8 (cadr e))))))