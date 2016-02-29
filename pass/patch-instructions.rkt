#lang racket
(require "../common.rkt")

(provide patch-instructions)


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