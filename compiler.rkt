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
(require "pass/reveal-functions.rkt")
(require "pass/flatten.rkt")
(require "pass/expose.rkt")
(require "pass/call-live-roots.rkt")
(require "pass/select-instructions.rkt")
(require "pass/uncover-live.rkt")
(require "pass/build-interference.rkt")
(require "pass/allocate-registers.rkt")
(require "pass/lower-conditionals.rkt")
(require "pass/patch-instructions.rkt")
(require "pass/print-x86.rkt")

(provide r3-passes typechecker)

(define  typechecker
  (curry typecheck-R2 '()))

(define r4-passes `(
                    ("uniquify" ,(uniquify '()) ,interp-scheme)
                    ("reveal-functions" ,reveal-functions ,interp-scheme)
                    ("flattens" ,flattens ,interp-C)
                    ("expose-allocation" ,expose-allocation ,interp-C)
                    ("call-live-roots" ,call-live-roots ,interp-C)
                    ("select instructions" ,select-instructions ,interp-x86)
                    ("uncover-live" ,uncover-live ,interp-x86)
                    ("build interference graph" ,build-interference ,interp-x86)
                    ("register allocation" ,allocate-registers ,interp-x86)
                    ("lower condition" ,lower-conditionals ,interp-x86)
                    ("patch instructions" ,patch-instructions ,interp-x86)
                    ("print x86" ,print-x86 #f)))





