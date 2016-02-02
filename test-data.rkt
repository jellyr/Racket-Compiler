Welcome to Racket v6.3.
﻿> 
compiler.rkt﻿> 
compiler.rkt﻿> 
compiler.rkt﻿> (define a 'x)
compiler.rkt﻿> (set! a 'd)
compiler.rkt﻿> a
'd
compiler.rkt﻿> 
compiler.rkt﻿> 
compiler.rkt﻿> 
compiler.rkt﻿> (define data '(program (let ([x (+ (read) (read))]) x)))
compiler.rkt﻿> ((uniquify '()) data)
'(program (let ((x42266 (+ (read) (read)))) x42266))
compiler.rkt﻿> (faltten '(program (let ((x42266 (+ (read) (read)))) x42266))
)
; faltten: undefined;
;  cannot reference an identifier before its definition
;   in module: "/Users/yang/workspace/grad/523/P423-523/compiler.rkt"
compiler.rkt﻿> (flatten '(program (let ((x42266 (+ (read) (  C-c C-c; user break
; Context:
;  /Users/yang/.emacs.d/elpa/racket-mode-20150822.2002/cmds.rkt:50:0
compiler.rkt﻿> 
﻿> 
compiler.rkt﻿> (flatten '(program (let ((x42266 (+ (read) (read)))) x42266)))
'(program
  (x42266 g48637 g48638)
  (assign g48637 (read))
  (assign g48638 (read))
  (assign x42266 (+ g48637 g48638))
  (return x42266))
compiler.rkt﻿> (select-instructions '(program
  (x42266 g48637 g48638)
  (assign g48637 (read))
  (assign g48638 (read))
  (assign x42266 (+ g48637 g48638))
  (return x42266)))
'(program
  (x42266 g48637 g48638)
  (callq read_int)
  (movq (reg rax) (var g48637))
  (callq read_int)
  (movq (reg rax) (var g48638))
  (movq (var g48637) (var x42266))
  (addq (var g48638) (var x42266))
  (movq (var x42266) (reg rax)))
compiler.rkt﻿> (uncover-live '(program
  (x42266 g48637 g48638)
  (callq read_int)
  (movq (reg rax) (var g48637))
  (callq read_int)
  (movq (reg rax) (var g48638))
  (movq (var g48637) (var x42266))
  (addq (var g48638) (var x42266))
  (movq (var x42266) (reg rax))))
'(program
  ((x42266 g48637 g48638)
   ((rax) () (g48637 rax) (g48637 g48638) (g48638 x42266) (x42266) ()))
  (callq read_int)
  (movq (reg rax) (var g48637))
  (callq read_int)
  (movq (reg rax) (var g48638))
  (movq (var g48637) (var x42266))
  (addq (var g48638) (var x42266))
  (movq (var x42266) (reg rax)))
compiler.rkt﻿> (build-interference '(program
  ((x42266 g48637 g48638)
   ((rax) () (g48637 rax) (g48637 g48638) (g48638 x42266) (x42266) ()))
  (callq read_int)
  (movq (reg rax) (var g48637))
  (callq read_int)
  (movq (reg rax) (var g48638))
  (movq (var g48637) (var x42266))
  (addq (var g48638) (var x42266))
  (movq (var x42266) (reg rax))))
(list
 'program
 (list
  '(x42266 g48637 g48638)
  (hash
   'rcx
   (set 'g48638 'g48637)
   'rdx
   (set 'g48638 'g48637)
   'g48638
   (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10 'r11 'x42266 'g48637)
   'x42266
   (set 'g48638)
   'r11
   (set 'g48638 'g48637)
   'r10
   (set 'g48638 'g48637)
   'r9
   (set 'g48638 'g48637)
   'r8
   (set 'g48638 'g48637)
   'g48637
   (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10 'r11 'g48638)
   'rdi
   (set 'g48638 'g48637)
   'rax
   (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10 'r11)
   'rsi
   (set 'g48638 'g48637)))
 '(callq read_int)
 '(movq (reg rax) (var g48637))
 '(callq read_int)
 '(movq (reg rax) (var g48638))
 '(movq (var g48637) (var x42266))
 '(addq (var g48638) (var x42266))
 '(movq (var x42266) (reg rax)))
compiler.rkt﻿> (allocate-registers '(program
  ((x42266 g48637 g48638)
   ((rax) () (g48637 rax) (g48637 g48638) (g48638 x42266) (x42266) ()))
  (callq read_int)
  (movq (reg rax) (var g48637))
  (callq read_int)
  (movq (reg rax) (var g48638))
  (movq (var g48637) (var x42266))
  (addq (var g48638) (var x42266))
  (movq (var x42266) (reg rax))))
; hash->list: contract violation
;   expected: hash?
;   given: '((rax) () (g48637 rax) (g48637 g48638) (g48638 x42266) (x42266) ())
; Context:
;  /Users/yang/workspace/grad/523/P423-523/compiler.rkt:168:0 highest-saturation
;  /Users/yang/workspace/grad/523/P423-523/compiler.rkt:195:0 allocate-registers-helper
;  /Users/yang/workspace/grad/523/P423-523/compiler.rkt:240:0 allocate-registers
compiler.rkt﻿> (allocate-registers '(program
  ((x42266 g48637 g48638)
   ((rax) () (g48637 rax) (g48637 g48638) (g48638 x42266) (x42266) ()))
  (callq read_int)
  (movq (reg rax) (var g48637))
  (callq read_int)
  (movq (reg rax) (var g48638))
  (movq (var g48637) (var x42266))
  (addq (var g48638) (var x42266))
  (movq (var x42266) (reg rax))))
; hash->list: contract violation
;   expected: hash?
;   given: '((rax) () (g48637 rax) (g48637 g48638) (g48638 x42266) (x42266) ())
; Context:
;  /Users/yang/workspace/grad/523/P423-523/compiler.rkt:168:0 highest-saturation
;  /Users/yang/workspace/grad/523/P423-523/compiler.rkt:195:0 allocate-registers-helper
;  /Users/yang/workspace/grad/523/P423-523/compiler.rkt:240:0 allocate-registers
compiler.rkt﻿> ((allocate-registers (list
 'program
 (list
  '(x42266 g48637 g48638)
  (hash
   'rcx
   (set 'g48638 'g48637)
   'rdx
   (set 'g48638 'g48637)
   'g48638
   (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10 'r11 'x42266 'g48637)
   'x42266
   (set 'g48638)
   'r11
   (set 'g48638 'g48637)
   'r10
   (set 'g48638 'g48637)
   'r9
   (set 'g48638 'g48637)
   'r8
   (set 'g48638 'g48637)
   'g48637
   (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10 'r11 'g48638)
   'rdi
   (set 'g48638 'g48637)
   'rax
   (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10 'r11)
   'rsi
   (set 'g48638 'g48637)))
 '(callq read_int)
 '(movq (reg rax) (var g48637))
 '(callq read_int)
 '(movq (reg rax) (var g48638))
 '(movq (var g48637) (var x42266))
 '(addq (var g48638) (var x42266))
 '(movq (var x42266) (reg rax)))))
; application: not a procedure;
;  expected a procedure that can be applied to arguments
;   given: '(program 0 (callq read_int) (movq (reg rax) (reg rcx)) (callq read_int) (movq (reg rax) (reg rbx)) (movq (reg rcx) (reg rcx)) (addq (reg rbx) (reg rcx)) (movq (reg rcx) (reg rax)))
;   arguments...: [none]
compiler.rkt﻿> (allocate-registers '(program
  ((x42266 g48637 g48638)
   ((rax) () (g48637 rax) (g48637 g48638) (g48638 x42266) (x42266) ()))
  (callq read_int)
  (movq (reg rax) (var g48637))
  (callq read_int)
  (movq (reg rax) (var g48638))
  (movq (var g48637) (var x42266))
  (addq (var g48638) (var x42266))
  (movq (var x42266) (reg rax))))
; hash->list: contract violation
;   expected: hash?
;   given: '((rax) () (g48637 rax) (g48637 g48638) (g48638 x42266) (x42266) ())
; Context:
;  /Users/yang/workspace/grad/523/P423-523/compiler.rkt:168:0 highest-saturation
;  /Users/yang/workspace/grad/523/P423-523/compiler.rkt:195:0 allocate-registers-helper
;  /Users/yang/workspace/grad/523/P423-523/compiler.rkt:240:0 allocate-registers
compiler.rkt﻿> (allocate-registers (list
 'program
 (list
  '(x42266 g48637 g48638)
  (hash
   'rcx
   (set 'g48638 'g48637)
   'rdx
   (set 'g48638 'g48637)
   'g48638
   (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10 'r11 'x42266 'g48637)
   'x42266
   (set 'g48638)
   'r11
   (set 'g48638 'g48637)
   'r10
   (set 'g48638 'g48637)
   'r9
   (set 'g48638 'g48637)
   'r8
   (set 'g48638 'g48637)
   'g48637
   (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10 'r11 'g48638)
   'rdi
   (set 'g48638 'g48637)
   'rax
   (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10 'r11)
   'rsi
   (set 'g48638 'g48637)))
 '(callq read_int)
 '(movq (reg rax) (var g48637))
 '(callq read_int)
 '(movq (reg rax) (var g48638))
 '(movq (var g48637) (var x42266))
 '(addq (var g48638) (var x42266))
 '(movq (var x42266) (reg rax))))
'(program
  0
  (callq read_int)
  (movq (reg rax) (reg rcx))
  (callq read_int)
  (movq (reg rax) (reg rbx))
  (movq (reg rcx) (reg rcx))
  (addq (reg rbx) (reg rcx))
  (movq (reg rcx) (reg rax)))
compiler.rkt﻿> 