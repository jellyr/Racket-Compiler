
    .globl lam20192
lam20192:
    pushq   %rbp
    movq    %rsp, %rbp
    pushq   %r15
    pushq   %r14
    pushq   %r13
    pushq   %r12
    pushq   %rbx
    subq    $0, %rsp
	movq  %rdi, %rax 
	 movq  %rsi, %r12 
	 movq  %rdx, %rbx 
	 movq  16(%r12), %r12 
	 movq  24(%r12), %r13 
	 addq  %rbx, %r12 
	 movq  %r13, %rbx 
	 addq  %r12, %rbx 
	 movq  %rbx, %rax 
	    addq    $0, %rsp
    popq    %rbx
    popq    %r12
    popq    %r13
    popq    %r14
    popq    %r15
    popq    %rbp
    retq
    .globl func20185
func20185:
    pushq   %rbp
    movq    %rsp, %rbp
    pushq   %r15
    pushq   %r14
    pushq   %r13
    pushq   %r12
    pushq   %rbx
    subq    $16, %rsp
	movq  %rdi, %r12 
	 movq  %rsi, %rbx 
	 movq  %rdx, %rbx 
	 movq  $4, -40(%rbp) 
	 leaq  lam20192(%rip), %rax 
	 movq  %rax, -48(%rbp) 
	 movq  free_ptr(%rip), %r13 
	 addq  $32, %r13 
	 cmpq  %r13, fromspace_end(%rip) 
	 setl  %al
	 movzbq  %al, %r13 
	 cmpq  $0, %r13 
	 je  then20496
	 addq  $0, %r12 
	 movq  %r12, %rdi 
	 movq  $32, %rsi 
	 callq  collect
	 jmp  ifend20497
	 then20496:
 ifend20497:
 movq  free_ptr(%rip), %r12 
	 addq  $32, free_ptr(%rip) 
	 movq  $135, 0(%r12) 
	 movq  -48(%rbp), %rax 
	 movq  %rax, 8(%r12) 
	 movq  $46, %r13 
	 movq  -40(%rbp), %rax 
	 movq  %rax, 16(%r12) 
	 movq  $46, %r13 
	 movq  %rbx, 24(%r12) 
	 movq  $46, %rbx 
	 movq  %r12, %rax 
	    addq    $16, %rsp
    popq    %rbx
    popq    %r12
    popq    %r13
    popq    %r14
    popq    %r15
    popq    %rbp
    retq
    .globl main
main:
    pushq   %rbp
    movq    %rsp, %rbp
    pushq   %r15
    pushq   %r14
    pushq   %r13
    pushq   %r12
    pushq   %rbx
    subq    $0, %rsp
	movq  $10000, %rdi 
	 movq  $10000, %rsi 
	 callq  initialize
	 movq  rootstack_begin(%rip), %rbx 
	 leaq  func20185(%rip), %r13 
	 movq  free_ptr(%rip), %rcx 
	 addq  $16, %rcx 
	 cmpq  %rcx, fromspace_end(%rip) 
	 setl  %al
	 movzbq  %al, %rcx 
	 cmpq  $0, %rcx 
	 je  then20388
	 movq  %rbx, %rcx 
	 addq  $0, %rcx 
	 movq  %rcx, %rdi 
	 movq  $16, %rsi 
	 callq  collect
	 jmp  ifend20389
	 then20388:
 ifend20389:
 movq  free_ptr(%rip), %rdx 
	 addq  $16, free_ptr(%rip) 
	 movq  $3, 0(%rdx) 
	 movq  %r13, 8(%rdx) 
	 movq  $46, %rcx 
	 movq  8(%rdx), %r8 
	 movq  %rbx, %rdi 
	 movq  %rdx, %rsi 
	 movq  $5, %rdx 
	 callq *%r8
	 movq  %rax, %r13 
	 leaq  func20185(%rip), %r14 
	 movq  free_ptr(%rip), %rcx 
	 addq  $16, %rcx 
	 cmpq  %rcx, fromspace_end(%rip) 
	 setl  %al
	 movzbq  %al, %rcx 
	 cmpq  $0, %rcx 
	 je  then20319
	 movq  %rbx, %rcx 
	 addq  $0, %rcx 
	 movq  %rcx, %rdi 
	 movq  $16, %rsi 
	 callq  collect
	 jmp  ifend20320
	 then20319:
 ifend20320:
 movq  free_ptr(%rip), %rdx 
	 addq  $16, free_ptr(%rip) 
	 movq  $3, 0(%rdx) 
	 movq  %r14, 8(%rdx) 
	 movq  $46, %rcx 
	 movq  8(%rdx), %r8 
	 movq  %rbx, %rdi 
	 movq  %rdx, %rsi 
	 movq  $3, %rdx 
	 callq *%r8
	 movq  %rax, %r14 
	 movq  %r13, %rdx 
	 movq  8(%rdx), %r8 
	 movq  %rbx, %rdi 
	 movq  %rdx, %rsi 
	 movq  $11, %rdx 
	 callq *%r8
	 movq  %rax, %r13 
	 movq  %r14, %rdx 
	 movq  8(%rdx), %r8 
	 movq  %rbx, %rdi 
	 movq  %rdx, %rsi 
	 movq  $15, %rdx 
	 callq *%r8
	 movq  %rax, %rbx 
	 movq  %r13, %rcx 
	 addq  %rbx, %rcx 
	 movq  %rcx, %rax 
	    movq    %rax, %rdi
	   callq print_int
	    movq    $0, %rax
    addq    $0, %rsp
    popq    %rbx
    popq    %r12
    popq    %r13
    popq    %r14
    popq    %r15
    popq    %rbp   
    retq
