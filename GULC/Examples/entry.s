.text
.globl _start

_start:
    call main

    movl %eax, %edi
    movq $60, %rax
    syscall
