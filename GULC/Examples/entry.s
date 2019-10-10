.text
.globl _start

_start:
    call _Z4main

    movl %eax, %edi
    movq $60, %rax
    syscall
