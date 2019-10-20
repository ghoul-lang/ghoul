.text
.globl _start

_start:
    call _Z4mainv

    movl %eax, %edi
    movq $60, %rax
    syscall
