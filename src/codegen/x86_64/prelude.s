global main

extern malloc, free, memcpy

%macro staticstr 2+
    [section .rodata]
    times (1 - ($ - $$) % 2) db 0
    %1: %2
    __?SECT?__
%endmacro

section .text
drop_pop:
    pop rax
    pop rdi
    add rsp, 8
    cmp rdi, 2
    jb .dont_free
    test edi, 1
    jne .dont_free
    push rax
    jmp free
.dont_free:
    jmp rax

cowify:
    pop rax
    pop rdi
    pop rsi
    cmp rdi, 1
    jl .is_false
    jz .is_true
    cmp rdi, 2
    jz .is_number
    push rsi
    push rdi
    jmp rax
.is_false:
    mov rdi, str_false
    mov rsi, 5
    push rsi
    push rdi
    jmp rax
.is_true:
    mov rdi, str_true
    mov rsi, 4
    push rsi
    push rdi
    jmp rax
.is_number:
    mov rax, 60
    mov rdi, 99
    syscall

staticstr str_true, db "true"
staticstr str_false, db "false"
