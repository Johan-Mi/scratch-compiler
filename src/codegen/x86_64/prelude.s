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

str_length:
    mov rax, ~0
.loop:
    inc rax
    test rsi, rsi
    jz .done
    dec rsi
    test byte [rdi], 0x80
    jz .one_byte
    cmp byte [rdi], 0b11000000
    jbe .two_bytes
    cmp byte [rdi], 0b11100000
    jbe .three_bytes
    add rsi, 4
    jmp .loop
.one_byte:
    inc rdi
    jmp .loop
.two_bytes:
    add rdi, 2
    jmp .loop
.three_bytes:
    add rdi, 3
    jmp .loop
.done:
    ret

usize_to_double:
    movq xmm0, rdi
    punpckldq xmm0, [.LCPI0_0]
    subpd xmm0, [.LCPI0_1]
    movapd xmm1, xmm0
    unpckhpd xmm1, xmm0
    addsd xmm1, xmm0
    movq rax, xmm1
    ret
.LCPI0_0:
    dd 1127219200
    dd 1160773632
    dd 0
    dd 0
.LCPI0_1:
    dq 0x4330000000000000
    dq 0x4530000000000000
