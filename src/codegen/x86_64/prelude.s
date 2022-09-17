global main

extern malloc, free, memcpy

%macro staticstr 2+
    [section .rodata]
    times (1 - ($ - $$) % 2) db 0
    %1: %2
    __?SECT?__
%endmacro

section .text
drop_any:
    cmp rdi, 2
    jbe .dont_free
    test edi, 1
    jnz .dont_free
    jmp free
.dont_free:
    ret

drop_pop_cow:
    pop rax
    pop rdi
    add rsp, 8
    test rdi, 1
    jnz .dont_free
    push rax
    jmp free
.dont_free:
    jmp rax

any_to_cow:
    cmp rdi, 1
    jb .is_false
    je .is_true
    cmp rdi, 2
    je .is_number
    mov rax, rdi
    mov rdx, rsi
    ret
.is_false:
    mov rax, str_false
    mov rdx, 5
    ret
.is_true:
    mov rax, str_true
    mov rdx, 4
    ret
.is_number:
    movq xmm0, rsi
    jmp double_to_cow

staticstr str_true, db "true"
staticstr str_false, db "false"
staticstr str_empty, db ""

str_length:
    mov rax, ~0
.loop:
    inc rax
    test rsi, rsi
    jz .done
    test byte [rdi], 0x80
    jz .one_byte
    cmp byte [rdi], 0b11011111
    jbe .two_bytes
    cmp byte [rdi], 0b11101111
    jbe .three_bytes
    add rdi, 4
    sub rsi, 4
    jmp .loop
.one_byte:
    inc rdi
    dec rsi
    jmp .loop
.two_bytes:
    add rdi, 2
    sub rsi, 2
    jmp .loop
.three_bytes:
    add rdi, 3
    sub rsi, 3
    jmp .loop
.done:
    ret

char_at:
    test rdx, rdx
    jz .return_empty_string
.loop:
    test rsi, rsi
    jz .return_empty_string
    dec rdx
    test rdx, rdx
    jz .found_correct_index
    test byte [rdi], 0x80
    jz .one_byte
    test byte [rdi], 0b00100000
    jz .two_bytes
    test byte [rdi], 0b00010000
    jz .three_bytes
    add rdi, 4
    sub rsi, 4
    jmp .loop
.one_byte:
    inc rdi
    dec rsi
    jmp .loop
.two_bytes:
    add rdi, 2
    sub rsi, 2
    jmp .loop
.three_bytes:
    add rdi, 3
    sub rsi, 3
    jmp .loop
.found_correct_index:
    push rdi
    mov rdi, 4
    call malloc
    pop rdi
    test byte [rdi], 0x80
    jz .write_one_byte
    test byte [rdi], 0b00100000
    jz .write_two_bytes
    test byte [rdi], 0b00010000
    jz .write_three_bytes
    mov edi, [rdi]
    mov dword [rax], edi
    mov rdx, 4
    ret
.write_one_byte:
    mov dil, [rdi]
    mov [rax], dil
    mov rdx, 1
    ret
.write_two_bytes:
    mov di, [rdi]
    mov [rax], di
    mov rdx, 2
    ret
.write_three_bytes:
    mov si, [rdi]
    mov [rax], si
    mov dil, [rdi+2]
    mov [rax+2], dil
    mov rdx, 3
    ret
.return_empty_string:
    mov rax, str_empty
    mov rdx, 0
    ret

usize_to_double:
    movq xmm1, rdi
    punpckldq xmm1, [.LCPI0_0]
    subpd xmm1, [.LCPI0_1]
    movapd xmm0, xmm1
    unpckhpd xmm0, xmm1
    addsd xmm0, xmm1
    ret
align 16
.LCPI0_0:
    dd 1127219200
    dd 1160773632
    dd 0
    dd 0
.LCPI0_1:
    dq 0x4330000000000000
    dq 0x4530000000000000

any_to_bool:
    mov rax, [rsp+8]
    cmp rax, 2
    jb .done
    je .is_number
    test qword [rsp+16], 1
    jnz .might_be_str_0
    cmp qword [rsp+16], 5
    je .might_be_str_false
    test qword [rsp+16], ~0
    setnz al
.done:
    ret
.might_be_str_0:
    cmp byte [rax], '0'
    setne al
    ret
.might_be_str_false:
    mov edi, [rax]
    and edi, ~0x20202020
    cmp edi, "FALS"
    setne al
    mov dil, [rax+4]
    and dil, ~0x20
    cmp dil, 'E'
    setne dil
    or al, dil
    ret
.is_number:
    movq xmm0, [rsp+16]
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    setne al
    ret

any_to_double:
    cmp rdi, 2
    je .is_number
    cmp rdi, 1
    je .is_true
    jb .is_false
    mov rax, 60
    mov rdi, 98
    syscall
.is_number:
    movq xmm0, rsi
    ret
.is_true:
    mov rax, __?float64?__(1.0)
    movq xmm0, rax
    ret
.is_false:
    xorpd xmm0, xmm0
    ret

double_to_usize:
    cvttsd2si rax, xmm0
    mov rcx, rax
    sar rcx, 63
    movapd xmm1, xmm0
    subsd xmm1, [.LCPI0_0]
    cvttsd2si rdx, xmm1
    and rdx, rcx
    or rdx, rax
    xor ecx, ecx
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    cmovae rcx, rdx
    ucomisd xmm0, [.LCPI0_1]
    mov rax, -1
    cmovbe rax, rcx
    ret
align 8
.LCPI0_0: dq 0x43e0000000000000
.LCPI0_1: dq 0x43efffffffffffff

clone_any:
    cmp rdi, 2
    jbe .done
    push rsi
    push rdi
    mov rdi, rsi
    call malloc
    mov rdi, rax
    pop rsi
    mov rdx, [rsp]
    call memcpy
    pop rdx
    ret
.done:
    mov rax, rdi
    mov rdx, rsi
    ret

double_to_cow:
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    jp .is_nan
    je .is_zero
    movq rdi, xmm0
    mov rax, ((1 << 11) - 1) << 52
    cmp rdi, rax
    je .is_infinity
    mov rax, ((1 << 12) - 1) << 52
    cmp rdi, rax
    je .is_minus_infinity
    ; TODO
    mov rax, 1
    mov rdi, 97
    syscall
.is_infinity:
    mov rax, str_Infinity
    mov rdx, 8
    ret
.is_minus_infinity:
    mov rax, str_minus_Infinity
    mov rdx, 9
    ret
.is_zero:
    mov rax, str_0
    mov rdx, 1
    ret
.is_nan:
    mov rax, str_NaN
    mov rdx, 3
    ret

staticstr str_Infinity, db "Infinity"
staticstr str_minus_Infinity, db "-Infinity"
staticstr str_0, db "0"
staticstr str_NaN, db "NaN"
