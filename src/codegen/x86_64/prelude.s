default rel

global main

extern malloc, free, memcpy, memmove, realloc, asprintf, srand48, drand48, time, getline, stdin, memcmp
extern log, log10, exp, exp10, sin, cos, tan, asin, acos, atan, fmod

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
    test dil, 1
    jz free wrt ..plt
.dont_free:
    ret

drop_pop_cow:
    pop rax
    pop rdi
    mov [rsp], rax
drop_cow:
    test dil, 1
    jz free wrt ..plt
    ret

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
    lea rax, [str_false]
    mov rdx, 5
    ret
.is_true:
    lea rax, [str_true]
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
    call malloc wrt ..plt
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
    lea rax, [str_empty]
    xor edx, edx
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
    cmp rdi, 2
    jb .done
    je .is_number
    push qword 0
    cmp rsi, 5
    je .might_be_str_false
    cmp rsi, 1
    je .might_be_str_0
    seta [rsp]
.drop_parameter:
    test dil, 1
    jnz .dont_free
    call free wrt ..plt
.dont_free:
    pop rax
.done:
    ret
.might_be_str_0:
    cmp byte [rdi], '0'
    setne [rsp]
    jmp .drop_parameter
.might_be_str_false:
    mov edx, [rdi]
    and edx, ~0x20202020
    cmp edx, "FALS"
    setne [rsp]
    mov dl, [rdi+4]
    and dl, ~0x20
    cmp dl, 'E'
    setne al
    or [rsp], al
    jmp .drop_parameter
.is_number:
    xor eax, eax
    movq xmm0, rsi
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
    test dil, 1
    jnz str_to_double
    push rdi
    call str_to_double
    mov rdi, [rsp]
    movsd [rsp], xmm0
    call free wrt ..plt
    movsd xmm0, [rsp]
    add rsp, 8
    ret
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
    test dil, 1
    jnz .done
    sub rsp, 8
    push rsi
    push rdi
    mov rdi, rsi
    call malloc wrt ..plt
    mov rdi, rax
    mov rsi, [rsp]
    mov rdx, [rsp+8]
    call memcpy wrt ..plt
    mov rdx, [rsp+8]
    add rsp, 24
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
    mov rax, __?float64?__(__?Infinity?__)
    cmp rdi, rax
    je .is_infinity
    mov rax, __?float64?__(-__?Infinity?__)
    cmp rdi, rax
    je .is_minus_infinity
    sub rsp, 8
    mov rdi, rsp
    mov eax, 1
    lea rsi, [.fmt]
    call asprintf wrt ..plt
    mov rdx, rax
    pop rax
    ret
.is_infinity:
    lea rax, [str_Infinity]
    mov rdx, 8
    ret
.is_minus_infinity:
    lea rax, [str_minus_Infinity]
    mov rdx, 9
    ret
.is_zero:
    lea rax, [str_0]
    mov rdx, 1
    ret
.is_nan:
    lea rax, [str_NaN]
    mov rdx, 3
    ret
.fmt: db "%g", 0

staticstr str_Infinity, db "Infinity"
staticstr str_minus_Infinity, db "-Infinity"
staticstr str_0, db "0"
staticstr str_NaN, db "NaN"

list_ensure_extra_capacity:
    mov rax, rdi
    mov rsi, [rdi+16]
    cmp [rdi+8], rsi
    jb .done
    push rdi
    shl rsi, 5
    add rsi, 16
    mov rdi, [rdi]
    call realloc wrt ..plt
    mov rdi, rax
    pop rax
    mov [rax], rdi
    shl qword [rax+16], 1
    inc qword [rax+16]
.done:
    ret

list_append:
    sub rsp, 8
    push rdx
    push rsi
    call list_ensure_extra_capacity
    mov rdi, [rax+8]
    shl rdi, 4
    mov rsi, [rax]
    pop qword [rsi+rdi]
    pop qword [rsi+rdi+8]
    inc qword [rax+8]
    add rsp, 8
    ret

list_get:
    cmp rdi, 2
    jbe .numeric_index
    cmp rsi, 4
    jne .numeric_index
    mov eax, [rdi]
    and eax, ~0x20202020
    cmp eax, "LAST"
    jne .numeric_index
    test dil, 1
    jnz .dont_free
    push rdx
    call free wrt ..plt
    pop rdx
.dont_free:
    cmp qword [rdx+8], 0
    jz .out_of_bounds
    mov rax, [rdx+8]
    shl rax, 4
    mov rsi, [rdx]
    mov rdi, [rsi+rax-16]
    mov rsi, [rsi+rax-8]
    jmp clone_any
.numeric_index:
    push rdx
    call any_to_double
    call double_to_usize
    pop rdx
    sub rax, 1
    jc .out_of_bounds
    cmp rax, [rdx+8]
    jae .out_of_bounds
    shl rax, 4
    mov rsi, [rdx]
    mov rdi, [rsi+rax]
    mov rsi, [rsi+rax+8]
    jmp clone_any
.out_of_bounds:
    lea rax, [str_empty]
    xor rdx, rdx
    ret

list_delete:
    cmp rdi, 2
    jbe .numeric_index
    cmp rsi, 4
    jne .numeric_index
    mov eax, [rdi]
    and eax, ~0x20202020
    cmp eax, "LAST"
    jne .numeric_index
    test dil, 1
    jnz .dont_free
    push rdx
    call free wrt ..plt
    pop rdx
.dont_free:
    mov rax, [rdx+8]
    sub rax, 1
    jc .done
    mov [rdx+8], rax
    shl rax, 4
    mov rsi, [rdx]
    mov rdi, [rsi+rax]
    jmp drop_any
.numeric_index:
    push rdx
    call any_to_double
    call double_to_usize
    pop rdx
    sub rax, 1
    jc .done
    cmp rax, [rdx+8]
    jae .done
    mov rax, [rdx+8]
    shl rax, 4
    mov rsi, [rdx]
    mov rdi, [rsi+rax]
    push rdx
    push rax
    sub rsp, 8
    call drop_any
    add rsp, 8
    pop rax
    pop rdx
    dec qword [rdx+8]
    shl rax, 4
    add rax, [rdx]
    mov rdi, rax
    lea rsi, [rdi+16]
    mov rdx, [rdx+8]
    sub rdx, rax
    jmp memmove wrt ..plt
.done:
    ret

list_delete_all:
    push rbx
    mov rbx, rdi
.loop:
    sub qword [rbx+8], 1
    jc .done
    mov rax, [rbx+8]
    shl rax, 4
    mov rsi, [rbx]
    mov rdi, [rsi+rax]
    call drop_any
    jmp .loop
.done:
    inc qword [rbx+8]
    pop rbx
    ret

list_replace:
    cmp rdi, 2
    jbe .numeric_index
    cmp rsi, 4
    jne .numeric_index
    mov eax, [rdi]
    and eax, ~0x20202020
    cmp eax, "LAST"
    jne .numeric_index
    test dil, 1
    jnz .dont_free_index
    push r8
    push rcx
    push rdx
    call free wrt ..plt
    pop rdx
    pop rcx
    pop r8
.dont_free_index:
    mov rax, [r8+8]
    jmp .do_it
.numeric_index:
    push r8
    push rcx
    push rdx
    call any_to_double
    call double_to_usize
    pop rdx
    pop rcx
    pop r8
    cmp rax, [r8+8]
    ja .out_of_bounds
.do_it:
    sub rax, 1
    jc .out_of_bounds
    shl rax, 4
    add rax, [r8]
    mov rdi, [rax]
    mov [rax], rdx
    mov [rax+8], rcx
    jmp drop_any
.out_of_bounds:
    mov rdi, rdx
    jmp drop_any

any_eq_str:
    cmp rdi, 2
    jbe .todo
    test dil, 1
    jnz str_eq_str
    push rdi
    call str_eq_str
    pop rdi
    push rax
    call free wrt ..plt
    pop rax
    ret
.todo:
    ; TODO
    mov eax, 60
    mov edi, 96
    syscall

any_lt_str:
    ; TODO
    mov eax, 60
    mov edi, 95
    syscall

any_eq_double:
    xor eax, eax
    cmp rdi, 2
    jb .done
    ja .is_cow
    movq xmm1, rsi
    ucomisd xmm0, xmm1
    sete al
.done:
    ret
.is_cow:
    sub rsp, 8
    movsd [rsp], xmm0
    push rsi
    push rdi
    call str_to_double
    xor eax, eax
    ucomisd xmm0, [rsp+16]
    sete al
    mov [rsp+16], rax
    call drop_pop_cow
    pop rax
    ret

any_lt_double:
    xor eax, eax
    cmp rdi, 2
    ja .is_cow
    jb .is_bool
    movq xmm1, rsi
    ucomisd xmm0, xmm1
    seta al
    ret
.is_bool:
    ucomisd xmm0, [.inf]
    sete al
    andn eax, edi, eax
    ret
.is_cow:
    ; TODO
    mov eax, 60
    mov edi, 93
    syscall
align 8
.inf: dq __?Infinity?__

double_lt_any:
    xor eax, eax
    cmp rdi, 2
    ja .is_cow
    jb .is_bool
    movq xmm1, rsi
    ucomisd xmm0, xmm1
    setb al
    ret
.is_bool:
    mov edx, 1
    mov eax, edi
    ucomisd xmm0, [.inf]
    cmovne eax, edx
    ret
.is_cow:
    test dil, 1
    jnz double_lt_str
    push rdi
    call double_lt_str
    pop rdi
    push rax
    call free wrt ..plt
    pop rax
    ret
align 8
.inf: dq __?Infinity?__

any_eq_any:
    cmp rdi, 2
    ja .first_is_cow
    jb .todo
    cmp rdx, 2
    ja .double_and_cow
    jb .todo
    xor eax, eax
    movq xmm0, rsi
    movq xmm1, rcx
    ucomisd xmm0, xmm1
    sete al
    ret
.double_and_cow:
    movq xmm0, rsi
    mov rdi, rdx
    mov rsi, rcx
    test dil, 1
    jnz str_eq_double
    jmp .todo
.first_is_cow:
    cmp rdx, 2
    jb .todo
    je .cow_and_number
    sub rsp, 8
    push rdi
    push rcx
    call str_eq_str
    mov [rsp+16], rax
    mov rdi, [rsp]
    call drop_cow
    add rsp, 8
    pop rdi
    call drop_cow
    pop rax
    ret
.cow_and_number:
    movq xmm0, rcx
    test dil, 1
    jnz str_eq_double
.todo:
    ; TODO
    mov eax, 60
    mov edi, 92
    syscall

any_lt_any:
    cmp rdi, 2
    ja .first_is_cow
    jb .todo
    cmp rdx, 2
    ja .double_and_cow
    jb .todo
    xor eax, eax
    movq xmm0, rsi
    movq xmm1, rcx
    ucomisd xmm0, xmm1
    setb al
    ret
.double_and_cow:
    movq xmm0, rsi
    mov rdi, rdx
    mov rsi, rcx
    test dil, 1
    jnz double_lt_str
    jmp .todo
.first_is_cow:
    cmp rdx, 2
    jne .todo
    movq xmm0, rcx
    test dil, 1
    jnz str_lt_double
.todo:
    ; TODO
    mov eax, 60
    mov edi, 91
    syscall

any_eq_bool:
    test dl, dl
    jz any_eq_false
any_eq_true:
    xor eax, eax
    cmp rdi, 2
    cmovb eax, edi
    jbe .done
    cmp rsi, 4
    jne .drop_parameter
    mov edx, [rdi]
    and edx, ~0x20202020
    cmp edx, "TRUE"
    sete al
.drop_parameter:
    test dil, 1
    jnz .done
    push rax
    call free wrt ..plt
    pop rax
.done:
    ret

any_eq_false:
    xor eax, eax
    mov edx, edi
    xor dl, 1
    cmp rdi, 2
    cmovb eax, edx
    jbe .done
    cmp rsi, 5
    jne .drop_parameter
    mov edx, [rdi]
    and edx, ~0x20202020
    cmp edx, "FALS"
    jne .drop_parameter
    mov dl, [rdi]
    and dl, ~0x20
    cmp dl, "E"
    sete al
.drop_parameter:
    test dil, 1
    jnz .done
    push rax
    call free wrt ..plt
    pop rax
.done:
    ret

double_lt_str:
    sub rsp, 8
    movsd [rsp], xmm0
    call str_to_double
    add rsp, 8
    test al, al
    jz .not_convertible_to_number
    xor eax, eax
    ucomisd xmm0, [rsp-8]
    seta al
    ret
.not_convertible_to_number:
    ; TODO
    mov eax, 60
    mov edi, 89
    syscall

str_lt_double:
    sub rsp, 8
    movsd [rsp], xmm0
    call str_to_double
    add rsp, 8
    test al, al
    jz .not_convertible_to_number
    xor eax, eax
    ucomisd xmm0, [rsp-8]
    setb al
    ret
.not_convertible_to_number:
    ; TODO
    mov eax, 60
    mov edi, 88
    syscall

random_between:
    ; TODO: perform rounding when both parameters are integers
    sub rsp, 24
    subsd xmm1, xmm0
    movsd [rsp], xmm0
    movsd [rsp+8], xmm1
    call drand48 wrt ..plt
    mulsd xmm0, [rsp+8]
    addsd xmm0, [rsp]
    add rsp, 24
    ret

str_to_double:
    xor eax, eax
    cmp rsi, 8
    je .might_be_infinity
.regular:
    ; TODO
    mov eax, 60
    mov edi, 90
    syscall
.might_be_infinity:
    mov rdx, "Infinity"
    cmp [rdi], rdx
    jne .regular
    movsd xmm0, [.inf]
    mov eax, 1
    ret
align 8
.inf: dq __?Infinity?__

str_eq_str:
    cmp rsi, rcx
    jne .no
    ; TODO: Case insensitive comparison
    mov rsi, rdx
    mov rdx, rcx
    sub rsp, 8
    call memcmp wrt ..plt
    add rsp, 8
    xor edx, edx
    test eax, eax
    setz dl
    mov eax, edx
    ret
.no:
    xor eax, eax
    ret

str_eq_double:
    ; TODO
    mov eax, 60
    mov edi, 87
    syscall
