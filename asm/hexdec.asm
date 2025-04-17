; hexdec
;
;
; This program for the super-8008 converts 16-bit hexidecimal input decimal.
;
; Load the program to memory location 0x20 and call the routine at 0x1000 to execute.
; To exit the loop enter 0.
;
; https://www.smbaker.com/master-blaster-an-8008-supercomputer
; https://jeremyenglish.org/mame-master-blaster.html


    include "bitfuncs.inc"
    cpu 8008new

    org 1000H


;----------------------------------------
; ROM routines
puts:           equ 26D6H
get_four:       equ 2575H


;----------------------------------------
; jump to main routine

    call main
    ret


;----------------------------------------
; Macros

; load an immediate value into h and l
; The one just saves some typing
;
; registers used: h, l
mvi_hl: macro address
    mvi h, hi(address)
    mvi l, lo(address)
    endm


; Get a 16-bit hex number from the terminal
; and save it to the address passed in 
; big-endian format
;
; registers used: a, b, h, l 
input: macro address
    call get_four
    mov a, h
    mov b, l
    mvi_hl address
    mov m, a
    inr l
    mov m, b
    endm


; print the message at the address to the terminal
;
; registers used: h, l and whatever puts uses
print: macro address
    mvi_hl address
    call puts         ;
    endm


; load the 16 bit value start at address into
; register a and b.  A has the high byte
; and B has the low byte (big-endian)
;
; registers used: a, b, h, l
load16: macro address
    mvi_hl address
    mov a, m
    inr l
    mov b, m
    endm


; save the registers A and B to memory
; at address. A has the high byte and B
; has the low byte
;
; registers used: a, b, h, l
save16: macro address
    mvi_hl address
    mov m, a
    inr l
    mov m, b
    endm

; Moving h and l together seems to be 
; common when working with pointers 
;
; registers used: a, b, h, l
mov_ab_hl: macro
    mov a, h
    mov b, l
    endm

; Moving h and l together seems to be 
; common when working with pointers 
;
; registers used: a, b, h, l
mov_hl_ab: macro
    mov h, a
    mov l, b
    endm


; decrement a memory location
;
; registers used: e, m, h, l
dec_mem_8: macro address
    mvi_hl address
    mov e, m
    dcr e                   
    mov m, e
    endm
 

;----------------------------------------
; Math routines


; # r0 msb of a
; # r1 lsb of a
; # r2 msb of b
; # r3 lsb of b
; # a - b
; def sub16(r0, r1, r2, r3):
;     # cheat for now so I don't need to worry
;     # about carry clear and set
;     a = regsToInt16(r0, r1)
;     b = regsToInt16(r2, r3)
;     x = a-b
;     res1, res2 = int16ToRegs(x)
;     return (res1, res2, x < 0)

; 16-bit subtraction
; (a, b) - (c, d)
; results are stored in a, b
;
; registers a, b, c, d, e
sub16:
    mov e, a
    mov a, b
    sub d
    mov b, a
    mov a, e
    sbb c
    ret

;----------------------------------------
; General purpose

; 16-bit compare
; If the high byte is the same for both words we check the low byte
; The calling function can then check the carry or zero or both for <, >, <=, >=, ==,
;
; registers: a, b, c, d

cmp16:
    cmp c                    ; compare high bytes of both words
    jz cmp16_low             ; high bytes are equal check the low bytes
    ret                      ; the calling function can now check if carry to get the result
cmp16_low:
    mov a, b                 ; compare the low bytes and return with the flags set
    cmp d
    ret


; Load the pointer value at location hl. The assumption in this
; version is the value in HL is not at a byte boundry to avoid
; a 16-bit add.  For example hl 0x00ff will not work since the
; first byte will come from 0xff and the second byte will come 
; from 0x00.
;
; registers: a, b, h, l, m
load_pointer:
    mov a, m
    inr l
    mov b, m
    mov_hl_ab               ; move the pointer to hl
    ret
 

;----------------------------------------
; decout

; For each value of n from 4 to 1, it compares the number to 8*10^n,
; then 4*10^n, then 2*10^n, then 1*10^n, each time subtracting if
; possible. 
;
; After finishing all the comparisons and subtractions in
; each decimal place value, it writes the digit to the output array
; as a byte value in the range [0, 9].
; 
; https://www.nesdev.org/wiki/16-bit_BCD 
; 
; def decout_8421(x):
;     s = ""
;     digits = "0123456789"
;     inc = [8,4,2,1]
;     stab = sub_table()
;     i = 0
;     while i < len(stab):
;         digit = 0
;         for j in inc:
;             if x >= stab[i]:
;                 x = x - stab[i]
;                 digit += j
;             i+=1
;         s += digits[digit]
;         
;     Finally, it writes the
;     remainder to element 0.
;     s += digits[x]
; 
;     return s


; the digit should be in E
decout_digit_write:
    mvi_hl decout_wptr      ; get the write pointer location
    mov c, h                ; save h and l to temp locations
    mov d, l                ;
    call load_pointer       ; load the pointer
    mov m, e                ; write the digit to memory
    inr b                   ; increment the address
    mov h, c                ; restore hl
    mov l, d                ; 
    inr l                   ; move to the low byte
    mov m, b                ; save it
    ret


decout:
    mvi_hl decout_i         ; load the outer counter
    mvi m, 4                ; loop over all five digits
    mvi_hl decout_sub       ; set up the subtraction value pointer
    mov_ab_hl               ; 
    save16 decout_subptr    ; save the pointer
    mvi_hl decout_result    ; set up with write pointer for the results
    mov_ab_hl               ;
    save16 decout_wptr      ; save the write pointer
decout_loop:
    mvi_hl decout_digit     ; initialize the current digit
    mvi m, 0h               ;
    mvi_hl decout_j         ; set the inner counter for current places 10k, 1k, ...
    mvi m, 4                ;
    mvi_hl decout_inc       ; set a pointer to the table for the digit
    mov_ab_hl               ; increment amount after a subtraction
    save16 decout_incptr    ; save it
decout_loop2:
    mvi_hl decout_subptr    ; get the sub table pointer
    call load_pointer       ;
    mov a, m                ; get the first byte
    mvi e, 0FFh             ; load the sentinel
    cmp e                   ; Is this the first sub table entry?
    jz decout_first_pass    ; skip this entry we know we are less than 80_000 since it is 16 bits
    mov e, a                ; save it to a temp location
    inr l                   ; inc subptr
    mov c, e                ; restore the high part of the word from e
    mov d, m                ; move the second byte from the table
    inr l                   ; inc subptr
    mov_ab_hl               ; save the updated decout_subptr
    save16 decout_subptr    ; save it
    mvi_hl decout_value     ; load the value
    mov a, m                ;
    inr l                   ; move to the next byte of the word
    mov b, m                ;
    mov e, a                ; save a copy of a to restore after compare
    call cmp16              ; compare working value with the value from the table
    jc decout_skip          ; table_value < working_value? then skip subtraction
                            ; c and d are still set to the sub value from the table
    mov a, e                ; restore a
    call sub16              ; working value - sub value
    dcr l                   ; move back to the first byte
    mov m, a                ; save first byte of subtraction
    inr l                   ; move to the next byte
    mov m, b                ; save the second byte of subtraction
    mvi_hl decout_incptr    ; load the increment pointer
    call load_pointer       ;
    mov e, m                ; store the inc table value in e
    mvi_hl decout_digit     ; get the current digit
    mov a, m                ; 
    add e                   ; add the inc table amount
    mov m, a                ; save it
decout_skip:
    jmp decout_other_passes ; we've alread increment the subptr
decout_first_pass:
    inr l                   ; inc the sub pointer by two
    inr l                   ;
    mov_ab_hl               ;
    save16 decout_subptr    ; save it
decout_other_passes:
    load16 decout_incptr    ; get the inc table pointer
                            ; since the incptr table is at a specific location
                            ; and the high byte isn't  changing. I can save a 
                            ; few lines of code here.
    inr b                   ; increment the inc pointer
    mov m, b                ; save it
    dec_mem_8 decout_j      ; decrement the inner counter
    jnz decout_loop2        ; until zero
    mvi_hl decout_digit     ; setup read for the digit
    mov a, m                ; get it from memory
    mvi b, lo(digits)       ; offset into the digits table by this amount
    add b                   ;
    mvi h, hi(digits)       ; set the high byte. (it shouldn't change)
    mov l, a                ; set low to the addition result

    mov e, m                ; get digit from the digits table
    call decout_digit_write ; write it to the pointer table
    dec_mem_8 decout_i      ; decrement the outer loop
    jnz decout_loop         ; are we done?
                            ; save the remainder to the last write location
                            ; get what is left in the value
    mvi_hl decout_value     ; setup offset addition to get the digit from the lookup table
    inr l                   ; we should only have one byte left
    mov a, m                ; get it from memory
    mvi b, lo(digits)       ; offset into the digits table by this amount
    add b                   ;
    mvi h, hi(digits)       ; set the high byte. (it shouldn't change)
    mov l, a                ; set low to the addition result
    mov e, m                ; get digit from the digits table
    call decout_digit_write ; write it to memory
    load16 decout_wptr      ; get the write pointer
    mov_hl_ab               ; 
    mvi m, 0                ; write the null terminator
    ret
 

main:
    print vermsg
main_loop:
    print num1msg
    input decout_value
    load16 decout_value
    mvi c, 0
    mvi d, 0
    call cmp16              ; zero will exit the loop
    jz main_done
    print newline
    call decout
    print decout_result
    print newline
    jmp main_loop
main_done:
    ret
 

vermsg:
    db "\r\nv3", 0
num1msg:
    db "\r\nhex> ", 0
newline:
    db "\r\n", 0

;----------------------------------------
; decout variables

; If we load the table and variables to specific locations then we only inc L
; instead of a full 16bit inc when converting to decimal
    org 0020H

decout_result:
    db 0, 0, 0, 0, 0, 0
decout_value:
    db 0, 0
decout_i:
    db 0, 0
decout_j:
    db 0, 0
decout_subptr:
    db 0, 0
decout_incptr:
    db 0, 0
decout_wptr:
    db 0, 0
decout_digit:
    db 0

;; decout_result:  equ 0020H ; 5 chars + null
;; decout_value:   equ 0026H ; word
;; decout_i:       equ 0028H ; word
;; decout_j:       equ 0029H ; word
;; decout_subptr:  equ 002BH ; word
;; decout_incptr:  equ 002DH ; word
;; decout_wptr:    equ 002FH ; word
;; decout_digit:   equ 0031H ; byte

    org 0100H

decout_inc:
    db 8,4,2,1

digits:
    db "0123456789"

decout_sub:
; 80,000 > 2^16 so we need to skip that one when converting.  I'm just padding it with FF for now
;  0x01, 0x38, 0x80,  80_000
;        0x9c, 0x40,  40_000
;        0x4e, 0x20,  20_000
;        0x27, 0x10,  10_000
;        0x1f, 0x40,   8_000
;        0xf, 0xa0,    4_000
;        0x7, 0xd0,    2_000
;        0x3, 0xe8,    1_000
;        0x3, 0x20,      800
;        0x1, 0x90,      400
;        0x0, 0xc8,      200
;        0x0, 0x64,      100
;        0x0, 0x50,       80
;        0x0, 0x28,       40
;        0x0, 0x14,       20
;        0x0, 0xa,        10

    db 0FFH, 0FFH, 09cH, 040H, 04eH, 020H, 027H, 010H 
    db 01fH, 040H, 00fH, 0a0H, 007H, 0d0H, 003H, 0e8H 
    db 003H, 020H, 001H, 090H, 000H, 0c8H, 000H, 064H 
    db 000H, 050H, 000H, 028H, 000H, 014H, 000H, 00aH


