    include "bitfuncs.inc"
    cpu 8008new

    ; Load this program at address 0x20 on master then call 0x100 to execute.

    org 0100H

;----------------------------------------
; ROM routines
puts:           equ 26D6H
mas_reset:      equ 2842H
getch:          equ 2FEDH
putch:          equ 2FDCH

;----------------------------------------
; System defines
LED_PORT:       equ 08H
MMAP1:          equ 0DH
RAM1:           equ 0DH
RAM2:           equ 0EH
RAM3:           equ 0FH

;----------------------------------------
; prime variables on the blaster
;const_max_prime:equ 07d0H ; 2000 numbers to search
;const_max_sqrt: equ 002dh ; we only need to cross up to sqrt(2000)
;const_bank_sz0: equ 029aH ; Sieve size on the first bank
;const_bank_sz1: equ 0535H ; Sieve size on the second bank
;const_fill_sz:  equ 029BH ; Size to sieve in each bank (3 * 667) = 2001 

;const_max_prime:equ 0ffdH ; 4093 numbers to search
;const_max_sqrt: equ 003fh ; we only need to cross up to sqrt(4093)

const_max_prime: equ 2710H ; 10_000 numbers to search
const_max_sqrt:  equ 0064h ; we only need to cross up to sqrt(10_000)
const_bank_sz0:  equ 0d05H ; Sieve size on the first bank
const_bank_sz1:  equ 1a0bH ; Sieve size on the second bank
const_fill_sz:   equ 0d06H ; Size to sieve in each bank (3 * 3334) = 10_002 

;const_max_prime:equ 001eH ; 30 numbers to search
;const_max_sqrt: equ 0006h ; we only need to cross up to sqrt(30)
;const_bank_sz0: equ 0009H ; Sieve size on the first bank
;const_bank_sz1: equ 0013H ; Sieve size on the second bank
;const_fill_sz:  equ 000aH ; Size to sieve in each bank (3 * 10) = 30

;const_max_prime:equ 012cH ; 300 numbers to search
;const_max_sqrt: equ 0012h ; we only need to cross up to sqrt(300)
;const_bank_sz0: equ 0063H ; Sieve size on the first bank
;const_bank_sz1: equ 00c7H ; Sieve size on the second bank
;const_fill_sz:  equ 0064H ; Size to sieve in each bank (3 * 100) = 300

is_prime:        equ 0050H ; byte
chk_prime:       equ 0051H ; word
ps_idx:          equ 0053H ; word
prime_out:       equ 0055H ; word
all_prime_count: equ 0057H ; word
all_prime_stop:  equ 0059H ; word
row_count:       equ 005bH ; byte
step_out:        equ 005dH ; byte
 

;----------------------------------------
; Entry point 

    jmp main

;----------------------------------------
; Macros

; print the message at the address to the terminal
;
; registers used: h, l and whatever puts uses
print: macro address
    mvi h,hi(address) ; show the startup message
    mvi l,lo(address) ;
    call puts         ;
    endm


; load an immediate value into h and l
; The one just saves some typing
;
; registers used: h, l
mvi_hl: macro address
    mvi h, hi(address)
    mvi l, lo(address)
    endm

; load the 16 bit value start at address into
; register a and b.  A has the high byte
; and B has the low byte (big-endian)
;
; registers used: a, b, h, l
load16: macro address, reg1, reg2
    mvi h, hi(address)
    mvi l, lo(address)
    mov reg1, m
    inr l
    mov reg2, m
    endm

; load the registers A and B from memory
; at address. A has the high byte and B
; has the low byte
;
; registers used: a, b, h, l
save16: macro address, reg1, reg2
    mvi h, hi(address)
    mvi l, lo(address)
    mov m, reg1
    inr l
    mov m, reg2
    endm

; save a byte in reg1 to memory
;
; registers used: m, h, l, reg1
save8: macro address, reg1
    mvi h, hi(address)
    mvi l, lo(address)
    mov m, reg1
    endm

; load reg1 with a byte from memory
;
; registers used: m, h, l, reg1
load8: macro address, reg1
    mvi h, hi(address)
    mvi l, lo(address)
    mov reg1, m
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
; Shift routines
 
; Shift right from registers A to B.
; The Carry from register A
; will be shifted into register B.
;
; In other words the high byte is in A
; and the low byte is in B
;
; C is used for temp storage
shr16:
    mov c, a ;save a
    ana a    ;clear carry
    mov a, c ;restore a
    rar      ;shift MSB (A) right
    mov c, a ;save updated a
    mov a, b ;shift carry into next byte
    rar
    mov b, a ;update with shifted value
    mov a, c ;restore a
    ret
    

; Shift left from registers B to A.
; the Carry from register B
; will be shifted into A.
;
; In other words the high byte is in A
; and the low byte is in B
;
; C is used for temp storage
shl16:
    mov c, a ;save a
    ana a    ;clear the carry
    mov a, b ;start shifting from the LSB
    ral     
    mov b, a ;update with shifted value                           
    mov a, c ;restore a and shift
    ral     
    ret                                

; Shift left from registers D to C, C to B, and B to A.
; the Carry from each previous register is shifted into the next.
;
; In other words the high word is in A B
; and the low word is in C D
;
; E is used for temp storage
shl32:
    mov e, a ;save a
    ana a    ;clear the carry
    mov a, d ;start shifting from the LSB
    ral
    mov d, a ;update with shifted value
    mov a, c ;shift carry into next byte
    ral
    mov c, a ;update with shifted value
    mov a, b ;shift carry into next byte
    ral 
    mov b, a ;update with shifted value
    mov a, e ;restore a and shift
    ral
    ret
;
;--------------------------------------------------------------------------------
; Math routines

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

; 16-bit add
; (a, b) + (c, d)
; results are stored in a, b
;
; registers used: a, b, c, d, e
add16:
    mov e, a
    mov a, b
    add d
    mov b, a
    mov a, e
    adc c
    ret

; increment the value in bc
;
; registers used: a, b, c
;TODO if i'm only using this in a few spot just replace with addition
inc16:
    mvi a, 1            ; using the ALU instead of inr to get the carry flag
    add c               ; increment the read address by one
    mov c, a            ; restore the updated low address
    mov a, b            ; move the high address to the accumulator
    mvi b, 0            ; only add the carry
    adc b               ; add the carry bit to the high address if we have one
    mov b, a            ; restore the updated high address
    ret

; increment the value in l and h if needed
;
; registers used: a, h, l
inc_addr_hl:
    mvi a, 1            ; using the ALU instead of inr to get the carry flag
    add l               ; increment the read address by one
    mov l, a            ; restore the updated low address
    mov a, h            ; move the high address to the accumulator
    mvi h, 0            ; only add the carry
    adc h               ; add the carry bit to the high address if we have one
    mov h, a            ; restore the updated high address
    ret

;--------------------------------------------------------------------------------
; General routines

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


; Is the value between 0 and max_prime?
in_range:
    load16 chk_prime, a, b              ; get the current value parameter to c d (second op of the compare)
    mvi c, hi(const_max_prime)
    mvi d, lo(const_max_prime)
    call cmp16                          ; chk_prime <= max_prime
    jz in_range_ret_true                ; they are equal so we are in range
    jnc in_range_ret_false              ; prime is greater than max_part_val
in_range_ret_true:
    mvi a, 0
    cpi 0
    ret
in_range_ret_false:
    mvi a, 1
    cpi 0
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
    save16 decout_subptr,a,b; save the pointer
    mvi_hl decout_result    ; set up with write pointer for the results
    mov_ab_hl               ;
    save16 decout_wptr, a, b; save the write pointer
decout_loop:
    mvi_hl decout_digit     ; initialize the current digit
    mvi m, 0h               ;
    mvi_hl decout_j         ; set the inner counter for current places 10k, 1k, ...
    mvi m, 4                ;
    mvi_hl decout_inc       ; set a pointer to the table for the digit
    mov_ab_hl               ; increment amount after a subtraction
    save16 decout_incptr,a,b; save it
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
    save16 decout_subptr,a,b; save it
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
    save16 decout_subptr,a,b; save it
decout_other_passes:
    load16 decout_incptr,a,b; get the inc table pointer
                            ; since the incptr table is at a specific location
                            ; and the high byte isn't changing. I can save a 
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
    load16 decout_wptr,a,b  ; get the write pointer
    mov_hl_ab               ; 
    mvi m, 0                ; write the null terminator
    ret
 

; fills a sieve with const_fill_sz 1's starting at address 1000
prime_fill:
    mvi b, 0
    mvi c, 0
    mvi d, hi(const_fill_sz)
    mvi e, lo(const_fill_sz)
    mvi_hl 1000H
pf_loop:
    mvi m, 1
    call inc_addr_hl
    call inc16
    mov a, b
    mov b, c
    mov c, d
    mov d, e
    mov e, a                ; equal16 overwrites a
    call cmp16
    mov a, e
    mov e, d
    mov d, c
    mov c, b
    mov b, a
    jnz pf_loop
    ret



; fill_reset: set all values of the Sieve to true on each memory bank
;             also sets the boot vector to initialize the prime table
;             on each blaster by setting the all values for its
;             partition to true.
fill_reset:
    mvi a, 0             ; 
    out LED_PORT         ; debug
    mvi a,RAM1           ; fill bank 1
    out MMAP1           
    call prime_fill
    mvi a, 1             ; 
    out LED_PORT         ; debug
 

    mvi a,RAM2           ; fill bank 2
    out MMAP1            
    call prime_fill
    mvi a, 2             ; 
    out LED_PORT         ; debug

    mvi a,RAM3           ; fill bank 3
    out MMAP1            
    call prime_fill
    mvi a, 3             ; 
    out LED_PORT         ; debug

    print prime_fill_text
    mvi a, 0
    mvi b, 2
    save16 chk_prime, a, b ; start checking for primes from 2

    jmp main_loop ; return



;----------------------------------------
; prime step: run one iteration of prime
prime_step:
    mvi a, RAM1                     ; load register A with a RAM bank
    out MMAP1                       ; send it to the mapper
 
    mvi a, 0                        ; set is prime to false, we check later
    out LED_PORT                    ; debug
    save8 is_prime, a               ;
    call in_range                   ; 
    jnz ps_done                     ; If we are not in range we are done
    load16 chk_prime, a, b          ; get the current number to check
    save16 ps_idx, a, b             ; index into the prime table

    mvi c, hi(const_bank_sz0)       ; if 0 <= ps_idx <= const_bank_sz0
    mvi d, lo(const_bank_sz0)       ; then we are in bank 1
    mov e, a                        ; save a since cmp16 will overwrite it
    call cmp16
    jz ps_range1
    jc ps_range1
    mov a, e
    mvi c, hi(const_bank_sz1)       ; if const_bank_sz0 < ps_idx <= const_bank_sz1
    mvi d, lo(const_bank_sz1)       ; then we are in bank 2
    call cmp16
    jz ps_range2
    jc ps_range2   
    jmp ps_range3                   ; else we are in bank 3 

ps_range1:
    mov a, e                        ; restore a to before compare
    mvi c, hi(1000H)                ; load the location of the sieve 
    mvi d, lo(1000H)                ;
    call add16                      ; offset into the table
    mov_hl_ab                       ; set memory location 
    jmp ps_set_check_prime
ps_range2:
    mov a, e                        ; restore a to before compare
    mvi c, hi(const_bank_sz0+1)     ; the next bank starts one byte after previous bank
    mvi d, lo(const_bank_sz0+1)     ;
    call sub16                      ; subtract off the start to adjust for being in bank 2
    mvi c, hi(1000H)                ; load the location of the sieve 
    mvi d, lo(1000H)                ;
    call add16                      ; offset into the table
    mov_hl_ab
    mvi a,RAM2                      ; switch to bank 2
    out MMAP1                       ; 
    jmp ps_set_check_prime
ps_range3:
    mov a, e                        ; restore a to before compare
    mvi c, hi(const_bank_sz1+1)     ; the next bank starts one byte after previous bank
    mvi d, lo(const_bank_sz1+1) 
    call sub16                      ; subtract off the start to adjust for being in bank 3
    mvi c, hi(1000H)                ; load the location of the parition table
    mvi d, lo(1000H)                ;
    call add16                      ; offset into the table
    mov_hl_ab
    mvi a,RAM3                      ; switch to bank 3
    out MMAP1                       ; 
    jmp ps_set_check_prime
 
ps_set_check_prime:
    mov a, m                        ; get the boolean value
    cpi 1                           ; is it true
    jnz ps_skip_value_chk           ; already crossed off so skip the value check
    mvi a, 1                        ; 
    out LED_PORT                    ; debug
    save8 is_prime, a               ; mark it as true
 
ps_skip_value_chk:
    mvi c, hi(const_max_sqrt)       ; have we already crossed everything?
    mvi d, lo(const_max_sqrt)       ;
    load16 chk_prime,a,b            ;
    call cmp16                      ; is chk_prime < sqrt(max_primes)
    jnc ps_done                     ; no, then we are done
    load16 ps_idx, a, b
    load16 chk_prime, c, d          ; 
    call add16                      ; ps_idx + chk_prime
    save16 ps_idx, a, b             ; save it as the current value used by the offset function

ps_mark_loop:
    load16 ps_idx, a, b             ; get the working index
    mvi c, hi(const_bank_sz0)       ; if 0 <= ps_idx <= const_bank_sz0
    mvi d, lo(const_bank_sz0)       ; then we are in bank 1
    mov e, a                        ; save a since cmp16 will overwrite it
    call cmp16
    jz ps_mark_loop_range1
    jc ps_mark_loop_range1
    mov a, e
    mvi c, hi(const_bank_sz1)       ; if const_bank_sz0 < ps_idx <= const_bank_sz1
    mvi d, lo(const_bank_sz1)       ; then we are in bank 2
    call cmp16
    jz ps_mark_loop_range2
    jc ps_mark_loop_range2
    jmp ps_mark_loop_range3         ; else use bank 3

ps_mark_loop_range1:
    mov a, e
    mvi c, hi(1000H)                ; load the location of the sieve
    mvi d, lo(1000H)                ;
    call add16                      ; offset into the table
    mov_hl_ab                       ; set the pointer
    jmp ps_mark_loop_crossoff
ps_mark_loop_range2:
    mov a, e
    mvi c, hi(const_bank_sz0+1)     ; restore a to before compare                        
    mvi d, lo(const_bank_sz0+1)     ; the next bank starts one byte after previous bank  
    call sub16                      ; subtract off the start to adjust for being in bank 2
    mvi c, hi(1000H)                ; load the location of the sieve
    mvi d, lo(1000H)                ;
    call add16                      ; offset into the table
    mov_hl_ab
    mvi a,RAM2                      ; use bank 2
    out MMAP1               
    mov_ab_hl
    jmp ps_mark_loop_crossoff
ps_mark_loop_range3:
    mov a, e
    mvi c, hi(const_bank_sz1+1)     ; the next bank starts one byte after previous bank
    mvi d, lo(const_bank_sz1+1) 
    call sub16                      ; subtract off the start to adjust for being in bank 2
    mvi c, hi(1000H)                ; load the location of the sieve
    mvi d, lo(1000H)                ;
    call add16                      ; offset into the table
    mov_hl_ab
    mvi a,RAM3                      ; use bank 3
    out MMAP1               
    mov_ab_hl
    jmp ps_mark_loop_crossoff       

ps_mark_loop_crossoff:
    mvi m, 0                        ; cross it out
    load16 ps_idx, a, b
    load16 chk_prime, c, d          ; need to increment by the check value
    call add16                      ; ps_idx + chk_prime
                                    ; do I need to check overflow here?
    jc ps_done                      ; we went past 16-bits
    mvi c, hi(const_max_prime)      ; are we at the end of the numbers we want to check?
    mvi d, lo(const_max_prime)      ;
    save16 ps_idx, a, b             ; save the updated index
    call cmp16                      ; offset_value < offset_end
    jc ps_mark_loop                 ; we are still in range
    jz ps_mark_loop                 ; make sure to cross last number if it is not prime
 
ps_done:
    ret


single_step:
    call prime_step
    load16 chk_prime, c, d      ; load master's chk_prime
    mvi a, 0
    mvi b, 1
    call add16
    save16 chk_prime, a, b

    load8 step_out, a           ; do we want to print the current number?
    cpi 1                       ; 
    jnz single_step_ret         ; if false exit
    save16 decout_value, c, d   ; setup decimal output variable
    call decout                 ; convert to decimal
    print space                 ;
    print decout_result         ;
single_step_ret:         
    ret


single_step_out:
    mvi a, 1                    ; echo the number during single step
    save8 step_out, a           ;
    call single_step            ; one step
    jmp main_loop


all_primes:
    mvi a, 0
    save8 step_out, a
    print newline
    mvi a, 0
    mvi b, 0
    save16 all_prime_count, a, b; initialize the counter to zero
    mvi a, hi(const_max_prime)
    mvi b, lo(const_max_prime)
    save16 all_prime_stop, a, b ; save it stop
    mvi a, 0
    save8 row_count,a           ; set row count to zero
 
all_primes_loop:
    load16 chk_prime, a, b      ; load master's chk_prime
    save16 prime_out, a, b      ; save the current prime before calling single_step
    call single_step            ; 
    mvi_hl is_prime             ; was the current value prime on this blaster?
    mov a, m                    ;
    cpi 0                       ; is false?
    jz all_primes_next          ; skip print

all_primes_print:
    load16 prime_out, a, b      ; get the saved prime
    save16 decout_value, a, b   ; save it to decimal output parameter
    call decout                 ; convert to decimal
    print decout_result         ; print it
    print space                 ;
    load8 row_count,a           ; get the current row_count
    mov c, a                    ; save a copy in c
    mvi b, 1                    ; increment by one
    add b                       ; increment
    save8 row_count,a           ; save it
    mov a, c                    ; restore A before inc
    cpi 9                       ; are we at max columns?
    jnz all_primes_next         ; if not go to next prime
    mvi a, 0                    ; if yes reset row count
    save8 row_count,a           ; save it
    print newline               ; print new line

all_primes_next:
    load16 all_prime_count, a, b; get the counter
    mvi c, 0                    ; increment by one
    mvi d, 1                    ;
    call add16                  ;
    save16 all_prime_count, a, b; save it
    load16 all_prime_stop, c, d ; get the stop
    call cmp16                  ; are we done?
    jnz all_primes_loop         ; no print next prime
    jmp main_loop               ; return


main:
    print menutxt
main_loop:
    print prompttxt
    call getch
    cpi 'a'                     ; is the input character below 'a'?
    jc $+5                      ; skip the next instruction if the character is already upper case
    sui 20H                     ; else, convert to the character to upper case
    call putch                  ; echo the character
    cpi 'I'
    jz fill_reset               ; fill the prime table with true
    cpi 'S'
    jz single_step_out          ; one step of prime
    cpi 'A'
    jz all_primes               ; print all primes in the range
    cpi 'Q'                     
    jz main_exit                ; exit the menu
    jmp main_loop
main_exit:
    ret
 

;----------------------------------------
; Strings
menutxt:    
    db  "\r\n"
    db  "I - Initalize prime table\r\n"
    db  "S - Single step\r\n"
    db  "A - Print all\r\n"
    db  "Q - Quit\r\n"
    db 0

prompttxt:
    db  "\r\n>>",0

prime_fill_text:
    db "\r\nInitalizing prime table\r\n", 0

newline:
    db "\r\n", 0

space:
    db " ", 0

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

    org 0600H

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



