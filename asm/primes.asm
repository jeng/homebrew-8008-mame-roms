    include "bitfuncs.inc"
    cpu 8008new

    ; Load this program at address 0x20 on master then call 0x1000 to execute.
    ; Bootstraps are loaded to the  blasters at 0x0000 since that is the reset
    ; vector for the 8008.

    org 1000H

;----------------------------------------
; Memory system defines
MMAP0:          equ 0CH
MMAP1:          equ 0DH
MMAP2:          equ 0EH
MMAP3:          equ 0FH

PAGE0:          equ 000H
PAGE1:          equ 010H
PAGE2:          equ 020H
PAGE3:          equ 030H

RAM0:           equ 0CH             ; 0x08 enables RAM, 0x04 disables EXT
RAM1:           equ 0DH
RAM2:           equ 0EH
RAM3:           equ 0FH
ROMOR:          equ 04H             ; bit 04H, if low will access ext RAM

EXTRAM0:        equ 08H
EXTRAM1:        equ 09H
EXTRAM2:        equ 0AH
EXTRAM3:        equ 0BH
ROM0:           equ ROMOR | 00H
ROM1:           equ ROMOR | 01H

;----------------------------------------
; Master/Blaster defines
MAS_STAT_PORT:  equ 01H
MAS_TAKE_PORT:  equ 14H
MAS_INT_PORT:   equ 15H
MAS_REQ_PORT:   equ 16H
MAS_RST_PORT:   equ 17H

BLASTER0:       equ 00FEH
BLASTER1:       equ 00FDH
BLASTER2:       equ 00FBH
BLASTER3:       equ 00F7H
BLASTER4:       equ 00EFH
BLASTER5:       equ 00DFH
BLASTER6:       equ 00BFH


;----------------------------------------
; OPs defines
CALL_OP:        equ 046H

;----------------------------------------
; ROM routines
puts:           equ 26D6H
mas_reset:      equ 2842H
getch:          equ 2FEDH
putch:          equ 2FDCH

;----------------------------------------
; Cylon defines
LED_PORT:       equ  08H
ROTATE_COUNT:   equ  07H
NUM_REPEAT:     equ  02H

;----------------------------------------
; prime variables on the blaster
;const_part_sz:  equ 03e8h ; const 7000 total
;const_part_sz:  equ 00b6h ; const  500 total

const_max_prime:equ 4e20H ; 20_000 numbers to search
const_part_sz:  equ 0B29h ; const 20_000/7 total
const_max_sqrt: equ 008dh ; const we only need to cross up to sqrt(20_000)

;const_max_prime:equ 2710H ; 10_000 numbers to search
;const_part_sz:  equ 0594h ; const 10_000/7 total
;const_max_sqrt: equ 0064h ; const we only need to cross up to sqrt(20_000)

;const_max_prime:equ 0ffdH ; 4093 numbers to search
;const_part_sz:  equ 0249h ; const 4093/7 total
;const_max_sqrt: equ 003fh ; const we only need to cross up to sqrt(20_000)

offset_start:   equ 0100H ; word
offset_end:     equ 0102H ; word
part_sz:        equ 0104H ; word
is_prime:       equ 0106H ; byte
chk_prime:      equ 0108H ; word
bps_part_idx:   equ 010AH ; word
prime_out:      equ 010CH ; word
all_prime_count:equ 010EH ; word
all_prime_stop: equ 0110H ; word
row_count:      equ 0112H ; byte
step_out:       equ 0113H ; byte
ofs_tbl_idx:    equ 0114H ; word
is_prime_out:   equ 0116H ; byte
tmp_prime:      equ 0117H ; word
lt_sqrt:        equ 0119H ; byte
offset_tbl:     equ 3000H ; const starting address of the offset table

;----------------------------------------
; mul16 Variables
mul16_result:   equ 0200H  ;word
mul16_l:        equ 0202H  ;word
mul16_r:        equ 0204H  ;word

;----------------------------------------
; div16 variables
dividen:        equ 0206H  ;word
divisor:        equ 0208H  ;word
shift_dividen:  equ 020AH  ;word
quotient:       equ 020CH  ;word
remainder:      equ 020EH  ;word

;----------------------------------------
; offset variables and constant
min_part_val:   equ 0210H   ;word
max_part_val:   equ 0212H   ;word
current_val:    equ 0214H   ;word
offset_tmp:     equ 0216H   ;word
offset_res:     equ 0218H   ;word
dbg_offs_1:     equ 021AH   ;word
dbg_offs_2:     equ 021CH   ;word
dbg_offs_3:     equ 021EH   ;word
dbg_offs_4:     equ 0220H   ;word
OUT_OF_BOUNDS:  equ 0FFH    ;word


;----------------------------------------
; Entry point 

    jmp main

;----------------------------------------
; Macros
wait: macro cycles
    mvi e, cycles
wait1:
    mvi d,0
wait2:
    inr d
    jnz wait2
    dcr e
    jnz wait1
    endm


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
 
; write the bootstrap passed to the macro to all blasters at page zero
;
; registers used: a, h, l, m
write_bstrap: macro address
    mvi a,EXTRAM0           ; load register A with a RAM bank
    out MMAP0               ; send it to the mapper
    mvi a,07FH              ; take_w take all of the blasters
    out MAS_TAKE_PORT       ; Take all blaster for writing
    mvi h, hi(PAGE0)
    mvi l, lo(PAGE0)
    mvi m, CALL_OP          ; this is the call opcode: 46 cf 10 is "call $10cf"
    inr l                   ; we know we are not wrapping on the increment since starting at 0000 
    mvi m, lo(address)
    inr l                   ; same here, no wrapping
    mvi m, hi(address)
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

; # r0 msb of a
; # r1 lsb of a
; # r2 msb of b
; # r3 lsb of b
; def bitmul2reg(r0, r1, r2, r3):
;     x0, x1 = (0,0)
;     for i in range(16):
;         n = lsb(r3)
;         r2, r3 = shr16(r2, r3)
;         if n == 1:
;             x0, x1 = add16(x0, x1, r0, r1)
;         r0, r1, _ = shl16(r0, r1)
; 
;     return (reg8(x0), reg8(x1))


; 16-bit multiplication
; (a, b) * (c, d)
; results are stored in a, b also in mul16_result
;
; registers a, b, c, d, e, h, l
mul16:
    save16 mul16_l, a, b       ; save the left term to memory
    save16 mul16_r, c, d       ; save the right term to memory
    mvi a, 0h                  ;
    mvi b, 0h                  ;
    save16 mul16_result, a, b  ; init the results to zero
    load16 mul16_l, a, b       ; restore a and b
    mvi e, 010h                ; loop through all 16 bits
mul16_1:                       ; Reg A MSB of left term and D LSB of right term
    mov l, a                   ; save A for the addition later
    mov a, d                   ; prepare to check if the LSB is 1
    ani 00000001B              ; mask off the other bits
    cpi 0H                     ; if the lsb is zero, jump past the addition
    jz mul16_shl               ;
    mov c, l                   ; set the right term of the addition
    mov d, b                   ;
                               ; a, b, h, l updated here
    load16 mul16_result, a, b  ; pull the result from memory
                               ; it is used as the left term of the addition
    mov l, e                   ; save the counter
                               ; a, b, c, d, e updated here
    call add16                 ; add to the result
    mov e, l                   ; restore the counter
                               ; a, b, h, l updated here
    save16 mul16_result, a, b  ; save the result
mul16_shl:
                               ; a, b, h, l updated here
    load16 mul16_r, a, b       ; load a and b with the right term
                               ; a, b, c updated here
    call shr16                 ; the shift takes place on a and b
    mov d, b                   ; keep d for the LSB check above
    save16 mul16_r, a, b       ; save the shifted right term
                               ; a, b, h, l used here
    load16 mul16_l, a, b       ; load a and b with the left term
                               ; a, b, c is used here
    call shl16                 ; the shift takes place on a and b 
                               ; a, b, h, l used here
    save16 mul16_l, a, b       ; save the shifted left term
    dcr e                      ; decrement the counter
    jnz mul16_1                ; while not zero
    load16 mul16_result, a, b  ; set the result in a and b
    ret

; # r0: high byte dividen
; # r1: low byte dividen
; # r2: high byte divisor
; # r3: low byte divisor
; def div32x16(r0, r1, r2, r3):
;     # need 16 bits of padding to shift into
;     t0, t1 = (0, 0)
;     quotient = 0
;     for i in range(16):
;         t0, t1, r0, r1 = shl32(t0, t1, r0, r1)
;         quotient <<= 1
;         t0, t1, negative = sub16(t0, t1, r2, r3)
;         if negative:
;             t0, t1 = add16(t0, t1, r2, r3)
;         else:
;             quotient = quotient | 1
;     res1, res2 = int16ToRegs(quotient)
;     return (res1, res2, t0, t1)

; 16-bit division
; (a, b) / (c, d)
; results are stored in quotient and remainder memory location
;
; registers a, b, c, d, e, h, l

div16:
    save16 dividen, a, b        ; Save the dividen 1st since it is in reg A and B
    save16 divisor, c, d        ; save the divisor
    mvi a, 00H                  ; init in the shift.  We need to pad out to 16
    mvi b, 00H                  ; this will be used to shift in the divisor on bit at a time
    save16 shift_dividen, a, b  ; save the padding
    save16 quotient, a, b       ; also save zero to the quotient
    mvi e, 010H                 ; loop over all 16 bits
div16_1:
    save8 tmp_e, e              ; save the counter
    load16 dividen, c, d        ; set c and d to the dividen
    load16 shift_dividen, a, b  ; set a and b to the shift dividen
    call shl32                  ; shift left for from the dividen into the shift dividen
    save16 shift_dividen, a, b  ; save the updated shift dividen
    save16 dividen, c, d        ; save the updated dividen
    load16 quotient, a, b       ; load quotient in a and b
    call shl16                  ; shift the quotient by 1 bit
    save16 quotient, a, b       ; save the update
    load16 divisor, c, d        ; load the divisor to the right term for the subtraction
    load16 shift_dividen, a, b  ; load the shift divide for the left term for the subtraction
    call sub16                  ; shift_dividen - divisor
                                ; result is negative?
    jc div16_dec                ; do not save subtraction when negative
    save16 shift_dividen, a, b  ; save the updated dividen
    load16 quotient, a, b       ; load the quotient into A and B
    mov e, a                    ; save A
    mov a, b                    ; move LSB to A for the OR operation
    ori 1                       ; OR in 1 since we were able to subtract the divisor
    mov b, a                    ; restore low byte of quotient
    mov a, e                    ; restore high byte of quotient
    save16 quotient, a, b       ; save the updated quotient
div16_dec:
    load8 tmp_e, e              ; restore the counter
    dcr e                       ; dec the counter
    jnz div16_1
    load16 shift_dividen, a, b  ; get the remainder
    save16 remainder, a, b      ; save to the remainder variable
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

; get_offset: get the offset in the current partition of a number, if the
; number is in range, or a multiple of the number > number if that is in range
; otherwise return out of bounds.
;
; parameters for this function are passed in specific memory locations
;
; start of the region (minimum partition value): min_part_val
; end of the region (maximum partition value): max_part_val
; number to check (current value): current_val
;
; A temporary memory location is used for calculating the offset when a
; multiple is in range.
;
; offset_tmp
;
; The result of the function is stored in a memory location
;
; offset_res
;
; When out-of-bounds both bytes will be set to OUT_OF_BOUNDS


;The range check was copied from get offset so that we can skip some
;expensive calculations once we are past the < sqrt phase
in_range:
    load16 current_val, c, d            ; get the current value parameter to c d (second op of the compare)
    load16 min_part_val, a, b           ; get the lower bounds (store in a b first op of the compare)
    call cmp16                          ; min_part_val <= current_val
    jz in_range_ret_true                ; They are equal so we are in range
    jnc in_range_ret_false              ; current_val is less than min_range
                                        ; we need to check a multiple (get_offset_lt)
                                        ; otherwise
                                        ; min_part_val is less than the current_val
                                        ; need to check current_val <= max next
                                        ; need to check current_val <= max next
    load16 max_part_val, c, d           ; load max_part_val into the second op
    load16 current_val, a, b            ; load current_val into the first op
    call cmp16                          ; current_val <= max_part_val
    jz in_range_ret_true                ; they are equal so we are in range
    jnc in_range_ret_false              ; current_val is greater than max_part_val
 
in_range_ret_true:
    mvi a, 0
    cpi 0
    ret
in_range_ret_false:
    mvi a, 1
    cpi 0
    ret


get_offset:
    load16 current_val, c, d            ; get the current value parameter to c d (second op of the compare)
    load16 min_part_val, a, b           ; get the lower bounds (store in a b first op of the compare)
    call cmp16                          ; min_part_val <= current_val
    jz get_offset_in_range              ; They are equal so we are in range
    jnc get_offset_lt                   ; current_val is less than min_range
                                        ; we need to check a multiple (get_offset_lt)
                                        ; otherwise
                                        ; min_part_val is less than the current_val
                                        ; need to check current_val <= max next
    load16 max_part_val, c, d           ; load max_part_val into the second op
    load16 current_val, a, b            ; load current_val into the first op
    call cmp16                          ; current_val <= max_part_val
    jz get_offset_in_range              ; they are equal so we are in range
    jnc get_offset_past_end             ; current_val is greater than max_part_val
                                        ; we are out of bounds
 
get_offset_in_range:
    load16 min_part_val, c, d           ; load min_part_val to the second op
    load16 current_val, a, b            ; load current_val to the first op
    call sub16                          ; current_val - min_part_val
    save16 offset_res, a, b             ; save to the results
    ret                                 ; return

get_offset_lt:
    load16 current_val, c, d            ; load current_val to second op
    load16 min_part_val, a, b           ; load min_part_val to first op
    call div16                          ; min_part_val / current_val
                                        ; results are stored in quotient and remainder memory locations
    load16 remainder, a, b              ; checking if min_part_val % current_val == 0
    mvi c, 0                            ; remainder == 0
    mvi d, 0                            ; remainder == 0
    call cmp16                          ;
    jnz get_offset_check_q              ; it is not equal (we have a remainder) check the quotient
    load16 remainder, a, b              ; the first element is the offset
    save16 offset_res, a, b             ; set it as the result (same as mvi a, 0 & mvi b, 0)
    ret                                 ; return the offset

get_offset_check_q:
    load16 current_val, c, d            ; load the current_val to the second op
    load16 quotient, a, b               ; load the quotient to the first op 
    call mul16                          ; floor(min_part_val/current_val) * current_val
                                        ; results are stored in a, b also in mul16_result
    load16 current_val, c, d            ; load current_val in second op
    call add16                          ; current_val + (floor(min_part_val/current_val) * current_val)
                                        ; results are in a b
    save16 offset_tmp, a, b             ; save to a temp location we need them later
    mov c, a                            ; also move to the second op
    mov d, b                            ;
    load16 min_part_val, a, b           ; load min_part_val to first op
    call cmp16                          ; min_part_val <= offset_tmp
    jz get_offset_in_range_2            ; they are equal so it is range
    jnc get_offset_lt_2                 ; offset_tmp is less than min_part_val, we are out of bounds
    load16 max_part_val, c, d           ; load max_part_val in op two
    load16 offset_tmp, a, b             ; load offset_tmp in op one
    call cmp16                          ; offset_tmp <= max_part_val
    jz get_offset_in_range_2            ; they are equal so we are in range
    jnc get_offset_past_end             ; offset_tmp is greater than max_part_val, we are out of bounds
 
get_offset_in_range_2:
    load16 min_part_val, c, d           ; load min_part_val in second op
    load16 offset_tmp, a, b             ; load offset_tmp in first op
    call sub16                          ; offset_tmp - min_part_val
    save16 offset_res, a, b             ; resutls are in a, b so save those to the offset_res
    ret                                 ; return the result

get_offset_lt_2:                        ; out of bounds condtions
get_offset_past_end:
    mvi a, OUT_OF_BOUNDS                ; this means or offset should never be 2^16
    mvi b, OUT_OF_BOUNDS                ; 
    save16 offset_res, a, b             ; save it and return
    ret                                 ;

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
    load16 decout_wptr,a,b  ; get the write pointer
    mov_hl_ab               ; 
    mvi m, 0                ; write the null terminator
    ret
 

;----------------------------------------
; Master Program
cylon: 
    mvi c, NUM_REPEAT
    mvi a, 01h              ; start with led one on
loop:
    mvi b, ROTATE_COUNT
loop1:
    out LED_PORT
    wait 03h
    rlc                     ; move the led left
    dcr b
    jnz loop1

    mvi b, ROTATE_COUNT
loop2:
    out LED_PORT
    wait 03h 
    rrc                     ; move the led right
    dcr b
    jnz loop2
    
    dcr c
    jnz loop
    ret


;----------------------------------------
; Load utilities


; load a program to the blaster
;
; Parameters:
;   PROG_READ_ADDR  regs h and l
;   PROG_END_ADDR   regs a and b
;   PROG_WRITE_ADDR regs c and d
;
; registers used: a, b, c, d, h, l
; TODO is e used?
loadprog:
                                    ; This will cobber h and l so don't do this
                                    ; save16 PROG_WRITE_ADDR, c, d
                                    ; to have 8 bytes passed in I would need an argument stack
                                    ; To load to blaster the hardware always uses page0
                                    ; so hard code that for now
    mov c, h                        ; move to other regs so they can be saved
    mov d, l                        ; move to other regs so they can be save
    save16 PROG_READ_ADDR, c, d     ; save the read address
    save16 PROG_END_ADDR, a, b      ; save the end address
    mvi a, hi(page0)
    mvi b, lo(page0)
    save16 PROG_WRITE_ADDR, a, b
loadprog2:
    load16 PROG_READ_ADDR, a, b     ; setup the read address
    mov_hl_ab                       ; move the address for memory access
    mov d, m                        ; Get a byte from memory
    call inc_addr_hl                ; increment the read address
    mov_ab_hl                       ; setup for saving
    save16 PROG_READ_ADDR, a, b     ; save it
    load16 PROG_WRITE_ADDR, a, b    ; get the write address
    mov_hl_ab                       ; set the address for memory write
    mov m, d                        ; write the byte to memory
    call inc_addr_hl                ; increment the memory address
    mov_ab_hl                       ; set it up for write
    save16 PROG_WRITE_ADDR, a, b    ; save it
    load16 PROG_END_ADDR, a, b      ; pull the prog end from memory
    load16 PROG_READ_ADDR, c, d     ; pull the read address for comparison
    call cmp16                      ; is the prog_read_addr == prog_end_addr
    jnz loadprog2                   ; We're not at the end of program continue
    ret


copier:
    print loadtxt

    ;setup external ram page 0 for all blaster writes
    mvi a,EXTRAM1               ; load register A with a RAM bank
    out MMAP0                   ; send it to the mapper
    mvi a,07FH                  ; take_w take all of the blasters
    out MAS_TAKE_PORT           ; Take a specific blaster for writing
    mvi_hl 1000H                ; setup the cylon-blaster program for loading
    mvi a, hi(blaster_prog_end) ; setup the end of the copy location
    mvi b, lo(blaster_prog_end) ; 
    call loadprog               ; load cylon to a blaster
    print bootstrap_hlt_txt
    write_bstrap 0000H          ; write a halt to the boot vector
    print donetxt
    mvi a,RAM0                  ; reset the memory mapper
    out MMAP0                   ;
    call mas_reset              ; reset all of the blasters
    call cylon                  ; run cylon for the fun of it
    jmp main_loop               ; think of this as return. the 8008 has a small call stack
    ;ret

; fill_reset: sets the partition start, end and size on each blaster
;             also sets the boot vector to initialize the prime table
;             on each blaster by setting the all values for its
;             partition to true.
fill_reset:
    ; take all blasters and write the partition size
    mvi a,EXTRAM0     ; load register A with a RAM bank
    out MMAP0         ; send it to the mapper
    mvi a,07FH        ; take_w take all of the blasters
    out MAS_TAKE_PORT       ; Take a specific blaster for writing
    mvi_hl part_sz
    mvi m, hi(const_part_sz)
    inr l
    mvi m, lo(const_part_sz)


    ;setup external ram page 0 for all blaster writes
    mvi a,EXTRAM0     ; load register A with a RAM bank
    out MMAP0         ; send it to the mapper
    
    ;blaster 0 from 0-999
    mvi e, 0
    save8 tmp_e, e
    mvi_hl blaster_array
    mvi c, 0
    mvi d, 0
fill_reset_loop:
    mov a, m                    ; get the next blaster in the array
    out MAS_TAKE_PORT           ; Take a specific blaster for writing
    mov_ab_hl                   ; save a copy of the blaster array address
    mvi_hl offset_start         ; overwrites my memory location
    mov m, c                    ; write out the current offset
    inr l                       ;
    mov m, d                    ;
    mov_hl_ab                   ; restore the blaster array address
    mvi a, hi(const_part_sz-1)  ; load the partition size -1 
    mvi b, lo(const_part_sz-1)  ; load the partition size -1 
    call add16                  ; add it to the current partition offset in c d
    mov c, a                    ; move the results to the current partition offset
    mov d, b                    ; move the results to the current partition offset
    mov_ab_hl                   ; save the blaster array address
    mvi_hl offset_end           ; save the current partition offset as the end
    mov m, c                    ;
    inr l                       ;
    mov m, d                    ;
    mov_hl_ab                   ; restore the blaster array address
    mvi a, 0                    ; setup for incrementing the offset in prep for next blaster
    mvi b, 1                    ;
    call add16                  ;
    mov c, a                    ; move the results to the current offset
    mov d, b                    ;
    call inc_addr_hl            ; move to the next blaster in the array
    mov_ab_hl                   ; save the address
    load8 tmp_e, e              ; get e from memory
    inr e                       ; increment loop counter
    save8 tmp_e, e              ; save it
    mov_hl_ab                   ; restore the address
    mov a, e                    ; 
    cpi 7                       ; are we done?
    jnz fill_reset_loop

                                ; write a halt to the boot vector
    write_bstrap blaster_prog_init
    mvi a,RAM0                  ; reset the memory mapper
    out MMAP0                   ;
    call mas_reset              ; reset all of the blasters

    print bootstrap_fill_txt

fill_reset_wait:
    in MAS_STAT_PORT            ; blaster status is low when running
    cpi 01111111B               ; check if all status are high 
    jnz fill_reset_wait         ; if false continue waiting  


    write_bstrap blaster_prog_offs_tbl
    mvi a,RAM0                  ; reset the memory mapper
    out MMAP0                   ;
    call mas_reset              ; reset all of the blasters


offset_tbl_wait:
    in MAS_STAT_PORT            ; blaster status is low when running
    cpi 01111111B               ; check if all status are high 
    jnz offset_tbl_wait         ; if false continue waiting  

    print bootstrap_offs_txt


;; single_step_bs:              ; just moved this to init.  I kept starting prime with the wrong bootstrap

    mvi c, 0                    ; prep to init chk_prime to zero
    mvi d, 0                    ;
                                ; take all blasters and write the number we are checking
    mvi a,EXTRAM0               ; load register A with a RAM bank
    out MMAP0                   ; send it to the mapper
    mvi a,07FH                  ; take_w take all of the blasters
    out MAS_TAKE_PORT           ; Take all blasters for writing
    save16 chk_prime, c, d      ; write a new prime to check on all blasters
                                ; write single step to the boot vector
    write_bstrap blaster_prog_step
    mvi a,RAM0                  ; reset the memory mapper
    out MMAP0                   ;
    call mas_reset              ; reset all of the blasters
    mvi_hl chk_prime            ; initialize check prime to 2
    mvi m, 00h                  ; this needs to come from memory
    inr l
    mvi m, 02h
    print bootstrap_prime_txt
 
    jmp main_loop               ; think of this as return. the 8008 has a small call stack
    ;ret


single_step:
    load16 chk_prime, c, d      ; load master's chk_prime
                                ; take all blasters and write the number we are checking
    mvi a,EXTRAM0               ; load register A with a RAM bank
    out MMAP0                   ; send it to the mapper
    mvi a,07FH                  ; take_w take all of the blasters
    out MAS_TAKE_PORT           ; Take all blasters for writing
    save16 chk_prime, c, d      ; write a new prime to check on all blasters
    mvi a,RAM0                  ; reset the memory mapper
    out MMAP0                   ;
    mvi a,0FFH                  ; untake the blasters
    out MAS_TAKE_PORT           ;
    mvi a, 000H                 ;
    out MAS_INT_PORT            ; interrupt the blasters (take them out of halt)

    load16 chk_prime, c, d      ; increment the chk_prime on master
    mvi a, 0
    mvi b, 1
    call add16
    save16 chk_prime, a, b

    load8 step_out, a           ; do we want to print the current number?
    cpi 1                       ; 
    jnz single_step_wait        ; if false go to the wait section
    save16 decout_value, c, d   ; setup decimal output variable
    call decout                 ; convert to decimal
    print space                 ;
    print decout_result         ;

single_step_wait:
    in MAS_STAT_PORT            ; get the status of all blasters
    out LED_PORT                ; show it on masters leds
    cpi 01111111B               ; wait on all blasters to finish
    jnz single_step_wait        ;           
    ret


single_step_out:
    mvi a, 1                    ; echo the number during single step
    save8 step_out, a           ;
    call single_step            ; one step

    jmp main_loop               ; think of this as return. the 8008 has a small call stack
    ;ret


all_primes:
    mvi a, 0
    mvi b, 0
    save8 step_out, a           ; set step_out = 0
    save16 all_prime_count, a, b; initialize the counter to zero
    save16 prime_out, a, b      ; prime_out = 0
    save8 is_prime_out, a       ; is_prime_out = 0
    save8 row_count,a           ; set row count to zero
    mvi a, hi(const_max_prime)  ; all_prime_stop = const_max_prime
    mvi b, lo(const_max_prime)  ;
    save16 all_prime_stop, a, b ; save it stop
    print newline
    mvi a,EXTRAM0               ; load register A with a RAM bank
    out MMAP0                   ; send it to the mapper
    mvi a,07FH                  ; take_w take all of the blasters
    out MAS_TAKE_PORT           ; Take all blasters for writing
    write_bstrap blaster_prog_all
    mvi a,RAM0                  ; reset the memory mapper
    out MMAP0                   ;
    call mas_reset              ; reset all of the blasters
 
all_primes_loop:
    load16 chk_prime, a, b      ; load master's chk_prime
    save16 tmp_prime, a, b      ; save the current prime before calling single_step
    ;call single_step            ; 

    ;; We can interleave the printing of the previous prime and running the blasters

    mov c, a
    mov d, b
    ;load16 chk_prime, c, d     ; load master's chk_prime
                                ; take all blasters and write the number we are checking
    mvi a,EXTRAM0               ; load register A with a RAM bank
    out MMAP0                   ; send it to the mapper
    mvi a,07FH                  ; take_w take all of the blasters
    out MAS_TAKE_PORT           ; Take all blasters for writing
    save16 chk_prime, c, d      ; write a new prime to check on all blasters
    mvi a,RAM0                  ; reset the memory mapper
    out MMAP0                   ;
    mvi a,0FFH                  ; untake the blasters
    out MAS_TAKE_PORT           ;
    mvi a, 000H                 ;
    out MAS_INT_PORT            ; interrupt the blasters (take them out of halt)

    load16 chk_prime, c, d      ; increment the chk_prime on master
    mvi a, 0
    mvi b, 1
    call add16
    save16 chk_prime, a, b

    ; is_prime_out set?
    ; then print the prime before going into the wait loop
    load8 is_prime_out, a
    cpi 1
    jnz all_primes_wait
    load16 prime_out, a, b
    save16 decout_value, a, b
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
    jnz all_primes_wait         ; if not go to wait
    mvi a, 0                    ; if yes reset row count
    save8 row_count,a           ; save it
    print newline               ; print new line

   

    ; load8 step_out, a           ; do we want to print the current number?
    ; cpi 1                       ; 
    ; jnz single_step_wait        ; if false go to the wait section
    ; save16 decout_value, c, d   ; setup decimal output variable
    ; call decout                 ; convert to decimal
    ; print space                 ;
    ; print decout_result         ;

all_primes_wait:
    in MAS_STAT_PORT            ; get the status of all blasters
    out LED_PORT                ; show it on masters leds
    cpi 01111111B               ; wait on all blasters to finish
    jnz all_primes_wait         ;           

    mvi a, 0
    save8 is_prime_out,a 
    ;;  end

    mvi a,EXTRAM0               ; load register A with a RAM bank
    out MMAP0                   ; send it to the mapper
    mvi_hl blaster_array        ; setup for iterating the blaster array
    mvi e, 0
            
all_primes_loop2:
    mov a, m                    ; get the next blaster in the array
    out MAS_TAKE_PORT           ; Take a specific blaster for writing
    mov_ab_hl                   ; save a copy of the blaster array address
    mvi_hl is_prime             ; was the current value prime on this blaster?
    mov d, m                    ;
    mov_hl_ab                   ; restore blaster array address
    mov a, d                    ; move to A to check
    cpi 1                       ; is true?
    jz all_primes_is_prime      ; print it 
    call inc_addr_hl            ; move to the next blaster in the array
    inr e                       ; increment loop counter
    mov a, e                    ; 
    cpi 7                       ; are we done?
    jz all_primes_next          ; not prime go to the next number

    jmp all_primes_loop2

all_primes_is_prime:
    mvi a,RAM0                  ; reset the memory mapper
    out MMAP0                   ;
    mvi a,0FFH                  ; untake the blasters
    out MAS_TAKE_PORT           ;
    load16 tmp_prime, a, b
    save16 prime_out, a, b
    mvi a, 1
    save8 is_prime_out, a

;; all_primes_print:
;;     mvi a,RAM0                  ; reset the memory mapper
;;     out MMAP0                   ;
;;     mvi a,0FFH                  ; untake the blasters
;;     out MAS_TAKE_PORT           ;
;;     load16 prime_out, a, b      ; get the saved prime
;;     save16 decout_value, a, b   ; save it to decimal output parameter
;;     call decout                 ; convert to decimal
;;     print decout_result         ; print it
;;     print space                 ;
;;     load8 row_count,a           ; get the current row_count
;;     mov c, a                    ; save a copy in c
;;     mvi b, 1                    ; increment by one
;;     add b                       ; increment
;;     save8 row_count,a           ; save it
;;     mov a, c                    ; restore A before inc
;;     cpi 9                       ; are we at max columns?
;;     jnz all_primes_next         ; if not go to next prime
;;     mvi a, 0                    ; if yes reset row count
;;     save8 row_count,a           ; save it
;;     print newline               ; print new line

all_primes_next:
    mvi a,RAM0                  ; reset the memory mapper
    out MMAP0                   ;
    mvi a,0FFH                  ;
    out MAS_TAKE_PORT           ; untake the blasters
    load16 all_prime_count, a, b; get the counter
    mvi c, 0                    ; increment by one
    mvi d, 1                    ;
    call add16                  ;
    save16 all_prime_count, a, b; save it
    load16 all_prime_stop, c, d ; get the stop
    call cmp16                  ; are we done?
    jnz all_primes_loop         ; no print next prime

    jmp main_loop               ; think of this as return. the 8008 has a small call stack
    ;ret


main:
    print menutxt
main_loop:
    print prompttxt
    call getch
    cpi 'a'                     ; is the input character below 'a'?
    jc $+5                      ; skip the next instruction if the character is already upper case
    sui 20H                     ; else, convert to the character to upper case
    call putch                  ; echo the character
    cpi 'L'
    jz copier                   ; copy to the blasters
    cpi 'I'
    jz fill_reset               ; fill the prime table with true
    ;cpi 'B'
    ;cz single_step_bs           ; bootstrap the prime code
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
    db  "L - Load prime program\r\n"
    db  "I - Initalize blasters\r\n"
    db  "S - Single step\r\n"
    db  "A - Print all\r\n"
    db  "Q - Quit\r\n"
    db 0

prompttxt:
    db  "\r\n>>",0
 
loadtxt:
    db "\r\nLoading code on the Blasters\r\n", 0

bootstrap_hlt_txt:
    db "Writing hlt bootstrap to Blasters\r\n", 0

bootstrap_fill_txt:
    db "\r\nWriting fill bootstrap to Blasters\r\n", 0

bootstrap_prime_txt:
    db "Writing prime bootstrap to Blasters\r\n", 0

bootstrap_offs_txt:
    db "Writing offset bootstrap to Blasters\r\n", 0

bootstrap_all_txt:
    db "\r\nWriting print all bootstrap to Blasters\r\n", 0

donetxt:
    db "Loading Finished\r\n", 0

newline:
    db "\r\n", 0

space:
    db " ", 0


;----------------------------------------
; blaster bootstrap programs 
blaster_prog_init:
    call blaster_fill
    hlt
    jmp blaster_prog_init

blaster_prog_step:
    load16 chk_prime, c, d          ; if the value hasn't been initialized just halt
    mvi a, 0
    mvi b, 0
    call cmp16
    cnz blaster_prime_step
    hlt
    jmp blaster_prog_step

blaster_prog_all:
    call blaster_prime_step
    hlt
    jmp blaster_prog_all

blaster_prog_offs_tbl:
    call blaster_offs_tbl
    hlt
    jmp blaster_offs_tbl


;----------------------------------------
; filler: set the value to 1
blaster_fill:
    mvi b, 0
    mvi c, 0
    mvi_hl part_sz
    mov d, m
    inr l
    mov e, m
    mvi_hl 2000H
bf_loop:
    mvi m, 1
    call inc_addr_hl
    call inc16
    mov a, b
    mov b, c
    mov c, d
    mov d, e
    mov e, a; equal16 overwrites a
    call cmp16
    mov a, e
    mov e, d
    mov d, c
    mov c, b
    mov b, a
    jnz bf_loop
    ret

;----------------------------------------
; prime step: run one iteration of prime
blaster_prime_step:
    mvi a, 1                        ; debug
    out LED_PORT                    ; debug
 
    mvi a, 0                        ; set is prime to false, we check verify later
    save8 is_prime, a               ;
    save8 lt_sqrt, a                ; lt_sqrt = 0
    load16 chk_prime, a, b          ; get the current value we're checking for primality
    save16 current_val, a, b        ; save it as the current value used by the offset function
    mvi c, hi(const_max_sqrt)       ; have we already crossed everything?
    mvi d, lo(const_max_sqrt)       ;
    mov e, a
    call cmp16                      ; is chk_prime < sqrt(max_primes)
    jc bps_get_offset               ; we still need to get the offset for cross off the list
    ;;;;;;;;;;;;;;
    mov c, e
    mov d, b
    load16 min_part_val, a, b           ; get the lower bounds (store in a b first op of the compare)
    call cmp16                          ; min_part_val <= current_val
    jz bps_in_range                     ; They are equal so we are in range
    jnc bps_done                        ; current_val is less than min_range
                                        ; we need to check a multiple (get_offset_lt)
                                        ; otherwise
                                        ; min_part_val is less than the current_val
                                        ; need to check current_val <= max next
                                        ; need to check current_val <= max next
    mov a, c
    mov b, d
    mov e, a
    load16 max_part_val, c, d           ; load max_part_val into the second op
    call cmp16                          ; current_val <= max_part_val

    jz bps_in_range                     ; they are equal so we are in range
    jnc bps_done                        ; current_val is greater than max_part_val
 
    ;;;;;;;;;;;;;
    ;call in_range                   ; we do not need to cross anything off, just check if prime
    ;jnz bps_done                    ; If we are not in range we are done
bps_in_range:
    mov a, e
    ; already in a, b
    ;load16 chk_prime, a, b          ; get the current number to check
    load16 min_part_val, c, d       ; get the starting offset
    call sub16                      ; value - offset
    save16 offset_res, a, b         ; set the results to the offset results
    jmp bps_prime_check             ; check if it is prime
bps_get_offset:               
    ; get the offset table pointer
    ; add the current value to it
    ; get the value from the table
    ; store it as the offset result
    mvi a, 1
    save8 lt_sqrt, a
    mvi c, hi(offset_tbl)
    mvi d, lo(offset_tbl)

    load16 chk_prime, a, b          ; set the table index to the current number
    mov e, c                        ; shl16 clobbers c
    call shl16                      ; prime * 2 to handle 16-bits
                                    ; since we are limtied to a total of 2^16 primes sqrt(2^16) <= 256
                                    ; ... we do not need to worry about a 32 bit shl
    mov c, e
    call add16                      ; offset the pointer

    mov_hl_ab                       ; move to hl for memory access
    mov c, m                        ; get high byte
    call inc_addr_hl                ; inc index
    mov d, m                        ; get low byte

    save16 offset_res, c, d         ; check if we are out of bounds
    mov a, c                        ; need to check if C out of bounds
    mov b, d                        ; move to b for prime check
    cpi OUT_OF_BOUNDS               ;
    jz  bps_done                    ; if we are then exit
bps_prime_check:
    mvi c, hi(2000H)                ; load the location of the parition table
    mvi d, lo(2000H)                ;
    call add16                      ; offset into the table
    mov_hl_ab                       ; move the offset to the pointer
    mov a, m                        ; get the boolean value
    cpi 1                           ; is it true
    jnz bps_skip_value_chk          ; already crossed off so skip the value check

    mov e, a                        ; debug
    mvi a, 2                        ; debug
    out LED_PORT                    ; debug
    mov a, e                        ; debug
 
    load16 offset_start, a, b       ; calc the offset value (offset_start + offset)
    load16 offset_res, c, d         ;
    call add16                      ; offset_start + offset
    load16 chk_prime, c, d          ; load the number we are checking
    call cmp16                      ; does it equal the offset value?  Then it is prime
    jnz bps_skip_value_chk          ; otherwise skip it

    mov e, a                        ; debug
    mvi a, 3                        ; debug
    out LED_PORT                    ; debug
    mov a, e                        ; debug
 
    mvi a, 1                        ; 
    save8 is_prime, a               ; mark it as true
bps_skip_value_chk:
    load8 lt_sqrt, a                ; is chk_prime < sqrt(max_primes)
    ani 1
    jz bps_done                     ; no, then we are done

    load16 part_sz, b, c            ; get the parition size to calc the end address
    mvi_hl 2000H                    ; point to the sieve
    mov a, c                        ; &seive + size
    add l                           ; 
    mov e, a                        ; store the low byte in E
    mov a, b                        ;
    adc h                           ;
    mov d, a                        ; store the high byte in D

    load16 offset_res, b, c         ; get the offset result from memory
    mvi_hl 2000H                    ; 
    mov a, c                        ;
    add l                           ;
    mov l, a                        ;
    mov a, b                        ;
    adc h                           ;
    mov h, a                        ; update the address to the first offset

    mov a, h                        ; save the calculated address
    mov b, l                        ; 
    mvi_hl chk_prime                ; get chk_prime
    inr l                           ;
    mov c, m                        ; chk_prime is 8-bits here since 256^2 == max(16-bits)
    mov h, a                        ; restore the calculated address
    mov l, b                        ;
    mvi b, 0                        ; init high byte of chk_prime to zero
                                    ; start address h l
                                    ; end address   d e
                                    ; chk_prime     b c
bps_mark_loop:
    mvi m, 0                        ; set to zero
    mov a, c                        ; &start_address += chk_prime
    add l                           ;
    mov l, a                        ;
    mov a, b                        ;
    adc h                           ;
    mov h, a                        ;
    cmp d                           ; start_address < end_address
    jc bps_mark_loop                ;
    mov a, l                        ;
    cmp e                           ;
    jc bps_mark_loop                ;

bps_done:
    ret


blaster_offs_tbl:
    mvi a, 0                        ; we start building the offset table from two   
    mvi b, 2                        ;
    save16 current_val, a, b        ; save it as the current value
    load16 offset_start, a, b       ; setup the start of the partition
    save16 min_part_val, a, b       ;
    load16 offset_end, a, b         ; setup the end of the partition
    save16 max_part_val, a, b       ;
    mvi b, hi(offset_tbl)           ; setup the table index
    mvi a, lo(offset_tbl)           ;
    mvi e, 4                        ; start at position 2 since we are 16 bits that is 4
    add e                           ;
    save16 ofs_tbl_idx, b, a        ;
bot_loop:
    mvi c, hi(const_max_sqrt)       ; have we already crossed everything?
    mvi d, lo(const_max_sqrt)       ;
    load16 current_val,a,b          ;

    mov e, a                        ; debug
    mov a, b                        ; debug
    out LED_PORT                    ; debug
    mov a,e                         ; debug

    call cmp16                      ; is chk_prime < sqrt(max_primes)
    jc bot_get_offset               ; we still need to get the offset for cross off the list

    mvi a, 0                        ; debug
    out LED_PORT                    ; debug

    ret
bot_get_offset:               
    call get_offset                 ; get the offset
    load16 offset_res, c, d         ; save the offset
    mvi_hl ofs_tbl_idx              ; prep for load
    call load_pointer               ; load the offset index to h l
    mov m, c                        ; save the hi byte
    call inc_addr_hl                ; inc the index
    mov m, d                        ; save the low byte
    call inc_addr_hl                ; inc the index
    mov_ab_hl                       ; move the pointer to a b to save it
    save16 ofs_tbl_idx, a, b        ; save it
    load16 current_val, b, c        ; get the current value
    call inc16                      ; inc it
    save16 current_val, b, c        ; save it
    jmp bot_loop                    ; check the next value
                                    ; return is in the loop

blaster_prog_end: db 0

; Do not copy these variables to the blaster
;
;----------------------------------------
; Scratch Memory
PROG_READ_ADDR: db 0H, 0H
PROG_END_ADDR:  db 0H, 0H
PROG_WRITE_ADDR: db 0H, 0H  
blaster_array: db blaster0, blaster1, blaster2, blaster3, blaster4, blaster5, blaster6
tmp_e: db 0h


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

    org 0300H

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



