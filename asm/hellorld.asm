    include "bitfuncs.inc"
    cpu 8008new

    org 1000H

puts: equ 26D6H
prompt: equ 2095H

textout:   mvi h,hi(text)
           mvi l,lo(text) 
           call puts
           call prompt

    org 1100H
text:
    db "\r\n"
    db "Hellorld\r\n",0
