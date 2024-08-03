mov io r1

loop:
    shr r1 1 r2
    xor r1 r2 r2
    shl r2 1 r3
    and r3 255 r3
    xor r2 r3 r4
    shr r4 2 r5
    xor r4 r5 r1
    and r1 3 r2
    mov r2 io
    jmp loop