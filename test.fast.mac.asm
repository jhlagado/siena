.macro expect,msg1,val1
    POP HL
    PUSH HL
    LD DE,val1
    OR A
    SBC HL,DE
    LD A,L
    OR H
    JR Z,expect%%M

    CALL prtstr
    .cstr msg1,"\r\nActual: "
    CALL prtdec

    CALL prtstr
    .cstr "\r\nExpected: "
    LD HL,val1
    CALL prtdec

    HALT
    .cstr
expect%%M:
    POP HL
.endm

.macro test,code1,val1
    LD SP,DSTACK
    CALL init
    CALL enter
    .cstr code1
    expect code1,val1
.endm
