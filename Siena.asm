; *************************************************************************
;
;  Siena programming language for the Z80 
;
;  by John Hardy 2022
;
;  Incorporating code from the MINT project by Ken Boak and Craig Jones. 
;
;  GNU GENERAL PUBLIC LICENSE    Version 3, 29 June 2007
;
;  see the LICENSE file in this repo for more information 
;
; *****************************************************************************

                                ; 
    DSIZE  EQU $80
    RSIZE  EQU $80
    TIBSIZE     EQU $100	    ; 256 bytes , along line!
    TRUE    EQU 1		        ; not FF, for Siena
    FALSE  EQU 0
    EMPTY  EQU 0		        ; for an empty macro, ctrl-<something>=macro, ie ctrl-h = backspace macros (in Siena)

    DATASIZE    EQU 26*2*2	    ; a..z, a..z words

; **************************************************************************
; Page 0  Initialisation
; **************************************************************************		

.org ROMSTART + $180		; 0+180 put Siena code from here	

; **************************************************************************
; this code must not span pages
; **************************************************************************
macros:

; ***********************************************************************
; Initial values for user mintVars		
; ***********************************************************************		
iAltVars:			            ; value copied into tables
    DW dStack                   ; a vS0 start of datastack			
    DW 0                        ; b  
    DW 0                        ; c vTIBPtr an offset to the tib
    DW 0                        ; d 
    DW 0                        ; e 
    DW 0                        ; f 
    DW 0                        ; g 
    DW HEAP                     ; h vHeapPtr \h start of the free mem

    .align $100
iOpcodes:
    DB lsb(exit_)               ; NUL 
    DB lsb(nop_)                ; SOH 
    DB lsb(nop_)                ; STX 
    DB lsb(etx_)                ; ETX 
    DB lsb(nop_)                ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)    ;     
    DB lsb(nop_)                ;    !  
    DB lsb(nop_)                ;    "
    DB lsb(hexnum_)             ;    #
    DB lsb(arg_)                ;    $  
    DB lsb(nop_)                ;    %  
    DB lsb(nop_)                ;    &
    DB lsb(strDef_)             ;    '
    DB lsb(lambda_)             ;    (    
    DB lsb(return_)             ;    )
    DB lsb(nop_)                ;    *  
    DB lsb(newAdd2_)            ;    +
    DB lsb(nop_)                ;    ,  
    DB lsb(num_)                ;    -
    DB lsb(dot_)                ;    .
    DB lsb(nop_)                ;    /	
    DB lsb(num_)                ;    0     
    DB lsb(num_)                ;    1    
    DB lsb(num_)                ;    2    
    DB lsb(num_)                ;    3    
    DB lsb(num_)                ;    4    
    DB lsb(num_)                ;    5    
    DB lsb(num_)                ;    6    
    DB lsb(num_)                ;    7    
    DB lsb(num_)                ;    8    
    DB lsb(num_)                ;    9    
    DB lsb(nop_)                ;    :    
    DB lsb(nop_)                ;    ;
    DB lsb(nop_)                ;    <
    DB lsb(nop_)                ;    =  
    DB lsb(nop_)                ;    >  
    DB lsb(nop_)                ;    ?    
    DB lsb(fetch_)              ;    @  
    DB lsb(call_)               ;    A     
    DB lsb(call_)               ;    B     
    DB lsb(call_)               ;    C     
    DB lsb(call_)               ;    D     
    DB lsb(call_)               ;    E     
    DB lsb(call_)               ;    F     
    DB lsb(call_)               ;    G     
    DB lsb(call_)               ;    h     
    DB lsb(call_)               ;    I     
    DB lsb(call_)               ;    J     
    DB lsb(call_)               ;    K     
    DB lsb(call_)               ;    L     
    DB lsb(call_)               ;    M     
    DB lsb(call_)               ;    N     
    DB lsb(call_)               ;    O     
    DB lsb(call_)               ;    p     
    DB lsb(call_)               ;    Q     
    DB lsb(call_)               ;    R     
    DB lsb(call_)               ;    S     
    DB lsb(call_)               ;    T     
    DB lsb(call_)               ;    U     
    DB lsb(call_)               ;    V     
    DB lsb(call_)               ;    W     
    DB lsb(call_)               ;    X     
    DB lsb(call_)               ;    Y     
    DB lsb(call_)               ;    Z    
    DB lsb(nop_)                ;    [
    DB lsb(nop_)                ;    \
    DB lsb(nop_)                ;    ]
    DB lsb(nop_)                ;    ^
    DB lsb(nop_)                ;    _
    DB lsb(nop_)                ;    `    	    
    DB lsb(a_)                  ;    a     
    DB lsb(var_)                ;    b  
    DB lsb(c_)                  ;    c  
    DB lsb(d_)                  ;    d  
    DB lsb(e_)                  ;    e  
    DB lsb(f_)                  ;    f  
    DB lsb(g_)                  ;    g  
    DB lsb(var_)                ;    h  
    DB lsb(i_)                  ;    i  
    DB lsb(var_)                ;    j  
    DB lsb(k_)                  ;    k  
    DB lsb(l_)                  ;    l  
    DB lsb(m_)                  ;    m  
    DB lsb(m_)                  ;    n  
    DB lsb(o_)                  ;    o  
    DB lsb(p_)                  ;    p  
    DB lsb(var_)                ;    q  
    DB lsb(r_)                  ;    r  
    DB lsb(s_)                  ;    s  
    DB lsb(var_)                ;    t  
    DB lsb(u_)                  ;    u  
    DB lsb(var_)                ;    v  
    DB lsb(w_)                  ;    w  
    DB lsb(x_)                  ;    x  
    DB lsb(var_)                ;    y  
    DB lsb(var_)                ;    z  
    DB lsb(nop_)                ;    {
    DB lsb(or_)                 ;    |  
    DB lsb(nop_)                ;    }  
    DB lsb(nop_)                ;    ~    
    DB lsb(nop_)                ;    DEL	
    DB lsb(EMPTY)               ; NUL ^@    
    DB lsb(EMPTY)               ; SOH ^a  1
    DB lsb(EMPTY)               ; STX ^b  2
    DB lsb(EMPTY)               ; ETX ^c  3
    DB lsb(EMPTY)               ; EOT ^d  4
    DB lsb(EMPTY)               ; ENQ ^e  5
    DB lsb(EMPTY)               ; ACK ^F  6
    DB lsb(EMPTY)               ; BEL ^G  7 
    DB lsb(EMPTY)               ; BS  ^h  8
    DB lsb(EMPTY)               ; TAB ^I  9
    DB lsb(EMPTY)               ; LF  ^J 10
    DB lsb(EMPTY)               ; VT  ^K 11
    DB lsb(EMPTY)               ; FF  ^l 12
    DB lsb(EMPTY)               ; CR  ^m 13
    DB lsb(EMPTY)               ; SO  ^N 14
    DB lsb(EMPTY)               ; SI  ^O 15
    DB lsb(EMPTY)               ; DLE ^p 16
    DB lsb(EMPTY)               ; ^Q     
    DB lsb(EMPTY)               ; ^R     
    DB lsb(EMPTY)               ; ^S    
    DB lsb(EMPTY)               ; ^T    
    DB lsb(EMPTY)               ; ^U       
    DB lsb(EMPTY)               ; ^V  
    DB lsb(EMPTY)               ; ^W    
    DB lsb(EMPTY)               ; ^X    
    DB lsb(EMPTY)               ; ^Y    
    DB lsb(EMPTY)               ; ^Z    
    DB lsb(EMPTY)               ; ^[  
    DB lsb(EMPTY)               ; ^\  
    DB lsb(EMPTY)               ; ^]  
    DB lsb(EMPTY)               ; ^^  
    DB lsb(EMPTY)               ; ^_  
    DB lsb(aNop_)  ;a0    sp  				;space
    DB lsb(aNop_)  ;a1    \!  			; this is a bug shud be lsb(cstore_)     
    DB lsb(aNop_)  ;a2    \"  				
    DB lsb(aNop_)  ;a3    \#  utility command		; table of special routines ie #5 etc				
    DB lsb(aNop_)  ;a4    \$  prints a newline to output	
    DB lsb(aNop_)  ;     
    DB lsb(aNop_)  ;     
    DB lsb(aNop_)  ;     
    DB lsb(aNop_)  ;a8    \(  ( b -- )    
    DB lsb(aNop_)  ;a9    \)       
    DB lsb(aNop_)  ;aa    \*       
    DB lsb(aNop_)  ;ab    \+       
    DB lsb(aNop_)  ;ac    \,  ( b -- ) prints a char    
    DB lsb(aNop_)  ;ad    \-       
    DB lsb(aNop_)  ;ae    \.  ( b -- ) prints a string from add term by null char    
    DB lsb(aNop_)  ;af    \/       
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;ba    \:	return add of a anon def, \: 1 2 3;    \\ ret add of this       
    DB lsb(aNop_)  ;bb    \;       
    DB lsb(aNop_)  ;bc    \<  ( port -- val )
    DB lsb(aNop_)  ;bd    \=    
    DB lsb(aNop_)  ;be    \>  ( val port -- )
    DB lsb(aNop_)  ;bf    \?
    DB lsb(aNop_)     ;c0    \@ byte fetch
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)    ;     
    DB lsb(aNop_)        
    DB lsb(aNop_)                       
    DB lsb(aNop_)                           
    DB lsb(aNop_)                           
    DB lsb(aNop_)                              
    DB lsb(aNop_)                           
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)          
    DB lsb(aNop_)        
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)
    DB lsb(aNop_)             
    DB lsb(aNop_)                
    DB lsb(aNop_)        
    DB lsb(aNop_)    
    DB lsb(aNop_)    

etx:        
    ld hl,-DSTACK
    add hl,sp
    jr nc,etx1
    ld sp,DSTACK
etx1:
    jr interpret

start:
    ld sp,DSTACK		        ; start of Siena
    call init		            ; setups
    call printStr		        ; prog count to stack, put code line 235 on stack then call print
    .cstr "Siena V0.0\r\n"

interpret:
    call prompt

    ld bc,0                     ; load bc with offset into TIB, decide char into tib or execute or control    
    ld (vTIBPtr),bc

interpret2:                     ; calc nesting (a macro might have changed it)
    ld e,0                      ; initilize nesting value
    push bc                     ; save offset into TIB, 
                                ; bc is also the count of chars in TIB
    ld hl,TIB                   ; hl is start of TIB
    jr interpret4

interpret3:
    ld a,(hl)                   ; a = char in TIB
    inc hl                      ; inc pointer into TIB
    dec bc                      ; dec count of chars in TIB
    call nesting                ; update nesting value

interpret4:
    ld a,c                      ; is count zero?
    or b
    jr nz, interpret3           ; if not loop
    pop bc                      ; restore offset into TIB
    
waitchar:    
    call getchar                ; loop around waiting for character from serial port
    cp $20			            ; compare to space
    jr nc,waitchar1		        ; if >= space, if below 20 set cary flag
    cp $0                       ; is it end of string? null end of string
    jr z,waitchar4
    cp '\r'                     ; carriage return? ascii 13
    jr z,waitchar3		        ; if anything else its macro/control 

macro:       
    ld (vTIBPtr),bc
    ld hl,ctrlCodes
    add a,l			            ; look up key of macros
    ld l,a
    ld e,(hl)
    ld a,e
    or a
    jr z,macro1
    ld d,msb(macros)
    push de
    call ENTER		            ; Siena go operation and jump to it
    .cstr "go"
macro1:
    ld bc,(vTIBPtr)
    jr interpret2

waitchar1:
    ld hl,TIB
    add hl,bc
    ld (hl),a                   ; store the character in textbuf
    inc bc
    call putchar                ; echo character to screen
    call nesting
    jr  waitchar                ; wait for next character

waitchar3:
    ld hl,TIB
    add hl,bc
    ld (hl),"\r"                ; store the crlf in textbuf
    inc hl
    ld (hl),"\n"  
    inc hl    
    inc bc
    inc bc
    call crlf                   ; echo character to screen
    ld a,e                      ; if zero nesting append and ETX after \r
    or a
    jr nz,waitchar
    ld (hl),$03                 ; store end of text ETX in text buffer 
    inc bc

waitchar4:    
    ld (vTIBPtr),bc
    ld bc,TIB                   ; Instructions stored on heap at address HERE, we pressed enter
    dec bc

; ********************************************************************************
;
; Dispatch Routine.
;
; Get the next character and form a 1 byte jump address
;
; This target jump address is loaded into hl, and using jp (hl) to quickly 
; jump to the selected function.
;
; Individual handler routines will deal with each category:
;
; 1. Detect characters a-z and jump to the User Command handler routine
;
; 2. Detect characters a-z and jump to the variable handler routine
;
; 3. All other characters are punctuation and cause a jump to the associated
; primitive code.
;
; Instruction Pointer IP bc is incremented
;
; *********************************************************************************

next:        
    inc bc                      ;  Increment the IP
    ld a, (bc)                  ;  Get the next character and dispatch
    ld l,a                      ;  Index into table
    ld h,msb(iOpcodes)           ;  Start address of jump table    
    ld l,(hl)                   ;  get low jump address
    ld h,msb(page4)             ;  Load h with the 1st page address
    jp (hl)                     ;  Jump to routine

init:       
    ld iy,DSTACK
    ld ix,RSTACK
    ld hl,ialtVars
    ld de,altVars
    ld bc,8 * 2
    ldir
    
    ld hl,data                  ; init namespaces to 0 using ldir
    ld de,hl
    inc de
    ld (hl),0
    ld bc,DATASIZE
    ldir
    ret
    
; initOps:
;     ld hl, iOpcodes
;     ld de, opcodes
;     ld bc, 256

; initOps1:
;     ld a,(hl)
;     inc hl
;     sla a       
;     ret z
;     jr c, initOps2
;     srl a
;     ld c,a
;     ld b,0
;     ldir
;     jr initOps1
    
; initOps2:    
;     srl a
;     ld b,a
;     ld a,(hl)
;     inc hl
; initOps2a:
;     ld (de),a
;     inc de
;     djnz initOps2a
;     jr initOps1

enter:     ;=9
    ld hl,bc
    call rpush                  ; save Instruction Pointer
    pop bc
    dec bc
    jp next     

printStr:        
    ex (sp),hl		            ; swap			
    call putStr		
    inc hl			            ; inc past null
    ex (sp),hl		            ; put it back	
    ret

lookupRef:
    ld d,0
lookupRef0:
    cp "a"
    jr nc,lookupRef2
lookupRef1:
    sub "a"
    ld e,0
    jr lookupRef3    
lookupRef2:
    sub "a"
    ld e,26*2
lookupRef3:
    add a,a
    add a,e
    ld hl,DATA
    add a,l
    ld l,a
    ld a,0
    adc a,h
    ld h,a
    xor a
    or e                        ; sets z flag if a-z
    ret

; **************************************************************************    
; calculate nesting value
; a is char to be tested, 
; e is the nesting value (initially 0)
; e is increased by ( and [ 
; e is decreased by ) and ]
; e has its bit 7 toggled by `
; limited to 127 levels
; **************************************************************************    

nesting:    
    cp $22                      ; quote char
    jr nz,nesting1
    bit 7,e
    jr z,nesting1a
    res 7,e
    ret
nesting1a: 
    set 7,e
    ret
nesting1:
    bit 7,e    
    ret nz    
    cp '{'
    jr z,nesting2
    cp '['
    jr z,nesting2
    cp '('
    jr nz,nesting3
nesting2:
    inc e
    ret
nesting3:
    cp '}'
    jr z,nesting4
    cp ']'
    jr z,nesting4
    cp ')'
    ret nz
nesting4:
    dec e
    ret 
    
; **********************************************************************			 
; Page 4 primitive routines 
; **********************************************************************
    .align $100
page4:

num_:    
    jp  num
hexnum_:    
    jp hexnum
arg_:
    jp arg
strDef_:
    jp strDef
newAdd2_:
    jp newAdd2
lambda_:    
    jp lambda
go_:
    jp go
return_:
    jp return

dot_:  
    pop hl
    call prtdec
dot2:
    ld a,' '       
    call putchar
    jp next
a_:
    jp a
c_:
    jp c
d_:
    jp d
e_:
    jp e
f_:
    jp f
g_:
    jp g
i_:
    jp i
k_:
    jp k
l_:
    jp l
m_:
    jp m
n_:
    jp n
o_:
    jp o
p_:
    jp p
r_:
    jp r
s_:
    jp s
u_:
    jp u
w_:
    jp w
x_:
    jp x

and_:    
    pop de                      ; Bitwise and the top 2 elements of the stack
    pop hl     
    ld a,e        
    and l           
    ld l,a        
    ld a,d        
    and h           
and1:
    ld h,a        
    push hl         
    jp next        
    
         
or_: 		 
    pop de                      ; Bitwise or the top 2 elements of the stack
    pop hl
    ld a,e
    or l
    ld l,a
    ld a,d
    or h
    jr and1

xor_:		 
    pop de                      ; Bitwise xor the top 2 elements of the stack
xor1:
    pop hl
    ld a,e
    xor     l
    ld l,a
    ld a,d
    xor     h
    jr and1

inv_:				            ; Bitwise INVert the top member of the stack
    ld de, $FFFF                ; by xoring with $FFFF
    jr xor1    
    
add_:                           ; add the top 2 members of the stack
    pop de        
    pop hl        
    add hl,de    
    push hl        
    jp next    
        
call_:
    ld a,(bc)
    call lookupRef1
    ld e,(hl)
    inc hl
    ld d,(hl)
    jp go1

hdot_:                          ; print hexadecimal
    pop hl
    call prthex
    jr    dot2

drop_:                          ; Discard the top member of the stack
    pop hl
    jp next

undrop_:
    dec sp
    dec sp
    jp next
    
dup_:    
    pop hl                      ; Duplicate the top member of the stack
    push    hl
    push    hl
    jp next
etx_:
    jp ETX
    
exit_:
    inc bc			            ; store offests into a table of bytes, smaller
    ld de,bc       
    call rpop                   ; Restore Instruction pointer
    ld bc,hl
    ex de,hl
    jp (hl)
    
fetch_:                         ; Fetch the value from the address placed on the top of the stack 
    pop hl    
fetch1:
    ld e,(hl)    
    inc hl    
    ld d,(hl)    
    push de    
    jp next       


key_:
    call getchar
    ld h,0
    ld l,a
    push hl
    jp next

mul_:    jp mul 

nop_:  
    jp next                     ; hardwire white space to always go to next (important for arrays)


over_:  
    pop hl                      ; Duplicate 2nd element of the stack
    pop de
    push de
    push hl
    push de                     ; and push it to top of stack
    jp next    
    
rot_:                           ; a b c -- b c a
    pop de                      ; a b    de = c
    pop hl                      ; a       hl = b
    ex (sp),hl                  ; b       hl = a
    push de                     ; b c    
    push hl                     ; b c a     
    jp next

                                ;  Left shift { is multiply by 2		
shl_:    
    pop hl                      ; Duplicate the top member of the stack
    add hl,hl
    push hl                     ; shift left fallthrough into add_     
    jp next            
    
				                ;  Right shift } is a divide by 2		
shr_:    
    pop hl                      ; Get the top member of the stack
shr1:
    srl h
    RR l
    push hl
    jp next            

store_:                         ; Store the value at the address placed on the top of the stack
    pop hl     
    pop de     
    ld (hl),e     
    inc hl    
    ld (hl),d     
    jp next  
          
swap_:                          ; a b -- b a Swap the top 2 elements of the stack
    pop hl
    ex (sp),hl
    push hl
    jp next
    
neg_:    
    ld hl, 0    		        ; NEGate the value on top of stack (2's complement)
    pop de       
    jr sub2                     ; use the SUBtract routine
    
sub_:  		                    ; Subtract the value 2nd on stack from top of stack 
    pop de    
    pop hl                      ; Entry point for INVert
sub2:    
    or a                        ; Entry point for NEGate
    sbc hl,de       
    push hl        
    jp next        
        
eq_:    
    pop hl
    pop de
    or a                        ; reset the carry flag
    sbc hl,de                   ; only equality sets hl=0 here
    jr z, true_
false_:
    ld hl, 0
    push hl
    jp next 

gt_:    
    pop de
    pop hl
    jr lt1
    
lt_:    
    pop hl
    pop de
lt1:    
    or a                        ; reset the carry flag
    sbc hl,de    
	    jr z,false_    
    jp m,false_
true_:
    ld hl, 1
    push hl
    jp next 

gte_:    
    pop de
    pop hl
    jr lte1
lte_:    
    pop hl
    pop de
lte1:    
    or a                        ; reset the carry flag
    sbc hl,de    
    jp m,false_
    jp true


var_:
    ld a,(bc)
    call lookupRef2
    push hl
    jp next
    
    
div_:    
    jr div


;*******************************************************************
; Page 5 primitive routines 
;*******************************************************************
    ;falls through 

    push hl
    jp next

; ********************************************************************
; 16-bit division subroutine.
;
; bc: divisor, de: dividend, hl: remainder

; *********************************************************************  
; This divides de by bc, storing the result in de, remainder in hl
; *********************************************************************

; 1382 cycles
; 35 bytes (reduced from 48)
		

div:        ;=34
    pop  de       ; get first value
    pop  hl       ; get 2nd value
    push bc       ; Preserve the IP
    ld b,h        ; bc = 2nd value
    ld c,l		
		
    ld hl,0    	  ; Zero the remainder
    ld a,16    	  ; Loop counter

div1:		     ;shift the bits from bc (numerator) into hl (accumulator)
    sla c
    rl b
    adc hl,hl

    sbc hl,de		    ;Check if remainder >= denominator (hl>=de)
    jr c,div2
    inc c
    jr div3
div2:		     ; remainder is not >= denominator, so we have to add de back to hl
    add hl,de
div3:
    dec a
    jr nz,div1
    ld d,b        ; Result from bc to de
    ld e,c
div4:    
    pop  bc       ; Restore the IP
    push de       ; push Result
    push hl       ; push remainder    

    jp next

    	     ;=57       

; **************************************************************************
; Page 6 Alt primitives
; **************************************************************************
    .align $100
page6:

anop_:
    jp next     

cFetch_:
    pop hl     
    ld d,0  
    ld e,(hl)    
    push    de    
    jp next       
  
comment_:
    inc bc        ; point to next char
    ld a,(bc)
    cp "\r"       ; terminate at cr 
    jr nz,comment_
    dec bc
    jp  next 

cStore_:	  
    pop    hl     
    pop    de     
    ld     (hl),e     
    jp next  
          
emit_:
    pop hl
    ld a,l
    call putchar
    jp next

exec_:
    call exec1
    jp next
exec1:
    pop hl
    ex (sp),hl
    jp (hl)

prompt_:
    call prompt
    jp next


inPort_:			    ; \<
    pop hl
    ld a,c
    ld c,l
    in l,(c)
    ld h,0
    ld c,a
    push hl
    jp next    

newln_:
    call crlf
    jp next    

outPort_:
    pop hl
    ld e,c
    ld c,l
    pop hl
    out (c),l
    ld c,e
    jp next    

prtstr_:
prtstr:
    pop hl
    call putStr
    jp next


rpush_:
    pop hl
    call rpush
    jp next

rpop_:
    call rpop
    push hl
    jp next

; **************************************************************************
; Page 6 primitive routines continued  (page 7) 
; **************************************************************************
    ; falls through to following page
a:
    inc bc
    ld a,(bc)
    cp 'd'    
    jp z,add_
    cp 'n'    
    jp z,and_
    dec bc
    jp var_
    
c:    
    inc bc
    ld a,(bc)
    cp 'a'    
    jp z,case_
    cp 'l'    
    jp z,closure_
    dec bc
    jp var_
    
d:    
    inc bc
    ld a,(bc)
    cp 'e'    
    jp z,def_
    cp 'i'    
    jp z,div_
    cp 'r'    
    jp z,drop_
    cp 'u'    
    jp z,dup_
    dec bc
    jp var_

e:
    inc bc
    ld a,(bc)
    cp 'q'    
    jp z,eq_
    dec bc
    jp var_

f:
    inc bc
    ld a,(bc)
    cp 'i'    
    jp z,filter_
    dec bc
    jp var_

g:
    inc bc
    ld a,(bc)
    cp 'e'    
    jp z,get_
    cp 'o'    
    jp z,go_
    cp 't'    
    jp z,gt_
    dec bc
    jp var_

i:
    inc bc
    ld a,(bc)
    cp 'f'    
    jp z,if_
    cp 'n'    
    jp z,inv_
    dec bc
    jp var_

k:
    jp x
    inc bc
    ld a,(bc)
    cp 'e'    
    jp z,key_
    dec bc
    jp var_

l:
    inc bc
    ld a,(bc)
    cp 'e'    
    jp z,let_
    cp 't'    
    jp z,lt_
    dec bc
    jp var_

m:
    inc bc
    ld a,(bc)
    cp 'a'    
    jp z,map_
    cp 'u'    
    jp z,mul_
    dec bc
    jp var_

n:
    inc bc
    ld a,(bc)
    cp 'e'    
    jp z,neg_
    dec bc
    jp var_

o:
    inc bc
    ld a,(bc)
    cp 'v'    
    jp z,over_
    cp 'r'    
    jp z,or_
    dec bc
    jp var_

p:
    inc bc
    ld a,(bc)
    cp 'r'    
    jp z,print_
    dec bc
    jp var_
r:
    inc bc
    ld a,(bc)
    cp 'o'    
    jp z,rot_
    dec bc
    jp var_

s:
    inc bc
    ld a,(bc)
    cp 'c'    
    jp z,scan_
    cp 'e'    
    jp z,set_
    cp 'h'    
    jp z,shift_
    cp 'u'    
    jp z,sub_
    cp 'w'    
    jp z,swap_
    dec bc
    jp var_

u:
    inc bc
    ld a,(bc)
    cp 'n'    
    jp z,undrop_
    dec bc
    jp var_

w:
    inc bc
    ld a,(bc)
    cp 'h'    
    jp z,while_
    dec bc
    jp var_

x:
    inc bc
    ld a,(bc)
    cp 'x'    
    jp z,xor_
    dec bc
    jp var_

case_:
closure_:
def_:
filter_:
get_:
if_:
let_:
map_:
print_:
scan_:
set_:
shift_:
while_:

    jp next

;*******************************************************************
; Page 5 primitive routines continued
;*******************************************************************

; ********************************************************************
; 16-bit multiply  
mul:        ;=19
    pop  de       ; get first value
    pop  hl
    push bc       ; Preserve the IP
    ld b,h        ; bc = 2nd value
    ld c,l
    
    ld hl,0
    ld a,16
mul2:
    add hl,hl
    rl e
    rl d
    jr nc,$+6
    add hl,bc
    jr nc,$+3
    inc de
    dec a
    jr nz,mul2
		pop bc			  ; Restore the IP
		push hl       ; Put the product on the stack - stack bug fixed 2/12/21
		jp next


;*******************************************************************
; Subroutines
;*******************************************************************

prompt:          
    call printStr
    .cstr "\r\n> "
    ret

putStr0:
    call putchar
    inc hl
putStr:
    ld a,(hl)
    or a
    jr nz,putStr0
    ret

rpush:     
    dec ix    
    ld (ix+0),h
    dec ix
    ld (ix+0),l
    ret

rpop:       
    ld l,(ix+0)    
    inc ix    
    ld h,(ix+0)
    inc ix    
rpop2:
    ret

crlf:       
    call printStr
    .cstr "\r\n"
    ret


;*******************************************************************
;*******************************************************************

num:
		ld hl,$0000				    ; Clear hl to accept the number
		ld a,(bc)				    ; Get numeral or -
    cp '-'
    jr nz,num0
    inc bc        ; no flags are affected
num0:
    ex af,af'    ; save zero flag = 0 for later
num1:
    ld a,(bc)    
    sub 30h       
    jr c, num2    ; not a number, exit loop 
    inc bc  
    ld d,h  
    ld e,l  
    add hl,hl    
    add hl,hl    
    add hl,de    
    add hl,hl    
    add a,l       
    ld l,a  
    jr nc,num1    
    inc h    
    jr num1 
num2:
    dec bc
    ex af,af'    ; restore zero flag
    jr nz, num3
    ex de,hl     ; negate the value of hl
    ld hl,0
    or a    ; jump to sub2
    sbc hl,de    
num3:
    push hl       ; Put the number on the stack
    jp next       ; and process the next character



hexnum:        ;
	    ld hl,0	    		    ; Clear hl to accept the number
hexnum1:
    inc bc
    ld a,(bc)		  ; Get the character which is a numeral
    bit 6,a       ; is it uppercase alpha?
    jr z, hexnum2     ; no a decimal
    sub 7        ; sub 7  to make $a - $F
hexnum2:
    sub $30       ; Form decimal digit
    jp c,num2
    cp $0F+1
    jp nc,num2
    add hl,hl    ; 2X ; Multiply digit(s) in hl by 16
    add hl,hl    ; 4X
    add hl,hl    ; 8X
    add hl,hl    ; 16X     
    add a,l       ; add into bottom of hl
    ld  l,a       ;    
    jr  hexnum1

prtdec:        ;=34 ; removes leading zeros
    ; ld a,h
    ; or l
    ; ld a, '0'
    ; jp z, putchar
    bit 7,h
    jr z,prtdec0
    ld a,'-'
    call putchar
    xor a  
    sub l  
    ld l,a
    sbc a,a  
    sub h  
    ld h,a
prtdec0:        
    push bc
    ld c,0        ; leading zeros flag = false
    ld de,-10000
    call prtdec1
    ld de,-1000
    call prtdec1
    ld de,-100
    call prtdec1
    ld e,-10
    call prtdec1
    inc c        ; flag = true for at least digit
    ld e,-1
    call prtdec1
    pop bc
    ret
prtdec1:	     
    ld b,'0'-1
prtdec2:	    
    inc b
    add hl,de
    jr c,prtdec2
    sbc hl,de
    ld a,'0'
    cp b
    jr nz,prtdec3
    xor a
    or c
    ret z
    jr prtdec4
prtdec3:	    
    inc c
prtdec4:	    
    ld a,b
    jp putchar
                                ; 
prthex:          
                                ; Display hl as a 16-bit number in hex.
    push bc                     ; preserve the IP
    ld a,h
    call prthex2
    ld a,l
    call prthex2
    pop bc
    ret
prthex2:		     
    ld	c,a
	rra 
	rra 
	rra 
	rra 
    call prthex3
    ld a,c
prthex3:		
    and	0x0F
	add	a,0x90
	daa
	adc	a,0x40
	daa
	jp putchar


                                ; 
arg:
    inc     bc                  ; get next char
    ld a,(bc)
    sub     "1"                 ; treat as a digit, 1 based index
    and     $07                 ; mask 
    add     a,a                 ; double
    ld l,a
    ld h,0
    ld e,iyl
    ld d,iyh
    ex de,hl
    or a
    sbc hl,de
    dec hl
    ld d,(hl)
    dec hl
    ld e,(hl)
    push de
    jp next

                                ; 
strDef:     
    ld de,(vHeapPtr)            ; DE = heap ptr
    push de                     ; save start of string 
    inc bc                      ; point to next char
    jr strDef2
strDef1:
    ld (de),a
    inc de                      ; increase count
    inc bc                      ; point to next char
strDef2:
    ld a,(bc)
    cp "'"                      ; ' is the string terminator
    jr nz,strDef1
    xor a                       ; write null to terminate string
    ld (de),a
    inc de
    ld (vHeapPtr),de            ; bump heap ptr to after definiton
    jp next  

newAdd2:
    push bc                     ; push IP
    
    ld d,(iy-1)
    ld e,(iy-2)
    ld h,(iy-3)
    ld l,(iy-4)
    
    push iy                     ; push base pointer
    ld iy,0                     ; base pointer = stack pointer
    add iy,sp
    
    add hl,de                   ; hl = hl + de   
    ex de,hl                    ; de = result

    pop hl                      ; hl = old BP
    pop bc                      ; bc = IP
    ld sp,hl                    ; sp = old BP
    push de                     ; push result    
    
    jp next    

lambda:              
    inc bc
    ld de,(vHeapPtr)            ; start of lambda defintion
    push de
lambda1:                        ; Skip to end of definition    
    ld a,(bc)                   ; Get the next character
    inc bc                      ; Point to next character
    ld (de),a
    inc de
    cp ")"                      ; Is it the end of the definition? 
    jr nz, lambda1              ; get the next element
    dec bc
    ld (vHeapPtr),de            ; bump heap ptr to after definiton
    jp next  

go:				                ; execute Siena lambda at pointer
    pop de                      ; de = pointer to lambda
go1:
    ld a,d                      ; skip if destination address is null
    or e
    jr z,go2

    push bc                     ; save IP 
    push iy                     ; push base pointer
    ld iy,0                     ; base pointer = stack pointer
    add iy,sp

    ld bc,de                    ; IP = pointer to lambda
    dec bc                      ; dec to prepare for next routine
go2:
    jp next       

return:
    pop de                      ; de = result 
    pop hl                      ; hl = old BP
    pop bc                      ; bc = IP
    ld sp,hl                    ; sp = old BP
    push de                     ; push result    
    jp next    
    
    