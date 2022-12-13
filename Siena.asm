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
    DSIZE   EQU $80
    RSIZE   EQU $80
    TIBSIZE EQU $100	        ; 256 bytes , along line!
    TRUE    EQU -1		        ; C-style true
    FALSE   EQU 0
    EMPTY   EQU 0		        ; 
    UNUSED  EQU $ff
    
    DATASIZE    EQU 26*2*2	    ; a..z, a..z words

; **************************************************************************
; Page 0  Initialisation
; **************************************************************************		

.org ROMSTART + $180		    ; 0+180 put Siena code from here	

; **************************************************************************
; this code must not span pages
; **************************************************************************
macros:

; ***********************************************************************
; Initial values for user mintVars		
; ***********************************************************************		
iAltVars:			            ; value copied into tables
    DW 0                        ; a 			
    DW 1                        ; b width in bytes of array operations (default 1 byte) 
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
    DB lsb(block_)              ;    (    
    DB lsb(blockend_)           ;    )
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
    DB lsb(kall_)               ;    A     
    DB lsb(kall_)               ;    B     
    DB lsb(kall_)               ;    C     
    DB lsb(kall_)               ;    D     
    DB lsb(kall_)               ;    E     
    DB lsb(kall_)               ;    F     
    DB lsb(kall_)               ;    G     
    DB lsb(kall_)               ;    h     
    DB lsb(kall_)               ;    I     
    DB lsb(kall_)               ;    J     
    DB lsb(kall_)               ;    K     
    DB lsb(kall_)               ;    L     
    DB lsb(kall_)               ;    M     
    DB lsb(kall_)               ;    N     
    DB lsb(kall_)               ;    O     
    DB lsb(kall_)               ;    p     
    DB lsb(kall_)               ;    Q     
    DB lsb(kall_)               ;    R     
    DB lsb(kall_)               ;    S     
    DB lsb(kall_)               ;    T     
    DB lsb(kall_)               ;    U     
    DB lsb(kall_)               ;    V     
    DB lsb(kall_)               ;    W     
    DB lsb(kall_)               ;    X     
    DB lsb(kall_)               ;    Y     
    DB lsb(kall_)               ;    Z    
    DB lsb(array_)              ;    [
    DB lsb(nop_)                ;    \
    DB lsb(arrayEnd_)           ;    ]
    DB lsb(nop_)                ;    ^
    DB lsb(nop_)                ;    _
    DB lsb(char_)               ;    `    	    
    DB lsb(a_)                  ;    a     
    DB lsb(var_)                ;    b  
    DB lsb(c_)                  ;    c  
    DB lsb(d_)                  ;    d  
    DB lsb(e_)                  ;    e  
    DB lsb(f_)                  ;    f  
    DB lsb(g_)                  ;    g  
    DB lsb(h_)                  ;    h  
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
    DB lsb(lambda_)             ;    {
    DB lsb(or_)                 ;    |  
    DB lsb(lambdaEnd_)             ;    }  
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
    call exec		            ; Siena exec_ operation and jump to it
    .cstr "ca"
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
lambdaEnd_:
    jp lambdaEnd
dot_:  
    pop hl
    call prtdec
dot2:
    ld a,' '       
    call putchar
    jp next
block_:
    jp block
blockend_:
    jp blockend
char_:
    jp char
array_:
    jp array
arrayEnd_:
    jp arrayEnd
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
h_:
    jp h
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
        
kall_:
    ld a,(bc)
    call lookupRef1
    ld e,(hl)
    inc hl
    ld d,(hl)
    jp exec1

hdot_:                          ; print hexadecimal
    pop hl
    call prthex
    jp dot2

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
    jp exit
    
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
    jp next                     ; hardwire white space to always exec_ to next (important for arrays)


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

; exec_:
;     call exec1
;     jp next
; exec1:
;     pop hl
;     ex (sp),hl
;     jp (hl)

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
    
b:
    inc bc
    ld a,(bc)
    cp 'y'    
    jp z,bytes
    dec bc
    jp var_

c:    
    inc bc
    ld a,(bc)
    cp 'a'    
    jp z, case
    dec bc
    jp var_
    
d:    
    inc bc
    ld a,(bc)
    cp 'e'    
    jp z,def
    cp 'i'    
    jp z,div_
    dec bc
    jp var_

e:
    inc bc
    ld a,(bc)
    cp 'x'    
    jp z, exec
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
    cp 't'    
    jp z,gt_
    dec bc
    jp var_

h:
    inc bc
    ld a,(bc)
    dec bc
    jp var_

i:
    inc bc
    ld a,(bc)
    cp 'n'    
    jp z,in
    cp 'v'
    jp z,inv_
    cp 'f'    
    jp nz,i1
    inc bc
    ld a,(bc)
    cp 'e'    
    jp z,ife
    dec bc
    jp if
i1:
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
    cp 'o'    
    jp z,lookup
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
    ; jp nz,s1
    ; inc bc
    ; ld a,(bc)
    ; cp 'l'    
    ; jp z,select
    ; cp 't'    
    jp z,set_
    ; dec bc
; s1:
    cp 'h'    
    jp z,shift_
    cp 'u'    
    jp z,sub_
    cp 'w'    
    jp z,switch
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
    cp 'o'    
    jp z,words
    dec bc
    jp var_

x:
    inc bc
    ld a,(bc)
    cp 'x'    
    jp z,xor_
    dec bc
    jp var_

closure_:
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

    ld a,UNUSED
    ld b,0
    ld hl, hashSlots
init1:
    ld (hl),a
    djnz init1 
    ld a,0
    ld (hashWordsPtr),a
    ret
    
num:
	ld hl,$0000				    ; Clear hl to accept the number
	ld a,(bc)				    ; Get numeral or -
    cp '-'
    jr nz,num0
    inc bc                      ; move to next char, no flags affected
num0:
    ex af,af'                   ; save zero flag = 0 for later
num1:
    ld a,(bc)                   ; read digit    
    sub "0"                     ; less than 0?
    jr c, num2                  ; not a digit, exit loop 
    cp 10                       ; greater that 9?
    jr nc, num2                 ; not a digit, exit loop
    inc bc                      ; inc IP
    ld de,hl                    ; multiply hl * 10
    add hl,hl    
    add hl,hl    
    add hl,de    
    add hl,hl    
    add a,l                     ; add digit in a to hl
    ld l,a
    ld a,0
    adc a,h
    ld h,a
    jr num1 
num2:
    dec bc
    ex af,af'                   ; restore zero flag
    jr nz, num3
    ex de,hl                    ; negate the value of hl
    ld hl,0
    or a                        ; jump to sub2
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

char:
    ld hl,0                     ; if `` is empty
char1:
    inc bc                      ; point to next char
    ld a,(bc)
    cp "`"                      ; ` is the string terminator
    jr z,char3
    cp $5c                      ; \ is the escape
    jr nz,char2
    inc bc
    ld a,(bc)
char2:
    ld l,a
    jr char1
char3:
    push hl
    ; dec bc
    jp next  

exec:				            ; execute lambda at pointer
    pop hl                      ; hl = pointer to lambda
exec1:
    ld a,h                      ; skip if destination address is null
    or l
    jr z,exec3
exec2:
    push bc                     ; push IP 
    push iy                     ; push SCP (scope pointer)
    push iy                     ; push BP
    ld iy,0                     ; BP = SP
    add iy,sp

    ld bc,hl                    ; IP = pointer to lambda
    dec bc                      ; dec to prepare for next routine
exec3:
    jp next       

lambda:              
    inc bc
    ld hl,(vHeapPtr)            ; start of lambda defintion
    push hl
    ld d,1                      ; nesting: count first parenthesis
lambda1:                        ; Skip to end of definition    
    ld a,(bc)                   ; Get the next character
    inc bc                      ; Point to next character
    ld (hl),a
    inc hl
    cp "'"
    jr z,lambda2
    cp "("
    jr z,lambda2
    cp ")"
    jr z,lambda2
    cp "{"
    jr z,lambda2
    cp "}"                      ; Is it the end of the definition? 
    jr z,lambda2
    cp "["
    jr z,lambda2
    cp "]"
    jr z,lambda2
    cp "`"
    jr nz,lambda1
lambda2:
    inc d
    bit 0,d                     ; balanced?
    jr nz, lambda1              ; not balanced, get the next element
    cp "}"                      ; Is it the end of the definition? 
    jr nz, lambda1              ; get the next element
    dec bc
    ld (vHeapPtr),hl            ; bump heap ptr to after definiton
    jp next  

lambdaEnd:
    pop hl                      ; hl = last result 
    ld d,iyh                    ; de = BP
    ld e,iyl
    ex de,hl                    ; hl = BP, de = result
    ld sp,hl                    ; sp = BP
    pop hl                      ; hl = old BP
    pop bc                      ; pop scope ptr (discard)
    pop bc                      ; bc = IP
    ld sp,hl                    ; sp = old BP
    ld iy,0                     ; iy = sp = old BP
    add iy,sp
    push de                     ; push result    
    jp next    

block:
    inc bc
    push bc                     ; return first opcode of block    
    ld d,1                      ; nesting: count first parenthesis
block1:                         ; Skip to end of definition    
    ld a,(bc)                   ; Get the next character
    inc bc                      ; Point to next character
    cp "'"
    jr z,block2
    cp "("
    jr z,block2
    cp ")"
    jr z,block2
    cp "{"
    jr z,block2
    cp "}"                       
    jr z,block2
    cp "["
    jr z,block2
    cp "]"
    jr z,block2
    cp "`"
    jr nz,block1
block2:
    inc d
    bit 0,d                     ; balanced?
    jr nz, block1               ; not balanced, get the next element
    cp ")"                      ; Is it the end of the block? 
    jr nz, block1               ; get the next element
    dec bc
    jp next  

blockend:
    pop hl                      ; hl = last result 
    ld d,iyh                    ; de = BP
    ld e,iyl
    ex de,hl                    ; hl = BP, de = result
    ld sp,hl                    ; sp = BP
    pop hl                      ; hl = old BP
    pop bc                      ; pop SCP (discard)
    pop bc                      ; bc = IP
    ld sp,hl                    ; sp = old BP
    ld iy,0                     ; iy = sp
    add iy,sp
    push de                     ; push result    
    jp next    

arg:
    inc bc                      ; get next char
    ld a,(bc)
    sub "1"                     ; treat as a digit, 1 based index
    and $07                     ; mask 
    add a,a                     ; double
    ld l,a                      ; hl = offset into args
    ld h,0
    ld e,(iy+2)                 ; de = SCP (scope ptr)
    ld d,(iy+3)
    ex de,hl                    ; hl = SCP - offset
    or a
    sbc hl,de
    dec hl                      ; de = arg 
    ld d,(hl)                   
    dec hl
    ld e,(hl)
    push de                     ; push arg
    jp next
                                ; 
exit:
    ld de,bc                    ; address of code after exit opcode
    inc de			            
    exx
    pop bc                      ; bc = last result 
    ld d,iyh                    ; de = BP
    ld e,iyl
    ex de,hl                    ; hl = BP, de = result
    ld sp,hl                    ; sp = BP
    exx
    pop hl                      ; hl = old BP
    pop bc                      ; pop SCP (discard)
    pop bc                      ; bc = IP
    ld sp,hl                    ; sp = old BP
    exx
    push bc                     ; push result    
    exx
    ex de,hl
    jp (hl)

in:
    pop hl                      ; hl = string    
    pop de                      ; de = char
in1:
    ld a,(hl)
    inc hl
    cp 0                        ; is end of string
    jr z,in2
    cp e
    jr nz,in1
    or a                        ; a is never 0, or a resets zero flag 
in2:
    ld hl,0                     ; hl = result
    jr z,in3
    dec hl                      ; if nz de = $ffff
in3:
    push hl                     ; push result    
    jp next    
    
newAdd2:
    push bc                     ; push IP
    ld e,(iy+2)                 ; get SCP from parent stack frame
    ld d,(iy+3)                 ; make this the old BP for this stack frame
    push de                     ; push SCP
    push iy                     ; push base pointer
    ld iy,(3+2)*2               ; base pointer = stack pointer - (stack frame vars) - 2 args
    add iy,sp                   ;
    
    ld d,(iy-1)
    ld e,(iy-2)
    ld h,(iy-3)
    ld l,(iy-4)

    add hl,de                   ; hl = hl + de   
    ex de,hl                    ; de = result

    pop hl                      ; hl = old BP
    pop bc                      ; pop SCP (discard)
    pop bc                      ; bc = IP
    ld sp,hl                    ; sp = old BP
    ld iy,0
    add iy,sp
    
    push de                     ; push result    
    jp next    

    
if:
    ld de,0                      ; null pointer for else
    jr ife1
ife: 
    pop de                      ; de = else
ife1:
    pop hl                      ; hl = then
    ex (sp),hl                  ; hl = condition, (sp) = then
    inc hl                      ; check for true
    ld a,h
    or l
    pop hl                      ; hl = then
    jr z,ife2                   
    ex de,hl                    ; condition = false, hl = else  
ife2:                           
    ld a,h                      ; check if hl is null
    or l
    jp z,next
    push bc                     ; push IP
    ld e,(iy+2)                 ; get SCP from parent stack frame
    ld d,(iy+3)                 ; make this the old BP for this stack frame
    push de                     ; push SCP
    push iy                     ; push BP  
    ld iy,0                     ; iy = sp
    add iy,sp
    ld bc,hl                    ; IP = then
    dec bc
    jp next    

switch:
    pop hl                      ; get condition from stack
    push bc                     ; create stack frame, push IP (replace later)
    ld e,(iy+2)                 ; get SCP from parent stack frame
    ld d,(iy+3)                 ; make this the old BP for this stack frame
    push de                     ; push SCP
    push iy                     ; push BP  
    ld iy,0                     ; BP = SP
    add iy,sp
    push hl                     ; push condition as first arg of new frame
    jp next
    
case:
    ld h,(iy-1)                 ; hl = selector
    ld l,(iy-2)
    inc hl                      ; hl -= 1 index from second arg    
    add hl,hl                   ; hl *= 2 word offset
    ld d,iyh                    ; hl = BP, de = offset
    ld e,iyl
    ex de,hl
    or a                        ; hl = BP - offset
    sbc hl,de
    ld de,hl                    ; save arg ptr
    dec hl                      ; hl += 2
    dec hl
    or a                        ; arg ptr - stack pointer
    sbc hl,sp
    jr nc,case0
    pop de                      ; pop last arg
    jr case1
case0:
    ex de,hl
    dec hl                      ; de = arg 
    ld d,(hl)                   
    dec hl
    ld e,(hl)
case1:
    ld a,d                      ; is arg == null ? then skip
    or e
    jr z,case2
    ld (iy+4),c                 ; update return address in stack frame
    ld (iy+5),b                  
    ld bc,de                    ; IP = arg
    dec bc
case2:
    jp next    
    
words:
    ld hl,2
    jp bytes
    
bytes:
    ld hl,1
bytes1:
    ld (vDataWidth),hl
    jp next
    
array:
    push bc                     ; create stack frame, push IP
    ld e,(iy+2)                 ; get SCP from parent stack frame
    ld d,(iy+3)                 ; make this the old BP for this stack frame
    push de                     ; push SCP
    push iy                     ; push BP  
    ld iy,0                     ; BP = SP
    add iy,sp
    jp next

arrayEnd:
    ld d,iyh                    ; de = BP
    ld e,iyl
    ld hl,de                    ; hl = de
    or a 
    sbc hl,sp                   ; hl = array count (items on stack)
    push bc                     ; bc' = IP
    exx
    pop bc                       
    exx
    ld bc,hl                    ; bc = count
    ld hl,(vHeapPtr)            ; hl = heap ptr
    ld (hl),c                   ; write count before array data
    inc hl
    ld (hl),b
    inc hl
    push hl                     ; hl = ptr to array (index 0)
    exx
    pop hl                      ; hl' = ptr to array (index 0)
    exx
    ld a,(vDataWidth)
    cp 1                        ; byte?
    jr nz, arrayEnd2
    ex de,hl

arrayEnd1:
    dec de
    dec de
    ld a,(de)
    ld (hl),a
    inc hl
    dec bc
    ld a,c
    or b
    jr nz,arrayEnd1
    jr arrayEnd4

arrayEnd2:
    dec de
    ld a,(de)
    ex af,af'
    dec de
    ld a,(de)
    ld (hl),a
    inc hl
    ex af,af'
    ld a,(de)
    ld (hl),a
    inc hl
    dec bc
    ld a,c
    or b
    jr nz,arrayEnd2
    
arrayEnd4:
    ld d,iyh                    ; de = BP
    ld e,iyl
    ex de,hl                    ; hl = BP, de = result
    ld sp,hl                    ; sp = BP
    pop hl                      ; hl = old BP
    pop bc                      ; pop SCP (discard)
    pop bc                      ; bc = IP
    ld sp,hl                    ; sp = old BP
    ld iy,0                     ; iy = sp
    add iy,sp
    exx
    push hl
    push bc
    exx
    pop bc
    jp next

; updateEntry:
;     ld bc, 
;     pop hl                          ; pointer to args
;     ld e,(hl) 
;     ret

.align $100
pearsonTable:
    db 46, 7, 26, 16, 20, 32, 38, 42, 40, 55, 56, 4, 0, 1, 5, 41, 27, 51, 2, 48, 59 
    db 3, 9, 49, 19, 39, 29, 45, 13, 10, 33, 8, 34, 63, 12, 52, 18, 23, 24, 14, 61 
    db 31, 50, 30, 36, 15, 44, 60, 47, 25, 28, 43, 21, 53, 6, 37, 54, 17, 62, 35, 57 
    db 22, 11, 58

; hash C-string in HL, result in HL 
hashStr:
    ld bc,0                             
hashStr1:    
    ld a,(hl)                           ; load next char
    inc hl
    or a                                 
    jr z,hashStr5                       ; if null exit
    cp "a"                              ; compress into the rang 0-63
    jr c,hashStr2
    sub ("a" - 36)
    jr hashStr4
hashStr2:
    cp "A"
    jr c, hashStr3
    sub ("A" - 10)
    jr hashStr4
hashStr3:    
    sub "0"
hashStr4:
    and $3f                             ; modulus 64
    ld d,msb(pearsonTable)              ; hl = offset into pearson table
    ld e,a                              ; copy A to A' 
    ex af,af'
    ld a,e                               
    
    xor b                                
    ld e,a                              ; look up A in pearson table
    ld a,(de)                           ; b = result
    ld b,a

    ex af,af'                           ; restore a
    inc a                               ; add 1, modulus 64
    and $3f                             
    xor c
    ld e,a                              ; look up A in pearson table
    ld a,(de)                           ; c = result
    ld c,a

    jr hashStr1
hashStr5:
    ld hl,bc                            ; hl = hash
    ret

; add entry to hash slots and hash pointers
; bc = hash, de = addr
; sets carry if successful
addEntry: 
    ld l,b                              ; b = hi, c = lo, a = hi
    sla l                               ; l = lo * 4, maps 0-63 to 0-127 * words
    sla l                               ; 
    ld h,msb(hashSlots)                 ; hl = slots[lo*4]
addEntry0:
    ld a,(hl)                           ; a = (hl), slot
    cp UNUSED                           ; is it unused?
    jr z,addEntry2
    cp c                                ; no, compare a with lo
    jr nz,addEntry1                    ; same lo, check hi 
    inc l                               
    ld a,(hl)
    cp b                                ; compare with hi
    jr z,addEntry3                      ; same hi, collision, exit
    dec l
addEntry1:
    inc l                               ; skip to next slot, modulus 256
    inc l
    jr addEntry0
addEntry2:                              ; new entry
    ld (hl),c                           ; (slot + 0) = hash lo
    inc hl
    ld (hl),b                           ; (slot + 1) = hash hi
    dec hl
    ld h,msb(hashWords)                 ; hl = slots[lo*4]
    ld (hl),e                           ; (slot + 2) = address
    inc hl
    ld (hl),d
    scf
    ret
addEntry3:
    ccf
    ret

; str addr -- bool
def:
    pop de                              ; de = address
    pop hl                              ; hl = str pointer
    push bc
    push de
    call hashStr                        ; hl = hash
    ld bc,hl                            ; bc = hash
    pop de                              ; de = addr
    call addEntry
    ld hl,0                             ; if c return TRUE
    jr nc,def1
    dec hl
def1:
    pop bc
    push hl
    jp next
    
; str -- addr
lookup:
    jp next
    