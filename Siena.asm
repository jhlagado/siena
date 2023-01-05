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

                                
DSIZE       EQU     $80
TIBSIZE     EQU     $100	        ; 256 bytes , along line!
TRUE        EQU     -1		        ; C-style true
FALSE       EQU     0
EMPTY       EQU     0		         
UNUSED      EQU     $ff
NUL         EQU     0
ETX         EQU     3
ESC         EQU     27

; **************************************************************************
; Page 0  Initialisation
; **************************************************************************		

.org ROMSTART + $180		    ; 0+180 put Siena code from here	

; **************************************************************************
; this code must not span pages
; **************************************************************************
macros:

; ***********************************************************************
; Initial values for system vars		
; ***********************************************************************		
isysVars:			            
    DW 0                        ; a vFrac fractional part of calculation			
    DW 2                        ; b vDataWidth in bytes of array operations (default 1 byte) 
    DW 0                        ; c vTIBPtr an offset to the tib
    DW 0                        ; d 
    DW 0                        ; e vLastDef
    DW 0                        ; f 
    DW 0                        ; g 
    DW HEAP                     ; h vHeapPtr \h start of the free mem

    .align $100

opcodesBase:




ctrlCodes:
    DB lsb(EMPTY)               ; ^@  0 NUL  
    DB lsb(EMPTY)               ; ^A  1 SOH
    DB lsb(EMPTY)               ; ^B  2 STX
    DB lsb(EMPTY)               ; ^C  3 ETX
    DB lsb(EMPTY)               ; ^D  4 EOT
    DB lsb(EMPTY)               ; ^E  5 ENQ
    DB lsb(EMPTY)               ; ^F  6 ACK
    DB lsb(EMPTY)               ; ^G  7 BEL
    DB lsb(EMPTY)               ; ^H  8 BS
    DB lsb(EMPTY)               ; ^I  9 TAB
    DB lsb(EMPTY)               ; ^J 10 LF
    DB lsb(EMPTY)               ; ^K 11 VT
    DB lsb(EMPTY)               ; ^L 12 FF
    DB lsb(EMPTY)               ; ^M 13 CR
    DB lsb(EMPTY)               ; ^N 14 SO
    DB lsb(EMPTY)               ; ^O 15 SI
    DB lsb(EMPTY)               ; ^P 16 DLE
    DB lsb(EMPTY)               ; ^Q 17 DC1    
    DB lsb(EMPTY)               ; ^R 18 DC2   
    DB lsb(EMPTY)               ; ^S 19 DC3  
    DB lsb(EMPTY)               ; ^T 20 DC4  
    DB lsb(EMPTY)               ; ^U 21 NAK     
    DB lsb(EMPTY)               ; ^V 22 SYN
    DB lsb(EMPTY)               ; ^W 23 ETB  
    DB lsb(EMPTY)               ; ^X 24 CAN   
    DB lsb(EMPTY)               ; ^Y 25 EM  
    DB lsb(EMPTY)               ; ^Z 26 SUB  
    DB lsb(EMPTY)               ; ^[ 27 ESC
    DB lsb(EMPTY)               ; ^\ 28 FS
    DB lsb(EMPTY)               ; ^] 29 GS
    DB lsb(EMPTY)               ; ^^ 30 RS
    DB lsb(EMPTY)               ; ^_ 31 US

opcodes:                        ; still available ! " % , @   
    DB lsb(nop_)                ; SP  
    DB lsb(not_)                ; !  
    DB lsb(nop_)                ; "
    DB lsb(hexnum_)             ; #
    DB lsb(arg_)                ; $  
    DB lsb(nop_)                ; %  
    DB lsb(and_)                ; &
    DB lsb(string_)             ; '
    DB lsb(paren_)              ; (    
    DB lsb(parenEnd_)           ; )
    DB lsb(mul_)                ; *  
    DB lsb(add_)                ; +
    DB lsb(nop_)                ; ,  
    DB lsb(sub_)                ; -
    DB lsb(dot_)                ; .
    DB lsb(div_)                ; /	
    DB lsb(num_)                ; 0     
    DB lsb(num_)                ; 1    
    DB lsb(num_)                ; 2    
    DB lsb(num_)                ; 3    
    DB lsb(num_)                ; 4    
    DB lsb(num_)                ; 5    
    DB lsb(num_)                ; 6    
    DB lsb(num_)                ; 7    
    DB lsb(num_)                ; 8    
    DB lsb(num_)                ; 9    
    DB lsb(symbol_)             ; :    
    DB lsb(clear_)              ; ;
    DB lsb(lt_)                 ; <
    DB lsb(eq_)                 ; =  
    DB lsb(gt_)                 ; >  
    DB lsb(index_)              ; ?    
    DB lsb(prop_)               ; @  
    DB lsb(ident_)              ; A     
    DB lsb(ident_)              ; B     
    DB lsb(ident_)              ; C     
    DB lsb(ident_)              ; D     
    DB lsb(ident_)              ; E     
    DB lsb(ident_)              ; F     
    DB lsb(ident_)              ; G     
    DB lsb(ident_)              ; h     
    DB lsb(ident_)              ; I     
    DB lsb(ident_)              ; J     
    DB lsb(ident_)              ; K     
    DB lsb(ident_)              ; L     
    DB lsb(ident_)              ; M     
    DB lsb(ident_)              ; N     
    DB lsb(ident_)              ; O     
    DB lsb(ident_)              ; p     
    DB lsb(ident_)              ; Q     
    DB lsb(ident_)              ; R     
    DB lsb(ident_)              ; S     
    DB lsb(ident_)              ; T     
    DB lsb(ident_)              ; U     
    DB lsb(ident_)              ; V     
    DB lsb(ident_)              ; W     
    DB lsb(ident_)              ; X     
    DB lsb(ident_)              ; Y     
    DB lsb(ident_)              ; Z    
    DB lsb(array_)              ; [
    DB lsb(comment_)            ; \
    DB lsb(arrayEnd_)           ; ]
    DB lsb(xor_)                ; ^
    DB lsb(ident_)              ; _
    DB lsb(char_)               ; `    	    
    DB lsb(ident_)              ; a     
    DB lsb(ident_)              ; b  
    DB lsb(ident_)              ; c  
    DB lsb(ident_)              ; d  
    DB lsb(ident_)              ; e  
    DB lsb(ident_)              ; f  
    DB lsb(ident_)              ; g  
    DB lsb(ident_)              ; h  
    DB lsb(ident_)              ; i  
    DB lsb(ident_)              ; j  
    DB lsb(ident_)              ; k  
    DB lsb(ident_)              ; l  
    DB lsb(ident_)              ; m  
    DB lsb(ident_)              ; n  
    DB lsb(ident_)              ; o  
    DB lsb(ident_)              ; p  
    DB lsb(ident_)              ; q  
    DB lsb(ident_)              ; r  
    DB lsb(ident_)              ; s  
    DB lsb(ident_)              ; t  
    DB lsb(ident_)              ; u  
    DB lsb(ident_)              ; v  
    DB lsb(ident_)              ; w  
    DB lsb(ident_)              ; x  
    DB lsb(ident_)              ; y  
    DB lsb(ident_)              ; z  
    DB lsb(block_)              ; {
    DB lsb(or_)                 ; |  
    DB lsb(blockEnd_)           ; }  
    DB lsb(inv_)                ; ~    
    DB lsb(nop_)                ; DEL	


; **********************************************************************			 
; symbolic operators 
; **********************************************************************
    .align $100
page4:

num_:    
    jp  num

hexnum_:    
    jp hexnum

arg_:
    jp arg

prop_:
    jp prop

string_:
    jp string

clear_:    
    jp clear

paren_:    
    jp paren

parenEnd_:
    jp parenEnd

dot_:  
    pop hl
    inc bc
    ld a,(bc)
    cp "h"
    jr nz,dot1
    call prthex
    jr dot4
dot1:
    cp "s"
    jr nz,dot2
    call prtstr
    jr dot4
dot2:
    cp "c"
    jr nz,dot3
    ld a,l
    call putchar
    jr dot4
dot3:
    dec bc
    call prtdec
dot4:
    ld a,' '       
    call putchar
    jp (ix)

; addr index -- addr2
index_:                         
    pop hl                              ; hl = index  
    pop de                              ; de = addr
    ld a,(vDataWidth)
    dec a
    jr z,index1
    add hl,hl                           ; if data width = 2 then double 
index1:
    add hl,de                           ; add addr
    push hl
    jp (ix)       

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
ident_:
    jp ident
symbol_:
    jp symbol
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
    jp (ix)        
    
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
    jp (ix)    
        
hdot_:                          ; print hexadecimal
    pop hl
    call prthex
    jp dot3

mul_:    
    jp mul 
    
sub_:  		                    ; Subtract the value 2nd on stack from top of stack 
    pop de    
    pop hl                      ; Entry point for INVert
sub2:    
    or a                        ; Entry point for NEGate
    sbc hl,de       
    push hl        
    jp (ix)        
        
not_:				            ; logical invert, any non zero value 
    ld hl,0                     ; is considered true
    jr eq1    

eq_:    
    pop hl
eq1:
    pop de
    jp equals

gt_:
    pop de
    pop hl
    jr lt1

lt_:
    pop hl
    pop de
lt1:
    inc bc
    ld a,(bc)
    cp "="
    jp z,lessthaneq
    dec bc
    jp lessthan
    
div_:    
    pop  de                     ; get first value
    pop  hl                     ; get 2nd value
    push bc                     ; preserve the IP    
    ld bc,hl                
    call divide
    ld (vFrac),hl
    pop bc
    push de                     ; push result
    jp (ix)

comment_:
    inc bc                      ; point to next char
    ld a,(bc)
    cp " "                      ; terminate on any char less than SP 
    jr nc,comment_
    dec bc
    jp (ix) 

nop_:  
    jp (ix)


;*******************************************************************
; word operators
;*******************************************************************

; shl  
; value count -- value2          shift left count places
shl:
    ld de,bc                    ; save IP    
    pop bc                      ; bc = count
    ld b,c                      ; b = loop counter
    pop hl                      
    inc b                       ; test for counter=0 case
    jr shl2
shl1:   
    add hl,hl                   ; left shift hl
shl2:   
    djnz shl1
    push hl
    ld bc,de                    ; restore IP
    jp (ix)

; shr  
; value count -- value2          shift left count places
shr:
    ld de,bc                    ; save IP    
    pop bc                      ; bc = count
    ld b,c                      ; b = loop counter
    pop hl                      
    inc b                       ; test for counter=0 case
    jr shr2
shr1:   
    srl h                       ; right shift hl
    rr l
shr2:   
    djnz shr1
    push hl
    ld bc,de                    ; restore IP
    jp (ix)

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
	jp (ix)

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
    jp (ix)       ; and process the next character

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
                                ; 
string:     
    ld de,(vHeapPtr)            ; DE = heap ptr
    push de                     ; save start of string 
    inc bc                      ; point to next char
    jr string2
string1:
    ld (de),a
    inc de                      ; increase count
    inc bc                      ; point to next char
string2:
    ld a,(bc)
    cp "'"                      ; ' is the string terminator
    jr nz,string1
    xor a                       ; write NUL to terminate string
    ld (de),a
    inc de
    ld (vHeapPtr),de            ; bump heap ptr to after definiton
    jp (ix)  

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
    jp (ix)  

paren:
    ld ix,paren2
    jr block
paren2:
    ld ix,next
    jp exec    

parenEnd:
    pop hl                      ; hl = last result 
    pop de
    pop bc
    pop bc
    push hl
    ld iyh,d
    ld iyl,e
    ld ix,next
    jp (ix)

block:
    inc bc
    push bc                     ; return first opcode of block    
    ld d,1                      ; nesting: count first parenthesis
block1:                         ; Skip to end of definition    
    ld a,(bc)                   ; Get the next character
    inc bc                      ; Point to next character
    cp " " + 1                  ; ignore whitespace 
    jr c,block1

    cp ")"
    jr z,block4
    cp "}"                       
    jr z,block4
    cp "]"
    jr z,block4

    cp "("
    jr z,block3
    cp "{"
    jr z,block3
    cp "["
    jr z,block3

    cp "'"
    jr z,block3
    cp "`"
    jr nz,block1
block2:
    inc d
    jr block1                   
block3:
    ld a,$80
    xor d
    ld b,a
    jr block1                   
block4:
    dec d
    jr nz, block1                 ; get the next element

    dec bc                      ; balanced, exit
    jp (ix)  

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
    jp (ix)    

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
    jp (ix)

prop:
    inc bc                      ; get next char
    ld a,(bc)
    sub "0"                     ; treat as a digit, 1 based index
    and $07                     ; mask 
    add a,a                     ; double
    ld l,a                      ; hl = offset into args
    ld h,0
    ld e,(iy+6)                 ; de = closure array
    ld d,(iy+7)
    add hl,de                   ; find address of prop in array
    ld e,(hl)                   
    inc hl
    ld d,(hl)
    push de                     ; push prop
    jp (ix)

; addr -- value
get:                         
    pop hl    
    ld d,0
    ld e,(hl)    
    ld a,(vDataWidth)
    dec a
    jr z,get1
    inc hl    
    ld d,(hl)    
get1:
    push de    
    jp (ix)       

; addr value -- value0
set:                         
    pop hl     
    pop de     
    ld a,(hl)
    ld (hl),e
    ld e,a
    ld a,(vDataWidth)
    dec a
    jr z,set1
    inc hl    
    ld a,(hl)
    ld (hl),d
    ld d,a
set1:	  
    push de                     ; return old value
    jp (ix)  
                                ; 
; in:
;  pop hl                      ; hl = string    
;  pop de                      ; de = char
; in1:
;  ld a,(hl)
;  inc hl
;  cp 0                        ; is end of string
;  jr z,in2
;  cp e
;  jr nz,in1
;  or a                        ; a is never 0, or a resets zero flag 
; in2:
;  ld hl,0                     ; hl = result
;  jr z,in3
;  dec hl                      ; if nz de = $ffff
; in3:
;  push hl                     ; push result    
;  jp (ix)    
    
; newAdd2:
;  push bc                     ; push IP
;  ld e,(iy+2)                 ; get SCP from parent stack frame
;  ld d,(iy+3)                 ; make this the old BP for this stack frame
;  push de                     ; push SCP
;  push iy                     ; push base pointer
;  ld iy,(3+2)*2               ; base pointer = stack pointer - (stack frame vars) - 2 args
;  add iy,sp                   ;
    
;  ld d,(iy-1)
;  ld e,(iy-2)
;  ld h,(iy-3)
;  ld l,(iy-4)

;  add hl,de                   ; hl = hl + de   
;  ex de,hl                    ; de = result

;  pop hl                      ; hl = old BP
;  pop bc                      ; pop SCP (discard)
;  pop bc                      ; bc = IP
;  ld sp,hl                    ; sp = old BP
;  ld iy,0
;  add iy,sp
    
;  push de                     ; push result    
;  jp (ix)    

    
; ifte
; condition then -- value
if:
    ld de,0                      ; NUL pointer for else
    jr ifte1

; ifte
; condition then else -- value
ifte: 
    pop de                      ; de = else
ifte1:
    pop hl                      ; hl = then
    ex (sp),hl                  ; hl = condition, (sp) = then
    inc hl                      ; check for true
    ld a,h
    or l
    pop hl                      ; hl = then
    jr z,ifte2                   
    ex de,hl                    ; condition = false, hl = else  
ifte2:                           
    ld a,h                      ; check if hl is NUL
    or l
    jr z,ifte3
    push bc                     ; push IP
    ld e,(iy+2)                 ; get SCP from parent stack frame
    ld d,(iy+3)                 ; make this the old BP for this stack frame
    push de                     ; push SCP
    push iy                     ; push BP  
    ld iy,0                     ; iy = sp
    add iy,sp
    ld bc,hl                    ; IP = then
    dec bc
ifte3:
    jp (ix)    

; switch
; index array -- value
switch: 
    pop de                      ; de = array
    pop hl                      ; hl = index  
    add hl,hl                   ; indec *= 2 
    add hl,de                   ; add array[0]
    ld c,(hl)                   ; bc = case    
    inc hl    
    ld b,(hl)    
    dec bc
    jp (ix)    

; c b --
; loops until c = 0
loop:                           
    pop de                      ; de = block                    c
    pop hl                      ; hl = condition    
    push de
    push bc                     ; push IP
    ld bc,de                    ; bc = block
    ld e,(iy+2)                 ; get SCP from parent stack frame
    ld d,(iy+3)                 ; make this the old BP for this stack frame
    push de                     ; push SCP
    push iy                     ; push BP  
    ld iy,0                     ; iy = sp
    add iy,sp
loop1:    
    ld a,l                      ; bc = block, hl = condition = zero?
    or h                        
    jr z,loop3
    ld de,loop2-1               ; IP return address
    push de
    ld e,(iy+2)                 ; push parent SCP
    ld d,(iy+3)                  
    push de                     ; 
    push iy                     ; push BP  
    ld iy,0                     ; iy = sp
    add iy,sp
    push hl                     ; push condition
    dec bc
    jp (ix)                     

loop2:
    db ESC                      ; escape from interpreter
    ld c,(iy+6)                 ; bc = block
    ld b,(iy+7)                  
    pop hl                      ; hl = condition
    jr loop1
    
loop3:
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
    ld ix,next                  ; needed?
    jp (ix)

case:
    pop hl                      ; get selector from stack
    push bc                     ; create stack frame, push IP (replace later)
    ld e,(iy+2)                 ; get SCP from parent stack frame
    ld d,(iy+3)                 ; make this the old BP for this stack frame
    push de                     ; push SCP
    push iy                     ; push BP  
    ld iy,0                     ; BP = SP
    add iy,sp
    push hl                     ; push selector as first arg of new frame
    jp (ix)
    
select:
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
    ld a,d                      ; is arg == NUL ? then skip
    or e
    jr z,case2
    ld (iy+4),c                 ; update return address in stack frame
    ld (iy+5),b                  
    ld bc,de                    ; IP = arg
    dec bc
case2:
    jp (ix)    
    
words:
    ld hl,2
    jr bytes1
bytes:
    ld hl,1
bytes1:
    ld (vDataWidth),hl
    jp (ix)
    
array:
    push bc                     ; create stack frame, push IP
    ld e,(iy+2)                 ; get SCP from parent stack frame
    ld d,(iy+3)                 ; make this the old BP for this stack frame
    push de                     ; push SCP
    push iy                     ; push BP  
    ld iy,0                     ; BP = SP
    add iy,sp
    jp (ix)

arrayEnd:
    ld d,iyh                    ; de = BP
    ld e,iyl
    ld ixh,d                    ; ix = BP
    ld ixl,e

    ld hl,de                    ; hl = de
    or a 
    sbc hl,sp                   ; hl = array count (items on stack)
    srl h
    rr l                        
    
    ex de,hl                    ; de = count
    ld hl,(vHeapPtr)            ; hl = array[-2]
    ld (hl),e
    inc hl
    ld (hl),d
    inc hl                      ; hl = array[0], de = count

    ld a,(vDataWidth)           ; vDataWidth=1? 
    cp 1                        
    jr nz, arrayEnd2

arrayEnd1:                      ; byte
    ld a,(ix-2)
    ld (hl),a

    inc hl
    dec ix
    dec ix

    dec de
    ld a,e
    or d
    jr nz,arrayEnd1
    jr arrayEnd3

arrayEnd2:                      ; word
    ld a,(ix-2)
    ld (hl),a
    inc hl
    ld a,(ix-1)
    ld (hl),a
    inc hl

    dec ix
    dec ix

    dec de
    ld a,e
    or d
    jr nz,arrayEnd2
    
arrayEnd3:

    ld d,iyh                    ; de = BP, hl = end of array
    ld e,iyl
    ex de,hl                    ; hl = BP, de = end of array
    ld sp,hl                    ; sp = BP

    pop hl                      ; hl = old BP, de = end of array
    pop ix                      ; pop SCP (discard)
    pop ix                      ; pop IP (discard)
    
    ex de,hl
    ld iyh,d
    ld iyl,e
    ex de,hl
    
    ; ld sp,hl                    ; sp = old BP
    ; ld iy,0                     ; iy = sp
    ; add iy,sp
    ld ix,next
    
    ld hl,(vHeapPtr)            ; hl = array[0], de = end of array
    inc hl
    inc hl
    push hl                     ; return array[0]
    ex de,hl                    ; hl = end of array, de = array[0] 

    or a
    sbc hl,de                   ; hl = size = end of array - array[0] 
    ex de,hl
    ld hl,(vHeapPtr)            ; hl = array[-2]
    ld (hl),e                   ; array[-2] = size
    inc hl
    ld (hl),d
    inc hl
    add hl,de
    ld (vHeapPtr),hl
    
    jp (ix)

; str -- num
hash:
    pop hl
    push bc
    ld bc,hl
    call hashStr
    pop bc
    push hl
    jp (ix)

; symbol addr -- 
def:
    ld hl,bc                            ; de = addr (sp) = IP (sp+2) = symbol
    ex (sp),hl                          
    ex de,hl                            
    ld hl,(vHeapPtr)                    ; hl = heap de = addr
    ld (hl),$cd                         ; compile "call exec"
    inc hl
    ld (hl),lsb(call)
    inc hl
    ld (hl),msb(call)
    inc hl
    ld b,1                              ; b = nesting
def1:
    ld a,(de)                           
    inc de
    ld (hl),a
    inc hl
    
    cp ")"
    jr z,def4
    cp "}"                       
    jr z,def4
    cp "]"
    jr z,def4

    cp "("
    jr z,def3
    cp "{"
    jr z,def3
    cp "["
    jr z,def3

    cp "'"
    jr z,def3
    cp "`"
    jr nz,def1
def2:
    inc b
    jr def1                   
def3:
    ld a,$80
    xor b
    ld b,a
    jr def1                   
def4:
    dec b
    jr nz, def1                 ; get the next element
    xor a                       ; end with NUL ??? needed?
    ld (hl),a

    ld de,(vHeapPtr)            ; de = start of definition
    ld (vHeapPtr),hl            ; update heap ptr to end of definition

    pop hl                      ; de = addr, hl = IP
    ex (sp),hl                  ; hl = symbol de = addr (sp) = IP
    ld bc,hl                    ; bc = symbol
    call defineEntry
    jr c,def5
    ; call error
    ; .cstr "Def Collision"
def5:
    pop bc
    jp (ix)

; symbol array block -- 
closure:
    pop hl                              ; hl = block
    pop de                              ; de = array
    push bc                             ; (sp) = block, (sp+2) = IP, (sp+2) = symbol                         
    push hl
    ld hl,(vHeapPtr)                    ; hl = heap ptr de = array
    ld (hl),$cd                         ; compile "call doclosure"
    inc hl
    ld (hl),lsb(doclosure)
    inc hl
    ld (hl),msb(doclosure)
    inc hl
    ld (hl),e                           ; compile array
    inc hl
    ld (hl),d
    inc hl
    pop de                              ; de =  block, (sp) = IP, (sp+2) = symbol
    ld b,1                              ; b = nesting
    jr def1

; symbol value -- 
let:
    ld hl,bc                            ; de = addr (sp) = IP (sp+2) = symbol
    ex (sp),hl                          
    ex de,hl                            
    ld hl,(vHeapPtr)                    ; hl = heap
    ld (hl),$cd                         ; compile "call dovar"
    inc hl
    ld (hl),lsb(dovar)
    inc hl
    ld (hl),msb(dovar)
    inc hl
    ld (hl),e
    inc hl
    ld (hl),d
    dec hl

    ld de,(vHeapPtr)            ; de = start of definition
    ld (vHeapPtr),hl            ; update heap ptr to end of definition

    pop hl                      ; de = addr, hl = IP
    ex (sp),hl                  ; hl = symbol de = addr (sp) = IP
    ld bc,hl                    ; bc = symbol
    call defineEntry
    jr c,let2
    ; call error
    ; .cstr "Let Collision"
let2:
    pop bc
    jp (ix)

; symbol value -- 
const:
    ld hl,bc                            ; de = addr (sp) = IP (sp+2) = symbol
    ex (sp),hl                          
    ex de,hl                            
    ld hl,(vHeapPtr)                    ; hl = heap
    ld (hl),$cd                         ; compile "call doconst"
    inc hl
    ld (hl),lsb(doconst)
    inc hl
    ld (hl),msb(doconst)
    inc hl
    ld (hl),e
    inc hl
    ld (hl),d
    inc hl
    
    ld de,(vHeapPtr)            ; de = start of definiition
    ld (vHeapPtr),hl            ; update heap ptr to end of definition

    pop hl                      ; de = addr, hl = IP
    ex (sp),hl                  ; hl = symbol de = addr (sp) = IP
    ld bc,hl                    ; bc = symbol
    call defineEntry
    jr c,const2
    ; call error
    ; .cstr "Const Collision"
const2:
    pop bc
    jp (ix)

; str -- addr
addr:
    pop hl                              ; hl = hash
    push bc
    ld bc,hl
    call lookupEntry
    jr c, addr1
    ld hl,0
    ; call printStr		        
    ; .cstr "Undefined"
    ; jp interpret
addr1:    
    pop bc
    ld de,3
    add hl,de
    push hl
    jp (ix)

symbol:
    inc bc
    ld de,PAD
    ld h,msb(opcodesBase)                   ; this table identifies the char type
    jr symbol1
symbol0:                                 ; copy to PAD area 
    inc bc                              ; characters that are part of the identifier  
    inc de
symbol1:                                 ; 0-9 A-Z a-z _
    ld a,(bc)
    ld (de),a
    or a
    jr z,symbol2
    ld l,a
    ld a,(hl)
    cp lsb(ident_)
    jr z,symbol0
    cp lsb(num_)
    jr z,symbol0
symbol2:
    dec bc
    xor a
    ld (de),a                           ; terminate string with NUL
    push bc
    ld bc,PAD
    call hashStr                        ; hl = hash
    pop bc
    push hl
    jp (ix)
    
ident:
    ld de,PAD
    ld h,msb(opcodesBase)                   ; this table identifies the char type
    jr ident1
ident0:                                 ; copy to PAD area 
    inc bc                              ; characters that are part of the identifier  
    inc de
ident1:                                 ; 0-9 A-Z a-z _
    ld a,(bc)
    ld (de),a
    or a
    jr z,ident2
    ld l,a
    ld a,(hl)
    cp lsb(ident_)
    jr z,ident0
    cp lsb(num_)
    jr z,ident0
ident2:
    dec bc
    xor a
    ld (de),a                           ; terminate string with NUL
    push bc
    ld bc,PAD
    call hashStr                        ; hl = hash
    ld bc,hl
    call lookupEntry
    pop bc
    jr c, ident3                        ; todo: no entry? print an error message 
    jp (ix)
ident3:    
    jp (hl)

frac:
    ld hl,(vFrac)
    push hl
    jp (ix)

sqrt1:
    pop hl
    push bc
    call squareRoot
    ld (vFrac),bc
    pop bc
    push de
    jp (ix)

abs1:
    pop hl
    bit 7,h
    ret z
    xor a  
    sub l  
    ld l,a
    sbc a,a  
    sub h  
    ld h,a
    push hl
    jp (ix)

mod:                           
    pop  de                     ; get first value
    pop  hl                     ; get 2nd value
    push bc                     ; preserve the IP    
    ld bc,hl                
    call divide
    pop bc
    push hl                     ; push remainder    
    jp (ix)

; hl = value1, de = value2
; hl = result
equals:
    or a                        ; reset the carry flag
    sbc hl,de                   ; only equality sets hl=0 here
    jr z, true1
    jp false1

; hl = value1 de = value2
; hl = result
lessthaneq:    
    or a                        
    sbc hl,de    
    jr lessthan1

; hl = value1 de = value2
; hl = result
lessthan:
    or a                        
    sbc hl,de    
    jr z,false1    

lessthan1:
    jp m,false1

true1:
    ld hl, TRUE
    push hl
    jp (ix) 

false1:
    ld hl, FALSE
    push hl
    jp (ix) 

; Z80 port input
; port -- value 
input:			    
    pop hl
    ld e,c                      ; save IP
    ld c,l
    in l,(c)
    ld h,0
    ld c,e                      ; restore IP
    push hl
    jp (ix)    

; Z80 port output
; value port --
output:
    pop hl
    ld e,c                      ; save IP
    ld c,l
    pop hl
    out (c),l
    ld c,e                      ; restore IP
    jp (ix)    

key:
    call getchar
    ld h,0
    ld l,a
    push hl
    jp (ix)

neg:    
    ld hl, 0    		        ; NEGate the value on top of stack (2's complement)
    pop de       
    jp sub2                     ; use the SUBtract routine
    
filter:
map:
scan:
    jp (ix)


; -------------------------------------------------------------------------------


; hash C-string 
; BC = str
; HL = hash
hashStr:
    ld hl,0                             
hashStr1:    
    ld a,(bc)                           ; load next char
    inc bc
    cp 0                                ; NUL?
    ret z                     
hashStr2:
    ld d,0
    ld e,a 
    add hl,de
    ld de,hl                            ; hl *= 193 (11000001)
    add hl,hl                           ; shift left
    add hl,de                           ; add
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,hl                           ; shift left
    add hl,de                           ; add
    jr hashStr1

; add entry to hash slots and hash pointers
; bc = hash (b = hi, c = lo), de = addr
; sets carry if successful
defineEntry:               
    sla c                               ; lo = lo * 2
    ld l,c                              ; lo1 = lo
    ld h,msb(hashSlots)                 ; hl = slots[lo*4]
defineEntry0:
    ld a,(hl)                           ; a = (lo1)
    cp UNUSED                           ; is it unused?
    jr z,defineEntry3                   ; yes, add entry
    ld a,c                              ; a = lo
    cp (hl)                             ; compare (lo1) with lo
    jr nz,defineEntry1                  ; no match loop around
    inc l 
    ld a,b                              ; a = hi
    cp (hl)                             ; compare (lo1+1) with hi
    jr z,defineEntry2                   ; identical hash, collision, exit
    dec l                               ; restore l
defineEntry1:
    inc l                               ; try next entry
    inc l 
    ld a,c                              ; compare lo and lo1
    cp l                                ; if equal then there's no space left, reject 
    jr nz,defineEntry0
defineEntry2:
    or a                                ; clear carry flag, failure
    ret
defineEntry3:                           ; new entry
    ld (hl),c                           ; (lo1) = hash lo
    inc hl
    ld (hl),b                           ; (lo1 + 1) = hash hi
    ld h,msb(hashWords)                 ; hl = slots[lo*4]
    ld (hl),d
    dec hl
    ld (hl),e                           ; (slot + 2) = address
    scf                                 ; set carry flag, success
    ret

; looks up hash and returns address
; bc = hash
; returns addr in hl, sets carry if successful
lookupEntry:
    sla c                               ; lo = lo * 2
    ld l,c                              ; lo1 = lo
    ld h,msb(hashSlots)                 ; hl = slots[lo*4]
lookupEntry0:
    ld a,(hl)                           ; a = (hl), slot
    cp UNUSED                           ; is it unused?
    jr z,defineEntry2                   ; yes, does not exist
    ld a,c                              ; a = lo
    cp (hl)                             ; compare (lo1) with lo
    jr nz,lookupEntry1                  ; no match loop around
    inc l 
    ld a,b                              ; a = hi
    cp (hl)                             ; compare (lo1+1) with hi
    jr z,lookupEntry3
    dec l
lookupEntry1:
    inc l 
    inc l 
    ld a,c 
    cp l                                ; no space left, reject 
    jr nz,lookupEntry0
lookupEntry2:
    or a                                ; clear carry flag, failure
    ret
lookupEntry3:
    ld h,msb(hashWords)                 ; hl = slots[lo*4]
    ld d,(hl)
    dec l                               ; restore l
    ld e,(hl)                           ; (slot + 2) = address
    ex de,hl
    scf
    ret

; division subroutine.
; bc: divisor, de: dividend, hl: remainder

divide:        
    ld hl,0    	                        ; zero the remainder
    ld a,16    	                        ; loop counter
divide1:		                        ; shift the bits from bc (numerator) into hl (accumulator)
    sla c
    rl b
    adc hl,hl
    sbc hl,de		                    ; check if remainder >= denominator (hl>=de)
    jr c,divide2
    inc c
    jr divide3
divide2:		                        ; remainder is not >= denominator, so we have to add de back to hl
    add hl,de
divide3:
    dec a
    jr nz,divide1
    ld de,bc                              ; result from bc to de
    ret

; squareroot
; Input: HL = value
; Result: DE = square root BC = remainder

squareRoot:
    ld bc,0800h   
    ld e,c        
    xor a         
squareRoot1:        
    add hl,hl     
    rl c          
    adc hl,hl     
    rl c          
    jr nc,$+4     
    set 0,l       
    ld a,e        
    add a,a       
    ld e,a        
    add a,a       
    bit 0,l       
    jr nz,$+5     
    sub c         
    jr nc,squareRoot4     
    ld a,c         
    sub e              
    inc e          
    sub e           
    ld c,a         
squareRoot4:
    djnz squareRoot1
    bit 0,l       
    jr z,squareRoot5         
    inc b         
squareRoot5:
    ld d,0
    ret           
 
prtdec:        
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
    ld c,0                      ; leading zeros flag = false
    ld de,-10000
    call prtdec1
    ld de,-1000
    call prtdec1
    ld de,-100
    call prtdec1
    ld e,-10
    call prtdec1
    inc c                       ; flag = true for at least digit
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
                                 
prthex:                         ; display hl as a 16-bit number in hex.
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

prtstr0:
    call putchar
    inc hl
prtstr:
    ld a,(hl)
    or a
    jr nz,prtstr0
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
 
prompt:          
    call printStr
    .cstr "\r\n> "
    ret

crlf:       
    call printStr
    .cstr "\r\n"
    ret

printStr:        
    ex (sp),hl		            ; swap			
    call prtstr		
    inc hl			            ; inc past NUL
    ex (sp),hl		            ; put it back	
    ret

define:
    pop hl
    ld a,(hl)
    inc hl
    ld bc,hl
    ld e,a
    ld d,0
    add hl,de
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    push hl                             ; bc = str 
    push de
    call hashStr                        ; hl = hash
    pop de
    ld bc,hl
    jp defineEntry

init:       
    ld ix,next
    ld iy,STACK
    ld hl,isysVars
    ld de,sysVars
    ld bc,8 * 2
    ldir
    
    ld a,UNUSED
    ld b,0
    ld hl, hashSlots
init1:
    ld (hl),a
    inc hl
    djnz init1 

    call define
    .pstr "abs",0                       
    dw abs1

    call define
    .pstr "addr",0                       
    dw addr

    call define
    .pstr "bytes",0                       
    dw bytes

    call define
    .pstr "call",0                       
    dw call

    call define
    .pstr "case",0                       
    dw case

    call define
    .pstr "const",0                       
    dw const

    call define
    .pstr "closure",0                       
    dw closure

    call define
    .pstr "def",0                       
    dw def

    call define
    .pstr "exec",0                       
    dw exec

    call define
    .pstr "false",0                       
    dw false

    call define
    .pstr "filter",0                       
    dw filter

    call define
    .pstr "frac",0                       
    dw frac

    call define
    .pstr "get",0                       
    dw get

    call define
    .pstr "hash",0                       
    dw hash

    call define
    .pstr "input",0                       
    dw input

    call define
    .pstr "if",0                       
    dw if

    call define
    .pstr "ifte",0                       
    dw ifte

    call define
    .pstr "key",0                       
    dw key

    call define
    .pstr "let",0                       
    dw let

    call define
    .pstr "loop",0                       
    dw loop

    call define
    .pstr "map",0                       
    dw map

    call define
    .pstr "mod",0                       
    dw mod

    call define
    .pstr "neg",0                       
    dw neg

    call define
    .pstr "output",0                       
    dw output

    call define
    .pstr "scan",0                       
    dw scan

    call define
    .pstr "select",0                       
    dw select

    call define
    .pstr "set",0                       
    dw set

    call define
    .pstr "shl",0                       
    dw shl

    call define
    .pstr "shr",0                       
    dw shr

    call define
    .pstr "sqrt",0                       
    dw sqrt1

    call define
    .pstr "switch",0                       
    dw switch

    call define
    .pstr "true",0                       
    dw true

    call define
    .pstr "words",0                       
    dw words

    ret

start:
    ld sp,STACK		        ; start of Siena
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
    cp $0                       ; is it end of string? NUL end of string
                                ; ???? NEEDED?
    jr z,waitchar4
    cp '\r'                     ; carriage return? ascii 13
    jr z,waitchar3		        ; if anything else its macro/control 

macro:       
;  ld (vTIBPtr),bc
;  ld hl,ctrlCodes
;  add a,l			            ; look up key of macros
;  ld l,a
;  ld e,(hl)
;  ld a,e
;  or a
;  jr z,macro1
;  ld d,msb(macros)
;  push de
;  call call		            ; Siena exec_ operation and jump to it
;  db DC1,0
; macro1:
;  ld bc,(vTIBPtr)
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
    ld (hl),ETX                 ; store end of text ETX in text buffer ??? NEEDED?
    inc bc

waitchar4:    
    ld (vTIBPtr),bc
    ld bc,TIB                   ; Instructions stored on heap at address HERE, 
                                ; we pressed enter
    dec bc

next:        
    inc bc                      ; Increment the IP
    ld a,(bc)                   ; Get the next character and dispatch
    ; bit 7,a                   ; is 15-bit opcode ?
    ; jr nz,next3
    cp " "                      ; whitespace?
    jr z,next                   ; space? ignore
    jr c,next1
    ld l,a                      ; index into table
    ld h,msb(opcodesBase)       ; start address of jump table    
    ld l,(hl)                   ; get low jump address
    ld h,msb(page4)             ; Load h with the 1st page address
    jp (hl)                     ; Jump to routine
next1:
    cp ESC                      ; escape from interpreter
    jr z,escape                   
    cp NUL                      ; end of input string?
    jr z,exit
    ; cp ETX                      ; end of command line input text?
    ; jr nz,next                   
;     ld hl,-STACK               ; etx, is SP valid? (too many pops?)
;     add hl,sp
;     jr nc,next2
;     ld sp,STACK                ; yes, reset stack
; next2:
    jp interpret                ; no, other whitespace, macros?
; next3:
;     ld h,a                    ; build address                      
;     inc bc
;     ld a,(bc)
;     ld l,a
;     add hl,hl
;     jp (hl)
        
escape:
    ld hl,bc                    ; address of code after escape opcode
    inc hl			            
    jp (hl)
  
; clear stack args
clear:
    ld d,iyh                    ; de = BP
    ld e,iyl
    ex de,hl                    ; hl = BP, de = result
    ld sp,hl                    ; sp = BP
    ld hl,0
    ld (vDataWidth),hl
    jp (ix)

exit:
    ld de,bc                    ; address of code after exit opcode
    inc de			            
    exx
    pop bc                      ; bc = last result 
    ld d,iyh                    ; de = BP
    ld e,iyl
    ex de,hl                    ; hl = BP 
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

; execute a block of code
; uses parent scope
exec:				            ; execute code at pointer
    pop hl                      ; hl = pointer to code
    ld a,h                      ; skip if destination address is NUL
    or l
    jr z,exec2
    push bc                     ; push IP 
    ld e,(iy+2)                 ; get SCP from parent stack frame
    ld d,(iy+3)                 ; make this the old BP for this stack frame
    push de                     ; push SCP
    push iy                     ; push BP
    ld iy,0                     ; BP = SP
    add iy,sp
    ld bc,hl                    ; IP = pointer to code
    dec bc                      ; dec to prepare for next routine
exec2:
    jp (ix)       

; call with args
; creates a scope
call:				            ; execute code at pointer
    pop hl                      ; hl = pointer to code
call1:
    ld a,h                      ; skip if destination address is NUL
    or l
    jr z,call2
    push bc                     ; push IP 
    push iy                     ; push SCP (scope pointer)
    push iy                     ; push BP
    ld iy,0                     ; BP = SP
    add iy,sp
    ld bc,hl                    ; IP = pointer to code
    dec bc                      ; dec to prepare for next routine
call2:
    jp (ix)       

; call with args
; pushes array, creates a scope
doclosure:
    pop hl
    ld e,(hl)                   ; load array and push
    inc hl
    ld d,(hl)
    inc hl
    push de
    jp call1
    
; -- addr
; returns address of variable
dovar:				            ; execute code at pointer
    jp (ix)

; -- value
; returns address of variable
doconst:				        ; execute code at pointer
    pop hl
    ld e,(hl)
    inc hl
    ld d,(hl)
    push de
    jp (ix)

    
    