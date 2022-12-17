        .ORG RAMSTART

TIB:        DS TIBSIZE

            DS RSIZE
rStack:        

            DS DSIZE
dStack:        
stack:
            .align $100

pad:
            DS $100

            DS $30
vByteMode:  DS 2                ; 
tbPtr:      DS 2                ; reserved for tests

RST08:      DS 2                 
RST10:      DS 2                 
RST18:      DS 2                 
RST20:      DS 2                 
RST28:      DS 2                 
RST30:      DS 2                ; 
BAUD        DS 2                ; 
INTVEC:     DS 2                ; 
NMIVEC:     DS 2                ; 
GETCVEC:    DS 2                ;   
PUTCVEC:    DS 2                ;   

            DS 26*2
sysVars:

            DS 2                ; a
vDataWidth: DS 2                ; b
vTIBPtr:    DS 2                ; c
            DS 2                ; d
vLastDef:   DS 2                ; e
            DS 2                ; f
            DS 2                ; g
vHeapPtr:   DS 2                ; h
            DS 2                ; i
            DS 2                ; j
            DS 2                ; k
            DS 2                ; l  
            DS 2                ; m  
            DS 2                ; n
            DS 2                ; o
            DS 2                ; p
            DS 2                ; q
            DS 2                ; r     
            DS 2                ; s
            DS 2                ; t
            DS 2                ; u
            DS 2                ; v
            DS 2                ; w
            DS 2                ; x     
            DS 2                ; y
            DS 2                ; z

; ****************************************************************
; NS Table - Each space holds 26 user commands, 26 user vars, 12 bytes free
; ****************************************************************
            .align $40

DATA:       ds dataSize

            .align $100
hashSlots:      ds $100

            .align $100
hashWords:      ds $100

HEAP:         
