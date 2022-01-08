	TITLE	'CHARACTER I/O HANDLER FOR CP/M 3.0'

	; DEFINE LOGICAL VALUES:
TRUE		EQU	-1
FALSE		EQU	NOT TRUE

	; DETERMINE IF FOR BANK SELECT OR NOT:
BANKED		EQU	FALSE	;<------------------- BANKED VERSION

	; DEFINE PUBLIC LABELS:
	PUBLIC	?CINIT,?CI,?CO,?CIST,?COST
	PUBLIC	@CTBL

	; DEFINE EXTERNAL LABELS AND ENTRY POINTS:
	IF	BANKED
	EXTRN	@CBNK
	EXTRN	?BNKSL
	ENDIF
	EXTRN	OUT$BLOCKS	;BLOCK OUTPUT ROUTINE TO I/O PORTS
	EXTRN	?PMSG



	; INCLUDE Z-80 MACROS:
;	MACLIB	Z80

	; EQUATES FOR MODE BYTE BIT FIELDS

MB$INPUT	EQU 0000$0001B	; DEVICE MAY DO INPUT
MB$OUTPUT	EQU 0000$0010B	; DEVICE MAY DO OUTPUT
MB$IN$OUT	EQU MB$INPUT+MB$OUTPUT

MB$SOFT$BAUD	EQU 0000$0100B	; SOFTWARE SELECTABLE BAUD RATES

MB$SERIAL	EQU 0000$1000B	; DEVICE MAY USE PROTOCOL
MB$XON$XOFF	EQU 0001$0000B	; XON/XOFF PROTOCOL ENABLED

BAUD$NONE	EQU 0		; NO BAUD RATE ASSOCIATED WITH THIS DEVICE
BAUD$50		EQU 1		; 50 BAUD
BAUD$75		EQU 2		; 75 BAUD
BAUD$110	EQU 3		; 110 BAUD
BAUD$134	EQU 4		; 134.5 BAUD
BAUD$150	EQU 5		; 150 BAUD
BAUD$300	EQU 6		; 300 BAUD
BAUD$600	EQU 7		; 600 BAUD
BAUD$1200	EQU 8		; 1200 BAUD
BAUD$1800	EQU 9		; 1800 BAUD
BAUD$2400	EQU 10		; 2400 BAUD
BAUD$3600	EQU 11		; 3600 BAUD
BAUD$4800	EQU 12		; 4800 BAUD
BAUD$7200	EQU 13		; 7200 BAUD
BAUD$9600	EQU 14		; 9600 BAUD
BAUD$19200	EQU 15		; 19.2K BAUD


	; MISCELLANEOUS EQUATES:

CRTSTAT	EQU	010H		;STATUS PORT TO CHECK CRT OUTPUT STATUS
KEYSTAT	EQU	010H
SIOAS	EQU	10h
SIOAD	EQU	11h
KEYIN	EQU	011H
KEYOUT	EQU	011H
SIOBS	equ	12h
SIOBD	equ	13h
CR	EQU	0DH
LF	EQU	0AH

p$zpio2	equ	20h	; place holder
p$cent$stat     equ p$zpio2+0
p$cent$data     equ p$zpio2+1
p$zpio2a        equ p$zpio2+2
p$zpio2b        equ p$zpio2+3

	; WILL START OFF IN COMMON MEMORY FOR BANKED OR NON-BANKED SYSTEMS:
	CSEG

	; ENTER HERE FOR BANKED SYSTEM DEVICE INPUT:
?CI:	
	jmp	CI
	; ENTER HERE FOR BANKED SYSTEM DEVICE OUTPUT:
?CO:	
	jmp	CO
	; ENTER HERE FOR BANKED SYSTEM DEVICE INPUT STATUS:
?CIST:	
	jmp	CIST0
	; ENTER HERE FOR BANKED SYSTEM DEVICE OUTPUT STATUS:
?COST:	
	jmp	COST0
	;;;;; DEVICE 0 HANDLERS (SD SYSTEMS VIDIO BOARD)
?CINIT:	; DEVICE 0 INITIALIZATION 
	MVI	A,3
	OUT	(KEYSTAT)
	out	(SIOBS)
	MVI	A,11h		; 8 data bits 2 stop bits
	OUT	(KEYSTAT)
	mvi	a,1		; 7 data bits even parity 2 stop bits
	out	(SIOBS)
	call	cent$init
	RET
;
;       ;Print a string in [DE] up to '$'
PSTRING:
        PUSH    B
        PUSH    D
        PUSH    H
        XCHG
PSTRX:  MOV     A,M
        CPI     '$'
        JZ      DONEP
        MOV     C,A
        CALL    CO0
        INX     H
        JMP     PSTRX
DONEP:  POP     H
        POP     D
        POP     B
        RET

STATUS$MSG	DB	CR,LF,'In input status','$'
KEYIN$MSG	DB	CR,LF,'In Keyin ','$'

;
; 
CIST0:				; DEVICE 0 INPUT STATUS:
CSTS:
	mov	a,b	; get dev nr.
	cpi	3
	jnc	null$status
	mov	l,b	; create data ports
	mvi	h,0	; index
	lxi	d,data$ports
	dad	d
	mov	a,m	; get port nr
	mov	c,m
	dcr	a
	sta	cs$in
	db	0dbh	; read the port
cs$in	db	0
	ani	1	; is there data ?
	rz		;  No.... 
	ori	0ffh	; or yes
	ret
;
CI0:				;DEVICE 0 INPUT:
CI:
	mov	a,b	; copy dev nr
	cpi	3
	jnc	null$input ; don't over run the table	
ci1:	call	CSTS	; get serial status 
	jz	ci1
	mov	a,c	; c has the port nr
	sta	ci$in
	db	0dbh	; read a byte
ci$in:	db	0
;	ani	7fh      ; possible bug here.....
	ret
;
null$input:
	mvi	a,1ah
	ret
;
COST0:	; DEVICE 0 OUTPUT STATUS:
	mov	a,b		; copy port nr
	cpi	3		; don't overrun the table
	jz	cent$stat	; well is it printer
	jnc	null$status
	mov	l,b		; form an index
	mvi	h,0
	lxi	d,data$ports	; add to table
	dad	d
	mov	a,m		; get port value
	dcr	a		; data is odd, stat is even
; ack self modifying code, BTW will not work in a rom
	sta	st$out
	db	0dbh		 
st$out	db	0
; we read the sio status register
	ani	2		; is buffer empty
	rz
	ori	0ffh
	ret
;
; this code can output to multiple devices
;
CO0:				; DEVICE 0 OUTPUT:
CO:	; character output....
	mov	a,b	; get device nr
	cpi	3	; centronics printer
	jz	centronics$out
	jnc	null$output
	mov	a,c	; save the output character
	push	psw
	push	b	; save device number
co$spin:		
	call	COST0	; poll status
	jz	co$spin	; not ready yet.
	pop	h	; get back device nr in hl
	mov	l,h
	mvi	h,0
	lxi	d,data$ports
	dad	d	; add index of table
	mov	a,m	; get port address.
	sta	co$out
	pop	psw
	db	0d3h
co$out	db	0
null$output:
	ret

null$status:
	xra	a
	ret
;
	;;;;; NULL ROUTINES:
NULL$CIST:
NULL$COST:
	XRA	A		;RETURN A FALSE STATUS RESULT
	RET
NULL$CI:
	MVI	A,1AH		;FOR INPUT RETURN A CNTL-Z (EOF)
NULL$INIT:
NULL$CO:
NULL$RET:
	RET			;HARMLESS RETURN
cent$init: 
;; need code here
	ret	
cent$stat:
;; need code here
	ret
centronics$out:
;; need code here
	ret

	; STORAGE FOR DEVICE CODE -- CAN RESIDE IN SAME SEGMENT AS THE BULK
	;  OF CHARACTER I/O ROUTINES:
DEV$CODE:	DS	1

	;;;;; CHRTBL
	; CHARACTER DEVICE TABLE
	CSEG			;MUST RESIDE IN COMMON MEMORY
@CTBL:
	DB	'CRT   '		;CONSOLE (DEVICE 0)
	DB	MB$IN$OUT
	DB	BAUD$NONE
	db	'CRT1  '
	db	MB$IN$OUT,BAUD$NONE
; add more crt's here 
	db	'CEN   '
	db	MB$OUTPUT,BAUD$NONE
MAX$DEVICES	EQU	($-@CTBL)/8	;# DEVICES IN TABLE
	DB	0			;TABLE TERMINATOR

;
; to add more sio ports you need to extend this
; table. 
data$ports:
	db	SIOAD,SIOBD	
	; OTHER DATA AREAS:
	DS	24		;CHARACTER I/O LOCAL STACK
IOSP	EQU	$
SPSAVE	DS	2

	END
