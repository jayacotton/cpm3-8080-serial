;==================================================================================
; Contents of this file are copyright Grant Searle
; HEX routine from Joel Owens.
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.hostei.com/grant/index.html
;
; eMail: home.micros01@btinternet.com
;
; If the above don't work, please perform an Internet search to see if I have
; updated the web page hosting service.
;
;==================================================================================

TPA	EQU	100H
REBOOT	EQU	0H
BDOS	EQU	5H
CONIO	EQU	6
CONINP	EQU	1
CONOUT	EQU	2
PSTRING	EQU	9
MAKEF	EQU	22
CLOSEF	EQU	16
WRITES	EQU	21
DELF	EQU	19
SETUSR	EQU	32

CR	EQU	0DH
LF	EQU	0AH

FCB	EQU	05CH
BUFF	EQU	080H

	ORG TPA


	MVI	A,0
	STA	buffPos
	STA	checkSum
	STA	byteCount
	STA	printCount
	LXI	H,BUFF
	SHLD	buffPtr


WAITLT:	CALL	GETCHR
	CPI	'U'
	JZ	SETUSER
	CPI	':'
	JNZ	WAITLT


	MVI	C,DELF
	LXI	D,FCB
	CALL	BDOS

	MVI	C,MAKEF
	LXI	D,FCB
	CALL	BDOS

GETHEX:
	CALL 	GETCHR
	CPI	'>'
	JZ	CLOSE
	MOV   B,A
	PUSH B
	CALL GETCHR
	POP B
	MOV   C,A

	CALL BCTOA

	MOV	B,A
	LDA	checkSum
	ADD	B
	STA	checkSum
	LDA	byteCount
	INR	A
	STA	byteCount

	MOV	A,B

	LHLD	buffPtr

	MOV	M,A
	INX	H
	SHLD	buffPtr

	LDA	buffPos
	INR	A
	STA	buffPos
	CPI	80H

	JNZ	NOWRITE

	MVI	C,WRITES
	LXI	D,FCB
	CALL	BDOS
	MVI	A,'.'
	CALL	PUTCHR

        ; New line every 8K (64 dots)
	LDA	printCount
	INR	A
	CPI	64
	JNZ	noCRLF
	STA	printCount
	MVI	A,CR
	CALL	PUTCHR
	MVI	A,LF
	CALL	PUTCHR
	MVI	A,0
noCRLF:	STA	printCount

	LXI	H,BUFF
	SHLD	buffPtr

	MVI	A,0
	STA	buffPos
NOWRITE:JMP	GETHEX
	

CLOSE:	LDA	buffPos
	CPI	0
	JZ	NOWRITE2

	MVI	C,WRITES
	LXI	D,FCB
	CALL	BDOS
	MVI	A,'.'
	CALL	PUTCHR

NOWRITE2:
	MVI	C,CLOSEF
	LXI	D,FCB
	CALL	BDOS

; Byte count (lower 8 bits)
	CALL 	GETCHR
	MOV   B,A
	PUSH B
	CALL GETCHR
	POP B
	MOV   C,A

	CALL BCTOA
	MOV	B,A
	LDA	byteCount
	SUB	B
	CPI	0
	JZ	byteCountOK

	MVI	A,CR
	CALL	PUTCHR
	MVI	A,LF
	CALL	PUTCHR

	LXI	D,countErrMess
	MVI	C,PSTRING
	CALL	BDOS

	; Sink remaining 2 bytes
	CALL GETCHR
	CALL GETCHR

	JMP	FINISH

byteCountOK:

; Checksum
	CALL 	GETCHR
	MOV   B,A
	PUSH B
	CALL GETCHR
	POP B
	MOV   C,A

	CALL BCTOA
	MOV	B,A
	LDA	checkSum
	SUB	B
	CPI	0
	JZ	checksumOK

	MVI	A,CR
	CALL	PUTCHR
	MVI	A,LF
	CALL	PUTCHR

	LXI	D,chkErrMess
	MVI	C,PSTRING
	CALL	BDOS
	JMP	FINISH

checksumOK:
	MVI	A,CR
	CALL	PUTCHR
	MVI	A,LF
	CALL	PUTCHR

	LXI	D,OKMess
	MVI	C,PSTRING
	CALL	BDOS
		


FINISH:
	MVI	C,SETUSR
	MVI	E,0
	CALL	BDOS

	JMP	REBOOT


SETUSER:
	CALL	GETCHR
	CALL	HEX2VAL
	MOV	E,A
	MVI	C,SETUSR
	CALL	BDOS
	JMP	WAITLT

	
; Get a char into A
;GETCHR: LD C,CONINP
;	CALL BDOS
;	RET

; Wait for a char into A (no echo)
GETCHR: 
	MVI	E,0FFH
	MVI	C,CONIO
	CALL 	BDOS
	CPI	0
	JZ	GETCHR
	RET

; Write A to output
PUTCHR: MVI C,CONOUT
	MOV E,A
	CALL BDOS
	RET


;------------------------------------------------------------------------------
; Convert ASCII characters in B C registers to a byte value in A
;------------------------------------------------------------------------------
BCTOA:	MOV   A,B	; Move the hi order byte to A
	SBI  30H	; Take it down from Ascii
	CPI  0AH	; Are we in the 0-9 range here?
	JC   BCTOA1	; If so, get the next nybble
	SBI  07	; But if A-F, take it down some more
BCTOA1:	RRC		; Rotate the nybble from low to high
	RRC		; One bit at a time
	RRC		; Until we
	RRC		; Get there with it
	MOV   B,A	; Save the converted high nybble
	MOV   A,C	; Now get the low order byte
	SBI  30H	; Convert it down from Ascii
	CPI   0AH	; 0-9 at this point?
	JC   BCTOA2	; Good enough then, but
	SBI  07	; Take off 7 more if it's A-F
BCTOA2:	ADD  B	; Add in the high order nybble
	RET

; Change Hex in A to actual value in A
HEX2VAL: SBI	30H
	CPI	0AH
	RC	
	SBI	07
	RET


buffPos	DB	0H
buffPtr	DW	0000H
printCount DB	0H
checkSum DB	0H
byteCount DB	0H
OKMess	DB	'OK$'
chkErrMess DB	'======Checksum Error======$'
countErrMess DB	'======File Length Error======$'
	END
