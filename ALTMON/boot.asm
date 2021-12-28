; IMSAI boot loader
; z80 DualCF card IDE controller
;-------------------------------------------------------------------------
;
; Memory location equates

;	org	0d000h		;ROM location	test location
	org	0f800h		;ROM location	final location

SPTR	equ	$-0fffh		;stack pointer (use 256 byte boundary)
BOOTBAS equ	SPTR-0ffh

; Turnkey Board - set TKYDSBL to 1 to disable Turnkey PROMs. This is
;    done by reading the sense switch input port during initialization.

TKYDSBL	equ	0		;non-zero = disable turnkey PROMs
SENSESW	equ	0FFh		;sense switch input port

; 88-2SIO equates

CONS	equ	10h		;console status port
COND	equ	11h		;console data port
TBE	equ	2		;transmit buffer entry
RDA	equ	1		;receive data available

IDEportA        EQU     030H    ;Lower 8 bits of IDE interface (8255)
IDEportB        EQU     031H    ;Upper 8 bits of IDE interface
IDEportC        EQU     032H    ;Control lines for IDE interface
IDEportCtrl     EQU     033H    ;8255 configuration port
IDEDrive        EQU     034H    ;Bit 0 = 0 Drive A, =1 Drive B

READcfg8255     EQU     10010010b ;Set 8255 IDEportC to output, IDEportA/B input
WRITEcfg8255    EQU     10000000b ;Set all three 8255 ports to output mode

;IDE control lines for use with IDEportC.

IDEa0line       EQU     01H     ;direct from 8255 to IDE interface
IDEa1line       EQU     02H     ;direct from 8255 to IDE interface
IDEa2line       EQU     04H     ;direct from 8255 to IDE interface
IDEcs0line      EQU     08H     ;inverter between 8255 and IDE interface
IDEcs1line      EQU     10H     ;inverter between 8255 and IDE interface
IDEwrline       EQU     20H     ;inverter between 8255 and IDE interface
IDErdline       EQU     40H     ;inverter between 8255 and IDE interface
IDErstline      EQU     80H     ;inverter between 8255 and IDE interface

;Symbolic constants for the IDE Drive registers, which makes the
;code more readable than always specifying the address bits

REGdata         EQU     IDEcs0line
REGerr          EQU     IDEcs0line + IDEa0line
REGseccnt       EQU     IDEcs0line + IDEa1line
REGsector       EQU     IDEcs0line + IDEa1line + IDEa0line
REGcylinderLSB  EQU     IDEcs0line + IDEa2line
REGcylinderMSB  EQU     IDEcs0line + IDEa2line + IDEa0line
REGshd          EQU     IDEcs0line + IDEa2line + IDEa1line              ;(0EH)
REGcommand      EQU     IDEcs0line + IDEa2line + IDEa1line + IDEa0line  ;(0FH)
REGstatus       EQU     IDEcs0line + IDEa2line + IDEa1line + IDEa0line
REGcontrol      EQU     IDEcs1line + IDEa2line + IDEa1line
REGastatus      EQU     IDEcs1line + IDEa2line + IDEa1line + IDEa0line

;IDE Command Constants.  These should never change.

COMMANDrecal    EQU     10H
COMMANDread     EQU     20H
COMMANDwrite    EQU     30H
COMMANDinit     EQU     91H
COMMANDid       EQU     0ECH
COMMANDspindown EQU     0E0H
COMMANDspinup   EQU     0E1H


; IDE Status Register:
;  bit 7: Busy  1=busy, 0=not busy
;  bit 6: Ready 1=ready for command, 0=not ready yet
;  bit 5: DF    1=fault occured insIDE drive
;  bit 4: DSC   1=seek complete
;  bit 3: DRQ   1=data request ready, 0=not ready to xfer yet
;  bit 2: CORR  1=correctable error occured
;  bit 1: IDX   vendor specific
;  bit 0: ERR   1=error occured


; Misc Equates

CR	equ	13		;ASCII carriage return
LF	equ	10		;ASCII line feed
CTRLC	equ	3		;ASCII control-c
ESC	equ	27		;ASCII ESCAPE

IMSBoot	equ	$
	mvi	a,3		;reset 6850 uart
	out	CONS
	out	CONS+2		;2nd 2SIO port as well
	mvi	a,11h		;8N1 divide 1
	out	CONS
	out	CONS+2		;2nd 2SIO port as well
	lxi	sp,SPTR
	call	SELECTA
	call	IDEinit
CPMBOOT:                        ;Boot CPM from IDE system tracks -- if present
        XRA     A               ;Load from track 0,sec 1, head 0 (Always)
	sta	NOHOLEMODE
        STA     TRK+1
        STA     TRK
        LDA     NOHOLEMODE     ;Conveniently, this is 0 for hole mode, 1 for no hole mode
        STA     SEC

        MVI     A,CPMBOOTCOUNT ;Count of CPMLDR sectors  (12)
        STA     SECTORCOUNT
        LXI     H,CPMLDRADDRESS ;DMA address where the CPMLDR resides in RAM (100H)
        SHLD    DMA

NextRCPM:
        CALL    wrlba           ;Update LBA on drive
        CALL    DISPLAYposition ;Display current Track,sector,head#
        CALL    ZCRLF

        LHLD    DMA
        CALL    READSECTOR      ;read a sector
        SHLD    DMA

        LDA     SECTORCOUNT
        DCR     A
        STA     SECTORCOUNT
        JZ      LOADDONE

        LHLD    SEC
        INX     H
        SHLD    SEC            ;Note we assume we alway will stay on tarck 0 in this special case
        JMP     NextRCPM
LOADDONE:
	jmp	CPMLDRADDRESS		; see ya'
READSECTOR:
        CALL    wrlba           ;Tell which sector we want to read from.
                                ;Note: Translate first in case of an error otherewise we
                                ;will get stuck on bad sector
        CALL    IDEwaitnotbusy  ;make sure drive is ready
        JC      SHOWerrors      ;Returned with NZ set if error
        MVI     D,COMMANDread
        MVI     E,REGcommand
        CALL    IDEwr8D         ;Send sec read command to drive.
        CALL    IDEwaitdrq      ;wait until it's got the data
        JC      SHOWerrors
        LHLD    DMA            ;DMA address
        MVI     B,0             ;Read 512 bytes to [HL] (256X2 bytes)
MoreRD16:
        MVI     A,REGdata       ;REG regsiter address
        OUT     IDEportC
        ORI     IDErdline       ;08H+40H, Pulse RD line
        OUT     IDEportC
        IN      IDEportA        ;Read the lower byte first (Note very early versions had high byte then low byte
        MOV     M,A             ;this made sector data incompatable with other controllers).
        INX     H
        IN      IDEportB        ;THEN read the upper byte
        MOV     M,A
        INX     H
        MVI     A,REGdata       ;Deassert RD line
        OUT     IDEportC
        dcr     b
        jnz     MoreRD16
        MVI     E,REGstatus
        CALL    IDErd8D
        MOV     A,D
        ANI     1H
        CNZ     SHOWerrors      ;If error display status
        RET
wrlba:

        CALL    IDEwaitnotbusy  ;Make sure drive isn't busy...
        JC      SHOWErrors      ;If error, display status
        LDA     NOHOLEMODE     ;Are we leaving original holes, or not?
        ORA     A
        JZ      wrlbaHoles      ;Leaving original holes
        JMP     wrlbaNoHoles    ;No holes


wrlbaHoles:                     ;Write the logical block address to the drive's registers
                                ;Note we do not need to set the upper nibble of the LBA
                                ;It will always be 0 for these small drives
        LDA     SEC            ;LBA mode Low sectors go directly
        INR     A               ;Sectors are numbered 1 -- MAXSEC (even in LBA mode)
        STA     DRIVESEC       ;For Diagnostic Display Only
        MOV     D,A
        MVI     E,REGsector     ;Send info to drive
        CALL    IDEwr8D         ;Note: For drive we will have 0 - MAXSEC sectors only

        LHLD    TRK
        MOV     A,L
        STA     DRIVETRK
        MOV     D,L             ;Send Low TRK#
        MVI     E,REGcylinderLSB
        CALL    IDEwr8D

        MOV     A,H
        STA     DRIVETRK+1
        MOV     D,H             ;Send High TRK#
        MVI     E,REGcylinderMSB
        CALL    IDEwr8D

        MVI     D,1             ;For now, one sector at a time
        MVI     E,REGseccnt
        CALL    IDEwr8D
        RET
IDEwaitnotbusy:                 ;ie Drive READY if 01000000
        MVI     B,0FFH
        MVI     A,0FFH          ;Delay, must be above 80H for 4MHz Z80. Leave longer for slower drives
        STA     DELAYStore

MoreWait:
        MVI     E,REGstatus     ;wait for RDY bit to be set
        CALL    IDErd8D
        MOV     A,D
        ANI     11000000B
        XRI     01000000B
        JZ      DoneNotBusy
        DCR     B
        JNZ     MoreWait
        LDA     DELAYStore     ;Check timeout delay
        DCR     A
        STA     DELAYStore
        JNZ     MoreWait
        STC                     ;Set carry to indicate an error
        ret
DoneNotBusy:
        ORA     A              
	RET
IDEwr8D:                                ;WRITE Data in [D] to IDE register in [E]
        MVI     A,WRITEcfg8255          ;Set 8255 to write mode
        OUT     IDEportCtrl

        MOV     A,D                     ;Get data put it in 8255 A port
        OUT     IDEportA

        MOV     A,E                     ;select IDE register
        OUT     IDEportC

        ORI     IDEwrline               ;lower WR line
        OUT     IDEportC

        MOV     A,E                     ;<-- Ken Robbins suggestion, raise WR line
        OUT     IDEportC                ;deassert RD pin

        XRA     A                       ;Deselect all lines including WR line
        OUT     IDEportC
        MVI     A,READcfg8255           ;Config 8255 chip, read mode on return
        OUT     IDEportCtrl
        RET

IDErd8D:                                ;READ 8 bits from IDE register in [E], return info in [D]
        MOV     A,E
        OUT     IDEportC                ;drive address onto control lines

        ORI     IDErdline               ;RD pulse pin (40H)
        OUT     IDEportC                ;assert read pin

        IN      IDEportA
        MOV     D,A                     ;return with data in [D]

        MOV     A,E                     ;<---Ken Robbins suggestion
        OUT     IDEportC                ;deassert RD pin

        XRA     A
        OUT     IDEportC                ;Zero all port C lines
        ret
wrlbaNoHoles:
                                ;Write the logical block address to the drive's registers
                                ;Starting with LBA 0 and without leaving an "holes"
        LHLD    TRK            ;Get the "CPM" requested track High & Low
        MOV     A,L             ;Get Low byte of track
        RRC                     ;Get bottom two bits in high bits of A
        RRC
        ANI     0C0H            ;Just what were the bottom two bits (now at the top)
        MOV     C,A             ;Save in C
        LDA     SEC            ;Sector number in A
        ANI     03FH            ;Take only bottom 6 bits, just in case
        ORA     C               ;Add in top 2 bits of track
        STA     DRIVESEC       ;For diagnostic display only
        MOV     D,A             ;Send info to the drive
        MVI     E,REGsector
        CALL    IDEwr8D

        MOV     A,L             ;Get low byte of track again
        RRC                     ;Extract out just the top 6 bits
        RRC
        ANI     03FH
        MOV     C,A             ;Save in C
        MOV     A,H     ;Get high byte of track.
        RRC                     ;Rotate twice, leaving low 2 bits
        RRC                     ;In upper bits of A
        ANI     0C0H            ;Mask all but the two bits we want
        ORA     C               ;Add in the top 6 bits of the first track byte
        STA     DRIVETRK
        MOV     D,A             ;Send Low TRK#
        MVI     E,REGcylinderLSB
        CALL    IDEwr8D

        MOV     A,H             ;Get high byte of track
        RRC                     ;Just the top 6 bits
        RRC
        ANI     03FH
        STA     DRIVETRK+1
        MOV     D,A             ;Send High TRK#
        MVI     E,REGcylinderMSB
        CALL    IDEwr8D

        MVI     D,1             ;For now, one sector at a time
        MVI     E,REGseccnt
        CALL    IDEwr8D
        RET
IDEwaitdrq:
        MVI     B,0FFH
        MVI     A,0FFH          ;Delay, must be above 80H for 4MHz Z80. Leave longer for slower drives
        STA     DELAYStore

MoreDRQ:
        MVI     E,REGstatus     ;wait for DRQ bit to be set
        CALL    IDErd8D
        MOV     A,D
        ANI     10001000B
        CPI     00001000B
        JZ      DoneDRQ
        DCR     B
        JNZ     MoreDRQ
        LDA     DELAYStore     ;Check timeout delay
        DCR     A
        STA     DELAYStore
        JNZ     MoreDRQ
        STC                     ;Set carry to indicate error
        RET
DoneDRQ:
        ORA     A               ;Clear carry
        RET
DISPLAYposition:                ;Display current track,sector & head position
        LXI     D,msgCPMTRK     ;Display in LBA format
        CALL    PSTRING         ;---- CPM FORMAT ----
        LDA     TRK+1          ;High TRK byte
        CALL    phex
        LDA     TRK            ;Low TRK byte
        CALL    phex

        LXI     D,msgCPMSEC
        CALL    PSTRING         ;SEC = (16 bits)
        LDA     SEC+1          ;High Sec
        CALL    phex
        LDA     SEC            ;Low sec
        CALL    phex
                                ;---- LBA FORMAT ----
        LXI     D, msgLBA
        CALL    PSTRING         ;(LBA = 00 (<-- Old "Heads" = 0 for these drives).
        LDA     DRIVETRK+1     ;High "cylinder" byte
        CALL    phex
        LDA     DRIVETRK       ;Low "cylinder" byte
        CALL    phex
        LDA     DRIVESEC
        CALL    phex
        LXI     D, MSGBracket   ;)
        CALL    PSTRING
        RET
; Print an 8 bit number, located in [A]
phex    equ     $
PHEX:   PUSH    PSW
        PUSH    B
        PUSH    PSW
        RRC
        RRC
        RRC
        RRC
        CALL    ZCONV
        POP     PSW
        CALL    ZCONV
        POP     B
        POP     PSW
        RET

ZCONV:  ANI     0FH             ;HEX to ASCII and print it
        ADI     90H
        DAA
        ACI     40H
        DAA
        CALL   	ptcn 
	RET
PSTRING:
        PUSH    B
        PUSH    D
        PUSH    H
        XCHG
PSTRX:  MOV     A,M
        CPI     '$'
        JZ      DONEP
        CALL   	ptcn 
        INX     H
        JMP     PSTRX
DONEP:  POP     H
        POP     D
        POP     B
        RET
ZCRLF:
        PUSH    PSW
        MVI     a,CR
        CALL   	ptcn 
        MVI     a,LF
        CALL   	ptcn 
        POP     PSW
        RET
SHOWerrors:
SHOWErrors equ  $
        ORA     A               ;Set NZ flag
        STC                     ;Set Carry Flag
        RET
IDEinit:
        PUSH    B               ;Save used registers
        PUSH    D
                                ;Initialze the 8255 and drive then do a hard reset on the drive,
        MVI     A,READcfg8255   ;Config 8255 chip (10010010B), read mode on return
        OUT     IDEportCtrl     ;Config 8255 chip, READ mode

                                ;Hard reset the disk drive
                                ;For some reason some CF cards need to the RESET line
                                ;pulsed very carefully. You may need to play around
        MVI     A,IDErstline    ;with the pulse length. Symptoms are: incorrect data comming
        OUT     IDEportC        ;back from a sector read (often due to the wrong sector being read)
                                ;I have a (negative)pulse of 2.7uSec. (10Mz Z80, two IO wait states).
        MVI     B,20H           ;Which seem to work for the 5 different CF cards I have.
ResetDelay:
        DCR     B
        JNZ     ResetDelay      ;Delay (reset pulse width)
        XRA     A
        OUT     IDEportC        ;No IDE control lines asserted (just bit 7 of port C)
        CALL    DELAYSHORT      ;Short Delay
        CALL    IDEwaitnotbusy  ;Wait for drive
        JC      WaitInitErr
        MVI     D,11100000b     ;Data for IDE SDH reg (512bytes, LBA mode,single drive,head 0000)
                                ;For Trk,Sec,head (non LBA) use 10100000
                                ;Note. Cannot get LBA mode to work with an old Seagate Medalist 6531 drive.
                                ;have to use the non-LBA mode. (Common for old hard disks).

        MVI     E,REGshd        ;00001110,(0EH) for CS0,A2,A1,
        CALL    IDEwr8D         ;Write byte to select the MASTER device
        MVI     B,02H           ;<<< Adjust delay time for hard disks to get up to speed (Currently ~ 2 seconds)
                                ;<<< This delay need to be much longer for actual Hard Disks, OK for CF Cards.
WaitInit:
        MVI     E,REGstatus     ;Get status after initilization
        CALL    IDErd8D         ;Check Status (info in [D])
        MOV     A,D
        ANI     80H
        JNZ     WaitInitL       ;Need a longer wait...
        POP     D               ;Restore registers
        POP     B
        RET                     ;Return. Well check for errors when we get back
WaitInitL:
        MVI     A,2
        CALL    DELAYLONG       ;Long delay, drive has to get up to speed
        DCR     B
        JNZ     WaitInit
        XRA     A
        DCR     A
        POP     D
        POP     B
        RET                   
SELECTA:
        XRA     A               ; Select drive 0
        STA     CURRENTDRIVE
        OUT     IDEDrive
        RET
DELAYSHORT:                     ;DELAY ~32 MS (DOES NOT SEEM TO BE CRITICAL)
        MVI     A,40
DELAY3: MVI     B,0
M0:     equ     $
        dcr     b
        jnz     M0
        DCR     A
        JNZ     DELAY3
        RET
DELAYLONG:                      ;Long delay (Seconds)
        STA     DELAYStore
        PUSH    B
        LXI     B,0FFFFH        ;<<< May need to adjust delay time to allow cold drive to
DELAY2: LDA     DELAYStore     ;    get up to speed.
DELAY1: DCR     A
        JNZ     DELAY1
        DCX     B
        MOV     A,C
        ORA     B
        JNZ     DELAY2
        POP     B
        RET
WaitInitErr:
        XRA     A
        DCR     A               ;Return NZ (error)
        POP     D               ;Restore Registers
        POP     B
        RET                     ;Return - check for errors there.
ptcn    push    psw

ptlop   in      CONS            ;wait for OK to transmit
        ani     TBE
        jz      ptlop

        pop     psw             ;recover a
        ani     07fh            ;get rid of msbit
        out     COND            ;and print it
        ret                     ;return from ptcn

msgCPMTRK:      DB      'CPM TRK = $'
msgCPMSEC:      DB      ' CPM SEC = $'
msgLBA:         DB      '  (LBA = 00$'
MSGBracket:      DB      ')$'

SEC:	equ	BOOTBAS
SECTORCOUNT: equ	SEC+2
DMA:	equ	SECTORCOUNT+1
TRK:	equ	DMA+2
NOHOLEMODE: equ	TRK+2
DRIVESEC: equ NOHOLEMODE+2
DRIVETRK: equ DRIVESEC+2
DELAYStore: equ DRIVETRK+2
CURRENTDRIVE: equ DELAYStore + 2
CPMBOOTCOUNT:    EQU     12              ;Allow up to 12 CPM sectors for CPMLDR
CPMLDRADDRESS: equ	0100h
	end

