;
; SDTXT adapted for use with KB-9  Microsoft Basic for the KIM-1 V1.1
; Hans Otten, 2023. Original Copyright MTU 
;
; V1.1 changes to 1.0
; - CA64 syntax
; - 0x0400 base address
;
; V1.0 changes to original SDTXT
; - TASM32 syntax
; - moved to $8800
; - start Basic at $8A00
; - start Viable Memory address at $8A00
; - init routine to start the Visable Memory, clear screen and welcome message 
; - init routine to patch KB9 character output
;
         ;  'SIMPLIFIED VISABLE MEMORY TEXT DISPLAY SUBROUTINE'
;        THIS SUBROUTINE TURNS THE VISABLE MEMORY INTO A DATA DISPLAY
;        TERMINAL (GLASS TELETYPE).
;        CHARACTER SET IS 96 FULL ASCII UPPER AND LOWER CASE.
;        CHARACTER MATRIX IS 5 BY 7 SET INTO A 6 BY 9 RECTANGLE.
;        LOWER CASE IS REPRESENTED AS SMALL (5 BY 5) CAPITALS.
;        SCREEN CAPACITY IS 22 LINES OF 53 CHARACTERS FOR FULL SCREEW
;        OR 11 LINES FOR HALF SCREEN.
;        CURSOR IS A NON-BLINKING UNDERLINE.
;        CONTROL CODES RECOGNIZED:
;        CR     $0D         SETS CURSOR TO LEFT SCREEN EDGE
;        LF     $0A         MOVES CURSOR DOWN ONE LINE, SCROLLS
;                            DISPLAY UP ONE LINE IF ALREADY ON BOTTOM
;                            LINE
;        BS     $08         MOVES CURSOR ONE CHARACTER LEFT, DOES
;                            NOTHING IF ALREADY AT LEFT SCREEN EDGE
;        FF     $0C         CLEARS SCREEN AND PUTS CURSOR AT TOP LEFT
;                            OF SCREEN, SHOULD BE CALLED FOR
;                            INITIALIZATION
;        ALL OTHER CONTROL CODES IGNORED.
;        ENTER WITH CHARACTER TO BE DISPLAYED IN A.
;        X AND Y PRESERVED.
;        3 BYTES OF RAM STORAGE REQUIRED FOR KEEPING TRACK OF THE
;        CURSOR
;        4 BYTES OF TEMPORARY STORAGE IN BASE PAGE REQUIRED FOR ADDRESS
;        POINTERS. (CAN BE DESTROYED BETWEEN CALLS TO SDTXT
;        4 BYTES OF TEMPORARY STORAGE ANYWHERE (CAN BE DESTROYED
;        BETWEEN CALLS TO SDTXT)

;        * **** VMORG #MUST# BE SET TO THE PAGE NUMBER OF THE VISIBLE *
;        * MEMORY BEFORE CALLING SDTXT **** 
;                          *
VMORGB	 = $A0 	  		 ; base address K-1008
KB9STR	 = $4065		 ; V1.1 start address 
KB9OUTC  = $2A52		 ; KIM KB-9 out character 
KB9INCH	 = $2AE6		 ; KIM KB-9 in character (with hardware echo!) 
KB9INC2  = $2457 
KIMINCH	 = $1E5A		 ; KIM-1 in character (with hardware echo!) 
;
;        GENERAL EQUATES

NLOC     =      8000         ; NUMBER OF VISIBLE LOCATIONS
CHHI     =      9            ; CHARACTER WINDOW HEIGHT
CHWID    =      6            ; CHARACTER WINDOW WIDTH
NCHR     =      320/CHWID    ; NUMBER OF CHARACTERS PER LINE
NLIN     =      NLOC/40/CHHI ; NUMBER OF TEXT LINES
NSCRL    =      NLIN-1*CHHI*40 ; NUMBER OF LOCATIONS TO SCROLL
NCLR     =      NLOC-NSCRL   ; NUMBER OF LOCATIONS TO CLEAR AFTER SCROLL

;
;        BASE PAGE TEMPORARY STORAGE
;   
ADP1     = $EA	             ; ADDRESS POINTER 1
ADP2     = ADP1 + 2          ; ADDRESS POINTER 2
MSGPTR	 = ADP2 + 2 		 ; message pointer 
;
        .org	 $0400      
SDINIT: 	 CLD				 ; Clear decimal and stack
		 LDX 	#$FF
		 TXS			
; 
; Init K-1008
;		 
		 LDA 	#$0C		 ; clear screen
		 JSR 	SDTXT		 ;
                 LDA #(STRMSG & $FF)    ; high part 
                 STA MSGPTR
                 LDA #(STRMSG >> 8)     ; low part of address of message
                 STA MSGPTR + 1
                 JSR K1008MG
                 JSR KCRLF
	
	
;		
; Patch KB-9 OUTCH 
;
		LDA 	#(SDTXT & $FF)	 ; patch K1--08 iut character into KB-9	
		STA 	KB9OUTC			
		LDA		#(SDTXT >> 8)
		STA 	KB9OUTC + 1
;		
; Patch KB-9 INCHAR 
;
		LDA 	#(KIMIN & $FF)	 ; patch K1--08 iut character into KB-9	
		STA 	KB9INCH			
		LDA		#(KIMIN >> 8)
		STA 	KB9INCH + 1
		LDA 	#(KIMIN & $FF)	 ; patch K1--08 iut character into KB-9	
		STA 	KB9INC2			
		LDA		#(KIMIN >> 8)
		STA 	KB9INC2 + 1

;
; Start KB9
;
		 JMP 	KB9STR	 	 ; start KB9 
;
; KIM-1 in character, echo also on K-1008
;
KIMIN:  	 JSR 	KIMINCH 	 ; get KIM-1 character	
		 JSR 	SDTXT		 ; echo on K-1008
		 RTS
;
; 
;
; display message on K-1008
; ADP1 address of string 

K1008MG:	LDY #$00  
NPRMSG:         LDA (MSGPTR),Y  
                BEQ EPRMSG  
                JSR SDTXT 
                INY
                BEQ EPRMSG
                BNE NPRMSG  
EPRMSG:         RTS 
;
; Print CRLF on K-1008
;
KCRLF:   	 LDA 	#$0D  
		 JSR 	SDTXT		 ; 
		 LDA 	#$0A  
		 JSR 	SDTXT		 ; 
		 RTS


;        GENERAL TEMPORARY STORAGE

 

BTPT:    .byte	0            ; BIT NUMBER TEMPORARY STORAGE
DCNT1:   .word	0            ; DOUBLE PRECISION COUNTER
MRGT1:   .byte 	0            ; TEMPORARY STORAGE FOR MERGE

;        PERMANENT RAM STORAGE

CSRX:    .byte 	0            ; CURRENT CHARACTER NUMBER (0=LEFT CHAR)
CSRY:    .byte  0            ; CURRENT LINE NUMBER (0=TOP LINE)
VMORG:   .byte  VMORGB          ; FIRST PAGE NUMBER OF VISIBLE MEMORY
		
SDTXT:   PHA                 ; SAVE REGISTERS
         TXA
         PHA
         TYA
         PHA
         LDA    #0           ; CLEAR UPPER ADP2
         STA    ADP2+1
         TSX                 ; GET INPUT BACK
         LDA    $103,X
         AND    #$7F         ; INSURE 7 BIT ASCII INPUT
         SEC
         SBC    #$20         ; TEST IF A CONTROL CHARACTER
         BMI    SDTX10       ; JUMP IF SO

;        CALCULATE TABLE ADDRESS FOR CHAR SHAPE AND PUT IT INTO ADPL

SDTXT1:  STA    ADP2         ; SAVE CHARACTER CODE IN ADP2
         JSR    SADP2L       ; COMPUTE 8*CHARACTER CODE IN ADP2
         JSR    SADP2L
         JSR    SADP2L
         EOR    #$FF         ; NEGATE CHARACTER CODE
         SEC                 ; SUBSTRACT CHARACTER CODE FROM ADP2 AND
         ADC    ADP2         ; PUT RESULT IN ADP1 FOR A FINAL RESULT OF
         STA    ADP1         ; 7*CHARACTER CODE
         LDA    ADP2+1
         ADC    #$FF
         STA    ADP1+1
         LDA    ADP1         ; ADD IN ORIGIN OF CHARACTER TABLE
         CLC
         ADC    #CHTB&$FF
         STA    ADP1
         LDA    ADP1+1
         ADC    #CHTB/256
         STA    ADP1+1       ; ADP1 NOW HAS ADDRESS OF TOP ROW OF
                             ; CHARACTER SHAPE
;        COMPUTE BYTE AND BIT ADDRESS OF FIRST SCAN LINE OF
;        CHARACTER AT CURSOR POSITION

         JSR    CSRTAD       ; COMPUTE BYTE AND BIT ADDRESSES OF FIRST
                             ; SCAN LINE OF CHARACTER AT CURSOR POS.

;        SCAN OUT THE 7 CHARACTER ROWS

         LDY    #0           ; INITIALIZE Y INDEX=FONT TABLE POINTER
SDTX2:   LDA    (ADP1),Y     ; GET A DOT ROW FROM THE FONT TABLE
         JSR    MERGE        ; MERGE IT WITH GRAPHIC MEMORY AT (ADP2)
         JSR    DN1SCN       ; ADD 40 TO ADP2 TO MOVE DOWN ONE SCAN
                             ; LINE IN GRAPHIC MEMORY
         INY                 ; BUMP UP POINTER INTO FONT TABLE
         CPY    #7           ; TEST IF DONE
         BNE    SDTX2        ; GO DO NEXT SCAN LINE IF NOT
         LDA    CSRX         ; DO A CURSOR RIGHT
         CMP    #NCHR-1      ; TEST IF LAST CHARACTER ON THE LINE
         BPL    SDTX3        ; SKIP CURSOR RIGHT IF SO
         JSR    CSRCLR       ; CLEAR OLD CURSOR
         INC    CSRX         ; MOVE CURSOR ONE POSITION RIGHT
SDTX3:   JMP    SDTXRT       ; GO INSERT CURSOR, RESTORE REGISTERS,
                             ; AND RETURN

;        INTERPRET CONTROL CODES

SDTX10:  CMP    #$ED         ; TEST IF CR  $0D-$20=ED
         BEQ    SDTXCR       ; JUMP IF SO
         CMP    #$EA         ; TEST IF LF  $0a-$20=EA
         BEQ    SDTXLF       ; JUMP IF SO
         CMP    #$E8         ; TEST IF BS  $08-$20=E8
         BEQ    SDTXCL       ; JUMP IF SO
         CMP    #$EC         ; TEST IF FF  $0C-$20=EC
         BEQ    SDTXFF       ; JUMP IF SO
         JMP    SDTXRT       ; GO RETURN IF UNRECOGNIZABLE CONTROL

SDTXCR:  JSR    CSRCLR       ; CARRIAGE RETURN, FIRST CLEAR CURSOR
         LDA    #0           ; ZERO CURSOR HORIZONTAL POSITION
         STA    CSRX
         JMP    SDTXRT       ; GO SET CURSOR AND RETURN

SDTXCL:  JSR    CSRCLR       ; CURSOR LEFT, FIRST CLEAR CURSOR
         LDA    CSRX         ; GET CURSOR HORIZONTAL POSITION
         CMP    #0           ; TEST IF AGAINST LEFT EDGE
         BEQ    SDTX20       ; SKIP UPDATE IF SO
         DEC    CSRX         ; OTHERWISE DECREMENT CURSOR X POSITION
SDTX20:  JMP    SDTXRT       ; GO SET CURSOR AND RETURN

SDTXFF:  LDA    VMORG        ; FORM FEED, CLEAR SCREEN TO ZEROES
         STA    ADP2+1       ; TRANSFER VISIBLE MEMORY ORIGIN ADDRESS
         LDA    #0           ; TO ADP2
         STA    ADP2
         LDA    #NLOC&$FF    ; SET COUNT OF LOCATIONS TO CLEAR IN DCNT1
         STA    DCNT1
         LDA    #<NLOC
         STA    DCNT1+1
         JSR    FCLR         ; CLEAR THE SCREEN
         LDA    #0
         STA    CSRX         ; PUT CURSOR IN UPPER LEFT CORNER
         STA    CSRY
         JMP    SDTXRT       ; GO SET CURSOR AND RETURN

SDTXLF:  JSR    CSRCLR       ; LINE FEED, FIRST CLEAR CURSOR
         LDA    CSRY         ; GET CURRENT LINE POSITION
         CMP    #NLIN-1      ; TEST IF AY BOTTOM OF SCREEN
         BPL    SDTX40       ; GO SCROLL IF SO
         INC    CSRY         ; INCREMENT LINE NUMBER IF NOT AT BOTTOM
         BNE    SDTXRT       ; GO INSERT CURSOR AND RETURN
SDTX40:  LDA    #0           ; SET UP ADDRESS POINTERS FOR MOVE
         STA    ADP2         ; ADP1 - SOURCE FOR MOVE = FIRST BYTE OF
         LDA    VMORG        ; SECOND LINE OF TEXT
         STA    ADP2+1       ; ADP2 = DESTINATION FOR MOVE = FIRST BYTE
         CLC                 ; IN VISIBLE MEMORY
         ADC    #>(CHHI*40)
         STA    ADP1+1
         LDA    #<(CHHI*40)
         STA    ADP1
         LDA    #<NSCRL      ; SET NUMBER OF LOCATIONS TO MOVE
         STA    DCNT1        ; LOW PART
         LDA    #>NSCRL      ; HIGH PART
         STA    DCNT1+1
         JSR    FMOVE        ; EXECUTE MOVE USING AN OPTIMIZED, HIGH
                             ; SPEED MEMORY MOVE ROUTINE

                             ; CLEAR LAST LINE OF TEXT
         LDA    #<(NLIN-1*CHHI*40)  ; SET ADDRESS POINTER
         STA    ADP2         ; LOW BYTE
         LDA    #>(NLIN-1*CHHI*40)
         CLC
         ADC    VMORG
         STA    ADP2+1       ; HIGH BYTE
         LDA    #<NCLR       ; SET LOW BYTE OF CLEAR COUNT
         STA    DCNT1
         LDA    #>NCLR       ; SET HIGH BYTE OF CLEAR COUNT
         STA    DCNT1+1
         JSR    FCLR         ; CLEAR THE DESIGNATED AREA

;        NO EFFECTIVE CHANGE IN CURSOR POSITION

SDTXRT:  JSR    CSRSET       ; RETURN SEQUENCE, INSERT CURSOR
         PLA                 ; RESTORE REGISTERS FROM THE STACK
         TAY
         PLA
         TAX
         PLA
         RTS                 ; RETURN
         ;  'SUBROUTINES FOR SDTXT'
;        COMPUTE ADDRESS OF BYTE CONTAINING LAST SCAN LINE OF
;        CHARACTER AT CURSOR POSITION
;        ADDRESS = CSRTAD+(CHHI-1)*40   SINCE CHHI IS A CONSTANT 9,
;        (CHHI-1)*40=320
;        BTPT HOLDS BIT ADDRESS, 0=LEFTMOST

CSRBAD:  JSR    CSRTAD       ; COMPUTE ADDRESS OF TOP OF CHARACTER CELL
                             ; FIRST
         LDA    ADP2         ; ADD 320 TO RESULT = 8 SCAN LINES
         CLC
         ADC    #320&$FF
         STA    ADP2
         LDA    ADP2+1
         ADC    #320/256
         STA    ADP2+1
         RTS

;        SET CURSOR AT CURRENT POSITION

CSRSET:  JSR    CSRBAD       ; GET BYTE AND BIT ADDRESS OF CURSOR
         LDA    #$F8         ; DATA = UNDERLINE CURSOR
CSRST1:  JMP    MERGE        ; MERGE CURSOR WITH GRAPHIC MEMORY
                             ; AND RETURN

;        CLEAR CURSOR AT CURRENT POSITION

CSRCLR:  JSR    CSRBAD       ; GET BYTE AND BIT ADDRESS OF CURSOR
         LDA    #0           ; DATA = BLANK DOT ROW
         JMP    MERGE        ; REMOVE DOT ROW FROM GRAPHIC MEMORY
                             ; AND RETURN

;        SHIFT ADP2 LEFT ONE BIT POSITION

SADP2L:  ASL    ADP2
         ROL    ADP2+1
         RTS

;        MOVE DOWN ONE SCAN LINE      DOUBLE ADDS 40 TO ADP2

DN1SCN:  LDA    ADP2         ; ADD 40 TO LOW BYTE
         CLC
         ADC    #40
         STA    ADP2
         LDA    #0           ; EXTEND CARRY TO UPPER BYTE
         ADC    ADP2+1
         STA    ADP2+1
         RTS                 ; RETURN

;        COMPUTE BYTE ADDRESS CONTAINING FIRST SCAN LINE OF
;        CHARACTER AT CURSOR POSITION AND PUT IN ADP2
;        BIT ADDRESS (BIT 0 IS LEFTMOST) AT BTPT
;        BYTE ADDRESS =VMORG*256+CHHI*40*CSRY+INT(CSRX*6/8)
;        SINCE CHHI IS A CONSTANT 9, THEN CHHI*40=360
;        BIT ADDRESS=REM(CSRX*5/8)

CSRTAD:  LDA    #0           ; AERO UPPER ADP2
         STA    ADP2+1
         LDA    CSRY         ; FIRST COMPUTE 360*CSRY
         ASL A               ;   COMPUTE 9*CSRY DIRECTLY IN A
         ASL A
         ASL A
         ADC    CSRY
         STA    ADP2         ;   STORE 9*CSRY IN LOWER ADP2
         JSR    SADP2L       ;   18*CSRY IN ADP2
         JSR    SADP2L       ;   36*CSRY IN ADP2
         ADC    ADP2         ;   ADD IN 9*CSRY TO MAKE 45*CSRY
         STA    ADP2
         LDA    #0
         ADC    ADP2+1
         STA    ADP2+1       ;   45*CSRY IN ADP2
         JSR    SADP2L       ;   90*CSRY IN ADP2
         JSR    SADP2L       ;   180*CSRY IN ADP2
         JSR    SADP2L       ;   360*CSRY IN ADP2
         LDA    CSRX         ; NEXT COMPUTE 6*CSRX WHICH IS A 9 BIT
         ASL A                ; VALUE
         ADC    CSRX
         ASL A
         STA    BTPT         ;   SAVE RESULT TEMPORARILY
         ROR A               ;   DIVIDE BY 8 AND TRUNCATE FOR INT
         LSR A               ;   FUNCTION
         LSR A               ;   NOW HAVE INT(CSRX*6/8)
         CLC                 ; DOUBLE ADD TO ADP2
         ADC    ADP2
         STA    ADP2
         LDA    ADP2+1
         ADC    VMORG        ; ADD IN VMORG*256
         STA    ADP2+1       ; FINISHED WITH ADP2
         LDA    BTPT         ; COMPUTE REM(CSRX*6/8) WHICH IS LOW 3
         AND    #7           ; BITS OF CSRX*6
         STA    BTPT         ; KEEP IN BTPT
         RTS                 ; FINISHED

;        MERGE A ROW OF 5 DOTS WITH GRAPHIC MEMORY STARTING AT BYTE
;        ADDRESS AND BIT NUMBER IN ADP2 AND BTPT
;        5 DOTS TO MERGE LEFT JUSTIFIED IN A
;        PRESERVES X AND Y

MERGE:   STA    MRGT1        ; SAVE INPUT DATA
         TYA                 ; SAVE Y
         PHA
         LDY    BTPT         ; OPEN UP A 5 BIT WINDOW IN GRAPHIC MEMORY
         LDA    MERGT, Y     ; LEFT BITS
         LDY    #0           ; ZERO Y
         AND    (ADP2),Y
         STA    (ADP2),Y
         LDY    BTPT
         LDA    MERGT+8,Y    ; RIGHT BITS
         LDY    #1
         AND    (ADP2),Y
         STA    (ADP2),Y
         LDA    MRGT1        ; SHIFT DATA RIGHT TO LINE UP LEFTMOST
         LDY    BTPT         ; DATA BIT WITH LEFTMOST GRAPHIC FIELD
         BEQ    MERGE2       ; SHIFT BTPT TIMES
MERGE1:  LSR A
         DEY
         BNE    MERGE1
MERGE2:  ORA    (ADP2),Y     ; OVERLAY WITH GRAPHIC MEMORY
         STA    (ADP2),Y
         LDA    #8           ; SHIFT DATA LEFT TO LINE UP RIGHTMOST
         SEC                 ; DATA BIT WITH RIGHTMOST GRAPHIC FIELD
         SBC    BTPT         ; SHIFT (8-BTPT) TIMES
         TAY
         LDA    MRGT1
MERGE3:  ASL A
         DEY
         BNE    MERGE3
         INY
         ORA    (ADP2),Y     ; OVERLAY WITH GRAPHIC MEMORY
         STA    (ADP2),Y
         PLA                 ; RESTORE y
         TAY
         RTS                 ; RETURN

MERGT:   .BYTE  $07,$83,$C1,$E0  ; TABLE OF MASKS FOR OPENING UP
         .BYTE  $F0,$F8,$FC,$FE  ; A 5 BIT WINDOW ANYWHERE
         .BYTE  $FF,$FF,$FF,$FF  ; IN GRAPHIC MEMORY
         .BYTE  $7F,$3F,$1F,$0F

;        FAST MEMORY MOVE ROUTINE
;        ENTER WITH SOURCE ADDRESS IN ADPT1 AND DESTINATION ADDRESS IN
;        ADPT2 AND MOVE COUNT (DOUBLE PRECISION) IN DCNT1.
;        MOVE PROCEEDS FROM LOW TO HIGH ADDRESSES AT APPROXIMATELY 16US
;        PER BYTE.
;        EXIT WITH ADDRESS POINTERS AND COUNT IN UNKNOWN STATE.
;        PRESERVES X AND Y REGISTERS.

FMOVE:   TXA                 ; SAVE X AND Y ON THE STACK
         PHA
         TYA
         PHA
FMOVE1:  DEC    DCNT1+1      ; TEST IF LESS THAN 256 LEFT TO MOVE
         BMI    FMOVE3       ; JUMP TO FINAL MOVE IF SO
         LDY    #0           ; MOVE A BLOCK OF 256 BYTES QUICKLY
FMOVE2:  LDA    (ADP1),Y     ; TWO BYTES AT A TIME
         STA    (ADP2),Y
         INY
         LDA    (ADP1),Y
         STA    (ADP2),Y
         INY
         BNE    FMOVE2       ; CONTINUE UNTIL DONE
         INC    ADP1+1       ; BUMP ADDRESS POINTERS TO NEXT PAGE
         INC    ADP2+1
         JMP    FMOVE1       ; GO MOVE NEXT PAGE
FMOVE3:  LDX    DCNT1        ; GET REMAINING BYTE COUNT INTO X
FMOVE4:  LDA    (ADP1),Y     ; MOVE A BYTE
         STA    (ADP2),Y
         INY
         DEX
         BNE    FMOVE4       ; CONTINUE UNTIL DONE
         PLA                 ; RESTORE INDEX REGISTERS
         TAY
         PLA
         TAX
         RTS                 ; AND RETURN

;        FAST MEMORY CLEAR ROUTINE
;        ENTER WITH ADDRESS OF BLOCK TO CLEAR IN ADP2 AND CLEAR COUNT
;        IN DCNT1.
;        EXIT WITH ADDRESS POINTERS AND COUNT IN UNKNOWN STATE
;        PRESERVES X AND Y REGISTERS

FCLR:    TYA                 ; SAVE Y
         PHA
FCLR1:   LDY    #0
         DEC    DCNT1+1      ; TEST IF LESS THAN 256 LEFT TO MOVE
         BMI    FCLR3        ; JUMP INTO FINAL CLEAR IF SO
         TYA                 ; CLEAR A BLOCK OF 256 QUICKLY
FCLR2:   STA    (ADP2),Y     ; CLEAR A BYTE
         INY
         BNE    FCLR2
         INC    ADP2+1       ; BUMP ADDRESS POINTER TO NEXT PAGE
         JMP    FCLR1        ; GO CLEAR NEXT PAGE
FCLR3:   TYA                 ; CLEAR REMAINING PARTIAL PAGE
FCLR4:   STA    (ADP2),Y
         INY
         DEC    DCNT1
         BNE    FCLR4
         PLA                 ; RESTORE Y
         TAY
         RTS                 ; RETURN
         ;    'CHARACTER FONT TABLE'
;        CHARACTER FONT TABLE
;        ENTRIES IN ORDER STARTING AT ASCII BLANK
;        96 ENTRIES
;        EACH ENTRY CONTAINS 7 BYTES
;        7 BYTES ARE CHARACTER MATRIX, TOP ROW FIRST, LEFTMOST DOT
;        IS LEFTMOST IN BYTE
;        LOWER CASE FONT IS SMALL UPPER CASE, 5 BY 5 MATRIX

CHTB:    .BYTE  $00,$00,$00    		; BLANK
         .BYTE  $00,$00,$00,$00
         .BYTE  $20,$20,$20    		; !
         .BYTE  $20,$20,$00,$20
         .BYTE  $50,$50,$50    		; "
         .BYTE  $00,$00,$00,$00
         .BYTE       $50,$50,$F8    ; #
         .BYTE  $50,$F8,$50,$50
         .BYTE       $20,$78,$A0    ; $
         .BYTE  $70,$28,$F0,$20
         .BYTE       $C8,$C8,$10    ; %
         .BYTE  $20,$40,$98,$98
         .BYTE       $40,$A0,$A0    ; &
         .BYTE  $40,$A8,$90,$68
         .BYTE       $30,$30,$30    ; '
         .BYTE  $00,$00,$00,$00
         .BYTE       $20,$40,$40    ; (
         .BYTE  $40,$40,$40,$20
         .BYTE       $20,$10,$10    ; )
         .BYTE  $10,$10,$10,$20
         .BYTE       $20,$A8,$70    ; *
         .BYTE  $20,$70,$A8,$20
         .BYTE       $00,$20,$20    ; +
         .BYTE  $F8,$20,$20,$00
         .BYTE       $00,$00,$00    ; ,
         .BYTE  $30,$30,$10,$20
         .BYTE       $00,$00,$00    ; -
         .BYTE  $F8,$00,$00,$00
         .BYTE       $00,$00,$00    ; .
         .BYTE  $00,$00,$30,$30
         .BYTE       $08,$08,$10    ; /
         .BYTE  $20,$40,$80,$80
         .BYTE       $60,$90,$90    ; 0
         .BYTE  $90,$90,$90,$60
         .BYTE       $20,$60,$20    ; 1
         .BYTE  $20,$20,$20,$70
         .BYTE       $70,$88,$10    ; 2
         .BYTE  $20,$40,$80,$F8
         .BYTE       $70,$88,$08    ; 3
         .BYTE  $30,$08,$88,$70
         .BYTE       $10,$30,$50    ; 4
         .BYTE  $90,$F8,$10,$10
         .BYTE       $F8,$80,$F0    ; 5
         .BYTE  $08,$08,$08,$F0
         .BYTE       $70,$80,$80    ; 6
         .BYTE  $F0,$88,$88,$70
         .BYTE       $F8,$08,$10    ; 7
         .BYTE  $20,$40,$80,$80
         .BYTE       $70,$88,$88    ; 8
         .BYTE  $70,$88,$88,$70
         .BYTE       $70,$88,$88    ; 9
         .BYTE  $78,$08,$08,$70
         .BYTE       $30,$30,$00    ; :
         .BYTE  $00,$00,$30,$30
         .BYTE       $30,$30,$00    ; ;
         .BYTE  $30,$30,$10,$20
         .BYTE       $10,$20,$40    ; LESS THAN
         .BYTE  $80,$40,$20,$10
         .BYTE       $00,$00,$F8    ; =
         .BYTE  $00,$F8,$00,$00
         .BYTE       $40,$20,$10    ; GREATER THAN
         .BYTE  $08,$10,$20,$40
         .BYTE       $70,$88,$08    ; ?
         .BYTE  $10,$20,$00,$20
         .BYTE       $70,$88,$08    ; @
         .BYTE  $68,$A8,$A8,$D0
         .BYTE       $20,$50,$88    ; A
         .BYTE  $88,$F8,$88,$88
         .BYTE       $F0,$48,$48    ; B
         .BYTE  $70,$48,$48,$F0
         .BYTE       $70,$88,$80    ; C
         .BYTE  $80,$80,$88,$70
         .BYTE       $F0,$48,$48    ; D
         .BYTE  $48,$48,$48,$F0
         .BYTE       $F8,$80,$80    ; E
         .BYTE  $F0,$80,$80,$F8
         .BYTE       $F8,$80,$80    ; F
         .BYTE  $F0,$80,$80,$80
         .BYTE       $70,$88,$80    ; G
         .BYTE  $B8,$88,$88,$70
         .BYTE       $88,$88,$88    ; H
         .BYTE  $F8,$88,$88,$88
         .BYTE       $70,$20,$20    ; I
         .BYTE  $20,$20,$20,$70
         .BYTE       $38,$10,$10    ; J
         .BYTE  $10,$10,$90,$60
         .BYTE       $88,$90,$A0    ; K
         .BYTE  $C0,$A0,$90,$88
         .BYTE       $80,$80,$80    ; L
         .BYTE  $80,$80,$80,$F8
         .BYTE       $88,$D8,$A8    ; M
         .BYTE  $A8,$88,$88,$88
         .BYTE       $88,$88,$C8    ; N
         .BYTE  $A8,$98,$88,$88
         .BYTE       $70,$88,$88    ; O
         .BYTE  $88,$88,$88,$70
         .BYTE       $F0,$88,$88    ; P
         .BYTE  $F0,$80,$80,$80
         .BYTE       $70,$88,$88    ; Q
         .BYTE  $88,$A8,$90,$68
         .BYTE       $F0,$88,$88    ; R
         .BYTE  $F0,$A0,$90,$88
         .BYTE       $78,$80,$80    ; S
         .BYTE  $70,$08,$08,$F0
         .BYTE       $F8,$20,$20    ; T
         .BYTE  $20,$20,$20,$20
         .BYTE       $88,$88,$88    ; U
         .BYTE  $88,$88,$88,$70
         .BYTE       $88,$88,$88    ; V
         .BYTE  $50,$50,$20,$20
         .BYTE       $88,$88,$88    ; W
         .BYTE  $A8,$A8,$D8,$88
         .BYTE       $88,$88,$50    ; X
         .BYTE  $20,$50,$88,$88
         .BYTE       $88,$88,$50    ; Y
         .BYTE  $20,$20,$20,$20
         .BYTE       $F8,$08,$10    ; Z
         .BYTE  $20,$40,$80,$F8
         .BYTE       $70,$40,$40    ; LEFT BRACKET
         .BYTE  $40,$40,$40,$70
         .BYTE       $80,$80,$40    ; BACKSLASH
         .BYTE  $20,$10,$08,$08
         .BYTE       $70,$10,$10    ; RIGHT BRACKET
         .BYTE  $10,$10,$10,$70
         .BYTE       $20,$50,$88    ; CARROT
         .BYTE  $00,$00,$00,$00
         .BYTE       $00,$00,$00    ; UNDERLINE
         .BYTE  $00,$00,$00,$F8
         .BYTE       $C0,$60,$30    ; GRAVE ACCENT
         .BYTE  $00,$00,$00,$00
         .BYTE       $00,$00,$20    ; A (LC)
         .BYTE  $50,$88,$F8,$88
         .BYTE       $00,$00,$F0    ; B (LC)
         .BYTE  $48,$70,$48,$F0
         .BYTE       $00,$00,$78    ; C (LC)
         .BYTE  $80,$80,$80,$78
         .BYTE       $00,$00,$F0    ; D (LC)
         .BYTE  $48,$48,$48,$F0
         .BYTE       $00,$00,$F8    ; E (LC)
         .BYTE  $80,$E0,$80,$F8
         .BYTE       $00,$00,$F8    ; F (LC)
         .BYTE  $80,$E0,$80,$80
         .BYTE       $00,$00,$78    ; G (LC)
         .BYTE  $80,$98,$88,$78
         .BYTE       $00,$00,$88    ; H (LC)
         .BYTE  $88,$F8,$88,$88
         .BYTE       $00,$00,$70    ; I (LC)
         .BYTE  $20,$20,$20,$70
         .BYTE       $00,$00,$38    ; J (LC)
         .BYTE  $10,$10,$50,$20
         .BYTE       $00,$00,$90    ; K (LC)
         .BYTE  $A0,$C0,$A0,$90
         .BYTE       $00,$00,$80    ; L (LC)
         .BYTE  $80,$80,$80,$F8
         .BYTE       $00,$00,$88    ; M (LC)
         .BYTE  $D8,$A8,$88,$88
         .BYTE       $00,$00,$88    ; N (LC)
         .BYTE  $C8,$A8,$98,$88
         .BYTE       $00,$00,$70    ; O (LC)
         .BYTE  $88,$88,$88,$70
         .BYTE       $00,$00,$F0    ; P (LC)
         .BYTE  $88,$F0,$80,$80
         .BYTE       $00,$00,$70    ; Q (LC)
         .BYTE  $88,$A8,$90,$68
         .BYTE       $00,$00,$F0    ; R (LC)
         .BYTE  $88,$F0,$A0,$90
         .BYTE       $00,$00,$78    ; S (LC)
         .BYTE  $80,$70,$08,$F0
         .BYTE       $00,$00,$F8    ; T (LC)
         .BYTE  $20,$20,$20,$20
         .BYTE       $00,$00,$88    ; U (LC)
         .BYTE  $88,$88,$88,$70
         .BYTE       $00,$00,$88    ; V (LC)
         .BYTE  $88,$88,$50,$20
         .BYTE       $00,$00,$88    ; W (LC)
         .BYTE  $88,$A8,$D8,$88
         .BYTE       $00,$00,$88    ; X (LC)
         .BYTE  $50,$20,$50,$88
         .BYTE       $00,$00,$88    ; Y (LC)
         .BYTE  $50,$20,$20,$20
         .BYTE       $00,$00,$F8    ; Z (LC)
         .BYTE  $10,$20,$40,$F8
         .BYTE       $10,$20,$20    ; LEFT BRACE
         .BYTE  $60,$20,$20,$10
         .BYTE       $20,$20,$20    ; VERTICAL BAR
         .BYTE  $20,$20,$20,$20
         .BYTE       $40,$20,$20    ; RIGHT BRACE
         .BYTE  $30,$20,$20,$40
         .BYTE       $10,$A8,$40    ; TILDA
         .BYTE  $00,$00,$00,$00
         .BYTE       $A8,$50,$A8    ; RUBOUT
         .BYTE  $50,$A8,$50,$A8
;
; Start message K-1008 
;
STRMSG:	

		 .literal 	"MTU K-1008 Visable Memory"
		 .byte 		0
;         
		 .END
