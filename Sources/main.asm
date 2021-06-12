;*******************************************************
;* CMPEN 472, HW11 sample 11 program,  converting analog 
;*            signal to digital data, MC9S12C128 Program
;*
;* Date:	          Apr. 22, 2020
;*
;* Programmer:     Samuel Johnson
;*
;* Company:        The Pennsylvania State University
;*                 Department of Computer Science and Engineering
;*
;* Date: 11/02/2018     Kyusun Choi
;* Date: 04/08/2020     Kyusun Choi   Updated for CodeWarrior Debug/Simulator
;* Date: 11/06/2020     Kyusun Choi
;* Date: 04/14/2020     Kyusun Choi
;* Date: 04/22/2021     Samuel Johnson
;*
;* Term ATD = ADC = Analog-to-Digital Converter
;* 
;*   Program for ADC Testing outputs information to the terminal
;*   based on the signal running in CH7. The ADC measures each data
;*   point on an interrupt which is then printed by a printer function.
;*
;* For SIMULATOR:
;*   Serial communication at fast rate (2M baud).
;*   Typewriter program, but when 'enter' key hit, ADC acquired 
;*   number printed on the terminal window, as a 1024 decimal number.
;*   Single AD conversion, 8bit, right justified.
;*
;*******************************************************
; export symbols
            XDEF      Entry       ; export 'Entry' symbol
            ABSENTRY  Entry       ; for assembly entry point
  
; symbols/addresses

PORTB       EQU  $0001        ; I/O port B, 7 segment display at bit 7 to 4
DDRB        EQU  $0003        ; I/O port B data direction control

ATDCTL2     EQU  $0082            ; Analog-to-Digital Converter (ADC) registers
ATDCTL3     EQU  $0083
ATDCTL4     EQU  $0084
ATDCTL5     EQU  $0085
ATDSTAT0    EQU  $0086
ATDDR0H     EQU  $0090
ATDDR0L     EQU  $0091
ATDDR7H     EQU  $009e
ATDDR7L     EQU  $009f

SCIBDH      EQU  $00c8            ; Serial port (SCI) Baud Rate Register H
SCIBDL      EQU  $00C9            ; Serial port (SCI) Baud Register L
SCICR2      EQU  $00CB            ; Serial port (SCI) Control Register 2
SCISR1      EQU  $00cc            ; Serial port (SCI) Status Register 1
SCIDRL      EQU  $00cf            ; Serial port (SCI) Data Register

TIOS        EQU  $0040            ; Timer Input Capture (IC) or Output Compare (OC) select
TIE         EQU  $004C            ; Timer interrupt enable register
TCNTH       EQU  $0044            ; Timer free runing main counter
TSCR1       EQU  $0046            ; Timer system control 1
TSCR2       EQU  $004D            ; Timer system control 2
TFLG1       EQU  $004E            ; Timer interrupt flag 1
TC2H        EQU  $0054            ; Timer channel 2 register
;*   CodeWarrior project MUST specify MC9S12C32 chip for the terminal simulation to work.

CR          equ  $0d              ; carriage return, ASCII 'Return' key
LF          equ  $0a              ; line feed, ASCII 'next line' character

intct       equ         79           ; my PC simulation works,  79 => 1 sec approximately
;intct       equ         7999         ; For 1 sec., interrupt count is 8000
; 125usec * 8000 = 1 sec,  0 to 7999 count is 8000
; For simulation, reduce this number for faster 1 sec. timing
; If interrupt count less than 8000, then not 1 sec yet.
;    no need to update display.

;*******************************************************
; variable/data section
            ORG  $3000               ; RAMStart defined as $3000
ATDdone     DS.B    1                ; ADC finish indicator, 1 = ATD finished
ctr125u     DS.W    1                ; 16bit interrupt counter for 125 uSec. of time
iYes        DS.B    1

TempMem     DS.B    5                ; Reserved for use in print subroutine


;*******************************************************
; interrupt vector section

;            ORG     $3FEA            ; Timer channel 2 interrupt vector setup, HC12 board
            ORG     $FFEA            ; Timer channel 2 interrupt vector setup, simulator
            DC.W    oc2isr

;*******************************************************
; code section
            ORG  $3100
Entry
            LDS   #Entry           ; initialize the stack pointer

            ldd   #$0001           ; For SIMULATION, Set SCI Baud Register = $0001 => 2M baud at 24MHz
            std   SCIBDH           ; SCI port baud rate change
            ldaa  #$0C             ; Enable SCI port Tx and Rx units
            staa  SCICR2           ; disable SCI interrupts

; ATD initialization
            LDAA    #%11000000       ; Turn ON ADC, clear flags, Disable ATD interrupt
            STAA    ATDCTL2
            LDAA    #%00001000       ; Single conversion per sequence, no FIFO
            STAA    ATDCTL3
            LDAA    #%10000111       ; 8bit, ADCLK=24MHz/16=1.5MHz, sampling time=2*(1/ADCLK)
            STAA    ATDCTL4          ; for SIMULATION

            LDAA    #%11111111   ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA    DDRB         ; as output
            STAA    PORTB        ; set all bits of PORTB, initialize


            ldx     #msg1            ; print the first message, 'Hello'
            jsr     printmsg
            jsr     nextline
            ldx     #msg2            ; print the second message, user instruction
            jsr     printmsg
            jsr     nextline
                                      
                                        

looop       jsr     getchar            ; type writer - check the key board
            cmpa    #$00               ;  if nothing typed, keep checking
            beq     looop
                                       ;  otherwise - what is typed on key board
            jsr     putchar            ; is displayed on the terminal window - echo print

            staa    PORTB              ; show the character on PORTB

            cmpa    #CR
            bne     looop              ; if Enter/Return key is pressed, move the
            ldaa    #LF                ; cursor to next line
            jsr     putchar
looop2            
            ldx     #msg4              ; give the user the prompt for pressing switch 0 
            jsr     printmsg
            jsr     nextline
            
            jsr     delay10usec        ; Wait for the characters to finish sending
            ldaa    #%00000000         ; load a with all 0s
            staa    PORTB              ; Clear port B
            
            
swCheck                                ; Start the adc conversion
            LDAA    #%10000111         ; right justified, unsigned, single conversion,
            STAA    ATDCTL5            ; single channel, CHANNEL 7, start the conversion
            ldaa    PORTB              ; Read SW0 at PORTB
            anda    #%00000001         ; Isolate bit 0
            beq     swCheck            ; If it is 0, loop back up to keep checking
            
            
            bclr    iYes,%11111111
            jsr     StartTimer2oc
            
            ldx     #0                 ; Setup the timer
            stx     ctr125u            
loop1024
            ldx     ctr125u
            cpx     #1024
            beq     transmitEnd
            
            brclr   iYes,%11111111,loop1024
            bclr    iYes,%11111111
            
            ldab    ATDDR0L               ; Load the current timer counter from the interupt service routine
            jsr     printNum           ; Prints out the number
            jsr     nextline           ; Go to the next line
            ldx     ctr125u            ; Increments the counter
            inx
            stx     ctr125u
                                       ; Start the next conversion
            LDAA    #%10000111         ; right justified, unsigned, single conversion,
            STAA    ATDCTL5            ; single channel, CHANNEL 7, start the conversion
            bra     loop1024
            
transmitEnd
            sei                        ; Stop interrupt OC2
            jsr     nextline           ; make a line break
            ldx     #msg3              ; print the ending messages
            jsr     printmsg
            jsr     nextline
            bra     looop2             ; jump back to checking for sw being pressed


;***********Timer OC2 interrupt service routine***************
oc2isr
            ldd   #3000              ; 125usec with (24MHz/1 clock)
            addd  TC2H               ;    for next interrupt
            std   TC2H               ; 
            bset  TFLG1,%00000100    ; clear timer CH2 interrupt flag, not needed if fast clear enabled
            bset  iYes, %11111111    ; Set the iYes flag every time the OC2 occurs
oc2done     RTI
;***********end of Timer OC2 interrupt service routine********

;*******************************************************
; subroutine section  

;***************StartTimer2oc************************
;* Program: Start the timer interrupt, timer channel 2 output compare
;* Input:   Constants - channel 2 output compare, 125usec at 24MHz
;* Output:  None, only the timer interrupt
;* Registers modified: D used and CCR modified
;* Algorithm:
;             initialize TIOS, TIE, TSCR1, TSCR2, TC2H, and TFLG1
;**********************************************
StartTimer2oc
            PSHD
            LDAA   #%00000100
            STAA   TIOS              ; set CH2 Output Compare
            STAA   TIE               ; set CH2 interrupt Enable
            LDAA   #%10000000        ; enable timer, Fast Flag Clear not set
            STAA   TSCR1
            LDAA   #%00000000        ; TOI Off, TCRE Off, TCLK = BCLK/1
            STAA   TSCR2             ;   not needed if started from reset

            LDD     #3000            ; 125usec with (24MHz/1 clock)
            ADDD    TCNTH            ;    for first interrupt
            STD     TC2H             ; 

            PULD
            BSET   TFLG1,%00000100   ; initial Timer CH2 interrupt flag Clear, not needed if fast clear set
            CLI                      ; enable interrupt
            RTS
;***************end of StartTimer2oc*****************

;***************printNum**********************
;* Program: Prints out the counter value to terminal
;* Input:   number in register D
;* Output:  Prints the number in decimal to terminal
;* Registers modified: 
;* Algorithm:
;    find each of the decimal places and converts them to ascii then calls putchar
;**********************************************
printNum
               pshx
               pshy
               
               ldaa     #0
               ldx      #$0A                  ; Load 10 into x
               ldy      #TempMem              ; Load the memory location of decimal val into y
               
               idiv                           ; D/X with remainder in B (this is last bit)
               stab     1, Y+                 ; Store the remainder into temp mem
               tfr      X, D                  ; Put whats left into D
               ldx      #$0A                  ; Load 10 into x
               
               idiv                           ; D/X with remainder in B (this is last bit)
               stab     1, Y+                 ; Store the remainder into temp mem
               tfr      X, D                  ; Put whats left into D
               ldx      #$0A                  ; Load 10 into x
               
               idiv                           ; D/X with remainder in B (this is last bit)
               stab     1, Y+                 ; Store the remainder into temp mem
               tfr      X, D                  ; Put whats left into D
               ldx      #$0A                  ; Load 10 into x
               
               idiv                           ; D/X with remainder in B (this is last bit)
               stab     1, Y+                 ; Store the remainder into temp mem
               tfr      X, D                  ; Put whats left into D
               ldx      #$0A                  ; Load 10 into x
               
               idiv                           ; D/X with remainder in B (this is last bit)
               stab     Y                     ; Store the remainder into temp mem
               tfr      X, D                  ; Put whats left into D
               ldx      #$0A                  ; Load 10 into x
               
               ; Now we go back and print in reverse order
               
               ldaa     #$30
               adda     1, Y-                 ; convert each character into ascii
               cmpa     #$30
               beq      print2
               ldab     #1                    ; load b with 1 to indicate a non leading 0
               jsr      putchar
print2               
               ldaa     #$30
               adda     1, Y-                 ; convert each character into ascii
               cmpb     #1
               beq      skip2                 ; If there is a non 0 leading number, we prints 0s
               cmpa     #$30
               beq      print3
               ldab     #1                    ; load b with 1 to indicate a non leading 0
skip2          jsr      putchar
print3               
               ldaa     #$30
               adda     1, Y-                 ; convert each character into ascii
               cmpb     #1
               beq      skip3                 ; If there is a non 0 leading number, we prints 0s
               cmpa     #$30
               beq      print4
               ldab     #1                    ; load b with 1 to indicate a non leading 0
skip3          jsr      putchar
print4               
               ldaa     #$30
               adda     1, Y-                 ; convert each character into ascii
               cmpb     #1
               beq      skip4                 ; If there is a non 0 leading number, we prints 0s
               cmpa     #$30
               beq      print5
               ldab     #1                    ; load b with 1 to indicate a non leading 0
skip4          jsr      putchar
print5               
               ldaa     #$30
               adda     1, Y-                 ; convert each character into ascii
               jsr      putchar
               
               puly
               pulx    
               rts
            
;***************end of printNum***************   

;**********************************************************************
; delay10usec subroutine
;
; This subroutine delays the progression of the program by 1 second
;
; Input:  a 16bit counter number in 'Counter2'
; Outout: time delay, cpu cycle wasted
; Registers in use: Y register, as counter
; Memory Locations in use: a 16bit input number at 'Counter2'
;
; Comments: The delay according to ideal calculations should be 59
;             but in order for my program to work from 0 - 100 within
;             4 seconds, I had to cut down on that number to 20

delay10usec
            PSHB                  ; Save B register
            LDAB     #200         ; 200 loops should be the perfect number for 10 ms
           
dly10Loop   
            SUBB     #1           ; B = B - 1
            BNE      dly10Loop    ; Branch if B != 0
            
            PULB                  ; Restore B register
            RTS                   ; return
;****************end of delay10usec***************      

;***********printHx***************************
; prinHx: print the content of accumulator A in Hex on SCI port
printHx     psha
            lsra
            lsra
            lsra
            lsra
            cmpa   #$09
            bhi    alpha1
            adda   #$30
            jsr    putchar
            bra    low4bits
alpha1      adda   #$37
            jsr    putchar            
low4bits    pula
            anda   #$0f
            cmpa   #$09
            bhi    alpha2
            adda   #$30
            jsr    putchar
            rts
alpha2      adda   #$37
            jsr    putchar
            rts
;***********end of printhx***************************
 
;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* C
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                jsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
                pula
                rts
;***********end of printmsg********************

;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                       ; send a character
            rts
;***************end of putchar*****************

;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline    ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            rts
;****************end of nextline***************

*
* Add any subroutines here
*


msg1        DC.B    'Hello, this is a 1024 data transmit program.', $00
msg2        DC.B    'Press Enter to start.', $00
msg3        DC.B    '> Done, you may close the file.', $00   ; error message
msg4        DC.B    '> Press SW0 for 1024 data trasmit', $00      ; command prompt


            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled