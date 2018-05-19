.include "m2560def.inc"

.def row = r16
.def col = r17
.def rmask = r18
.def cmask = r19
.def temp1 = r20
.def temp2 = r21
.def direction = r22
.def currPosition = r23
.def switchCount = r24
.def timeYet = r25

.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F
.equ DOWN = 0x2
.equ UP = 0x1
.equ STALL = 0x0
.equ BOTTOMFLOOR = 0x0
.equ TOPFLOOR = 0x9
.equ YES = 0x1
.equ NO = 0x0

dseg:
floors:
    .byte 10
tempCounter:            ; Counter for how many 128 us has went by
    .byte 2
targetNumInterrupts:
    .byte 2

cseg:
.macro loopThrough  ; @0 - start @1 - destination
    ldi yl, low(floors)     ; Load start of list to y
    ldi yh, high(floors)
    adiw yh:yl, @0
    ldi temp1, @0       ; load parameter 0 to temp1 (our counter)
    ldi temp2, @1       ; Load our number (the floor we want to loop to) we want to temp2
    inc temp2           ; Add 1
loop:
    cp temp1, temp2     ; Compare temp1 and temp2
    breq end            ; If we are 'there' end macro
    adiw yh:yl, 1       ; Add 1 to the index pointer
    inc temp1           ; Add 1 to counter
    rjmp loop

loopThroughEnd:
    nop         ; Shinami
.endmacro

.macro setBit           ; We need to service this floor
     loopThrough BOTTOMFLOOR, @0
     st y, 1
     cpi direction, STALL
     breq setInitialDirection
     rjmp setBitEnd
setInitialDirection:
     cpi currPosition, @0
     brlt setUp
     rjmp setDown
setDown:
     setDirection DOWN
     rjmp setBitEnd
setUp:
     setDirection UP
     rjmp setBitEnd
setBitEnd:
     checkFloors
     nop
.endmacro

.macro clrBit           ; Clear the floor (we dont wanna go there)
    loopThrough BOTTOMFLOOR, @0
    st y, 0
    checkFloors
.endmacro

.macro checkFloors
    mov temp1, direction
    lookAhead
    cpi temp1, UP
    breq checkDown
    cpi temp1, DOWN
    breq checkUp
    rjmp operateElevatorEnd

checkDown:
    cpi direction, DOWN
    breq directionChange
    rjmp operateElevator

checkUp:
    cpi direction, UP
    breq directionChange
    rjmp operateElevator

directionChange:
    cpi switchCount, 1
    breq operateElevatorClear
    inc switchCount
    checkFloors

operateElevator:
    moveElevator
    rjmp operateElevatorEnd

operateElevatorClear:
    clr switchCount
    clr direction     ; direction should be zero if there is nothing above or below the elevator
    rjmp operateElevatorEnd

operateElevatorEnd:
    nop

.endmacro

.macro moveElevator
    clr temp1
    cpi direction, UP
    breq moveAhead
    cpi direction, DOWN
    breq moveBehind
    rjmp moveEnd
moveAhead:
    cpi currPosition, TOPFLOOR
    breq moveEnd
    mov temp1, currPosition
    inc currPosition
    loopThrough temp1, currPosition
    ld temp2, y
    cpi temp2, 1
    breq moveEnd
    rjmp moveAhead
moveBehind:
    cpi currPosition, BOTTOMFLOOR
    breq moveEnd
    mov temp1, currPosition
    dec currPosition
    loopThrough temp1, currPosition
    ld temp2, y
    cpi temp2, 1
    breq moveEnd
    rjmp moveBehind
moveEnd:
    clrBit currPosition
    nop
.endmacro

.macro setDirection ; @0 - destination
    cpi currPosition, @0
    brlt down
    ldi direction, UP
    rjmp setDirectionEnd
down:
    ldi direction, DOWN
    rjmp setDirectionEnd
setDirectionEnd:
    nop
.endmacro

.macro lookAhead ; See if there are floors waiting in the direction we are facing
    clr temp1
    cpi direction, UP
    breq loopAhead
    cpi direction, DOWN
    breq loopBehind
loopAhead:
    mov temp2, currPosition
iLikeNerds:
    cpi temp2, TOPFLOOR
    breq switch
    mov temp1, temp2
    inc temp1
    loopThrough temp2, temp1
    ld temp2, y
    cpi temp2, 1
    breq lookAheadEnd
    mov temp2, temp1
    rjmp iLikeNerds
iLikeCocoPops:
    cpi temp2, BOTTOMFLOOR
    breq switch
    mov temp1, temp2
    dec temp1
    loopThrough temp2, temp1
    ld temp2, y
    cpi temp2, 1
    breq lookAheadEnd
    mov temp2, temp1
    rjmp iLikeCocoPops
loopBehind:
    mov temp2, currPosition
    rjmp iLikeCocoPops
switch:
    cpi direction, UP
    breq UPToDOWN
    ldi direction, UP
    rjmp lookAheadEnd
UPToDOWN:
    ldi direction, DOWN
lookAheadEnd:
    nop
.endmacro

.macro clrBuilding ; Goes through floor register, sets each bit to 0
    push temp1
    ldi yh, high(floors)    ; load start of list to y
    ldi yl, low(floors)
    ldi temp1, 9            ; counter for # floors
clrLoop:
    cpi temp1, 9
    breq floorCleared
    st y+, 0
    dec temp1
    brne clrLoop
floorCleared:
    pop temp1
.endmacro:


.macro displayFloorLED ; Light updates as we move up to a floor (ie. at floor 5 we have 6 leds switched on)
    inc currPosition        ; Increment currPosition so that we will also display floor 0
    clr temp1
    clr temp2
    ldi temp2, 1
    cpi currPosition, 9     ; If it is floor 8 we have turn on the top two bits of the led bar which is not in PORTC
    breq floor8
    cpi currPosition, 10    ; If it is floor 9 we have turn on the top two bits of the led bar which is not in PORTC
    breq floor9
checkLoop:  ; This is for floors 0-7
    cp temp1, currPosition  ; Check if temp1 has reached the floor we are at now
    breq display            ; If it is then we can display it
    lsl temp2               ; Otherwise left shift temp2 ie. 0b00000001 becomes 0b00000010
    inc temp2               ; Add 1 so 0b00000010 becomes 0b00000011 this way we turn on the bottomn two lights
    inc temp1
    rjmp checkLoop
floor8:
    ldi temp1, 0b00000001
    rjmp topTwoBits
floor9:
    ldi temp1, 0b00000011
    rjmp topTwoBits
topTwoBits:
    out PORTG, temp1        ; PORTG has the top two bits in the ledBar
    ldi temp2, 0b11111111   ; Also have to turn on every other light under those two bits
display:
    out PORTC, temp2
    dec currPosition        ; Decrement currPosition so it is so 0-9 not 1-10
.endmacro

.macro clear
    ldi YL, low(@0)     ; load the memory address to Y
    ldi YH, high(@0)
    clr temp            ; set temp to zero by clearing
    st Y+, temp         ; set the two bytes in SRAM to 0
    st Y, temp
.endmacro

.macro setTargetTime ; takes in the number of interrupts we want @0
    push temp1
    push temp2
    clr temp1
    clr temp2
    adiw temp2:temp1, @0
    sts targetNumInterrupts, temp1
    sts targetNumInterrupts + 1, temp2
    pop temp1
    pop temp2
.endmacro

.org 0x0000
	jmp RESET
.org INT0addr
    jmp DEFAULT
.org INT1addr
    jmp DEFAULT
.org OVF0addr
    jmp Timer0OVF

DEFAULT:
    reti

RESET:
	ldi temp1, low(RAMEND)
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	ldi temp1, PORTLDIR        ; Set port L direction to output for Pin 7:4 and input for Pin 3:0
	sts DDRL, temp1
	ser temp1
	out DDRC, temp1            ; Set port C to output (LEDs)
    out DDRG, temp1
	clr temp1
	out PORTC, temp1           ; Turn off the leds
    out PORTG, temp1

    ldi temp1, (1 << ISC01) | (1 << ISC11)   ; Set INT0 and INT1 to accept falling edge signal
    sts EICRA, temp1                         ; Store temp in EICRA (used for INT0 and INT1)

    in temp1, EIMSK                          ; Get the current EIMSK
    ori temp1, (1 << INT0) | (1 << INT1)     ; Logical OR to enable INT1 and INT0
    out EIMSK, temp1                         ; Output temp to EIMSK

    ldi temp1, 0b00000000                    ; Set the mode of operation of the timer to Normal Mode (counting direction is up)
    out TCCR0A, temp1                        ; Timer will roll over when it passes its maximum 8-bit value
    ldi temp1, 0b00000010                    ; Sets the prescaling value to 8 but setting CS01 (clock select bit 1) to 1
    out TCCR0B, temp1
    ldi temp1, 1<<TOIE0                      ; Set TOIE0 (defined to be 0) to enable the Timer 0 Overflow Interrupt
    sts TIMSK0, temp1

    sei                                      ; Global Interrupt Enable

    clr currPosition           ; Clear current position when program resets
    clr switchCount

main:
	ldi cmask, INITCOLMASK
	clr col
    rjmp columnLoop


columnLoop:
	cpi col, 4         ; Check if we have checked all columns
	breq main          ; If we have checked then we go back to main and repeat
	sts PORTL, cmask   ; Otherwise output the mask to PORTL

	ldi temp1, 0xFF    ; Implement a delay

delay:
	dec temp1
	brne delay

	lds temp1, PINL        ; Load PINL to temp1
	andi temp1, ROWMASK    ; Use logical and with ROWMASK
	cpi temp1, 0xF         ; If there is no buttons pressed then we go to next column
	breq nextCol

	ldi rmask, INITROWMASK ; Otherwise load initial mask to rmask
	clr row

rowLoop:
	cpi row, 4             ; Check if we have checked all rows
	breq nextCol           ; If we have then go to the next column
	mov temp2, temp1       ; Otherwise move temp1 to temp2
	and temp2, rmask       ; Use logical and with rmaks
	breq convert           ; If they are equal meaning we have got the row we want then go to convert
	inc row                ; Otherwise increment row
	lsl rmask              ; Left shift rmask
	jmp rowLoop

nextCol:
	lsl cmask              ; Left shift the cmask
	inc cmask              ; increment cmask
	inc col                ; Increase col
	jmp columnLoop

convert:
	cpi col, 3             ; Check if col is 3 which means it is letter (we don't deal with it)
	breq main

	cpi row, 3             ; Check if row is 3
	breq possiblyZero

	mov temp1, row         ; Convert row and col to the number of the button
	lsl temp1              ; number = 3 * row + col + 1
	add temp1, row
	add temp1, col
	inc temp1
    setBit temp1
	jmp display

possiblyZero:
	cpi col, 1             ; Check if col is 1
	brne main              ; If not then we go back to main
	clr temp1

display:
	out PORTC, temp1       ; Output to LED
	jmp main

Timer0OVF:
    push temp1
    push temp2
    push r12
    push r13

    lds r12, tempCounter            ; Load the first byte of tempCounter into r24
    lds r13, tempCounter + 1        ; Load the second byte of tempCounter into r25

    adiw r13:r12, 1                 ; Add 1 to the word (r25:r24)


    cpi r12, low(targetNumInterrupts)              ; compare the low(7812) with r24
    ldi temp1, high(targetNumInterrupts)
    cpc r13, temp1                   ; Compare with carry
    brne notTimeYet

clearCounter:
    clear tempCounter               ; Clear tempCounter as it has been one second so we count again
    ldi timeYet, YES
    rjmp end

notTimeYet:
    sts tempCounter, r12            ; It has not been one second yet so we just store the new time
    sts tempCounter + 1, r13

end:
    pop r13                         ; Pop off all conflict registers
    pop r12
    pop temp2
    pop temp1
    reti
