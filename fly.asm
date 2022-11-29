;;;;;;;;;;;;;;;;;;;;;
;;; ZX81 fly low hit hard game by Adrian Pilkington
;;; uses simple charcter/block graphics and requires 16K ZX81
;;;;;;;;;;;;;;;;;;;;;

;;todo
;;       
;;       make also work for joystick inputs
;;       include fire button command and make ship show fireing cannon when pressed
;;       add enemys, both ground luanched vertically and from right in air
;;       add collision detection and end of game
;;
;;      BUGS  : the "ship" character flickers but only when not on the top line, probably due to the "scrolling left the readd ship"

#include "zx81defs.asm" ;; https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
;EQUs for ROM routines
#include "zx81rom.asm"
;ZX81 char codes/how to survive without ASCII
#include "charcodes.asm"
;system variables
#include "zx81sys.asm"

;the standard REM statement that will contain our 'hex' code
#include "line1.asm"

; these variables need converting to screen addresses for zx81
; problem with zx81 is the screen display D_FILE memory address changes with size of basic program 
; see https://www.sinclairzxworld.com/viewtopic.php?t=3919
; (the asm here is converted to one line of basic)
#define ROWS_IN_SCREEN 24
#define COL_IN_SCREEN 32
#define RANDOM_BYTES_MEM_LOCATION 2000
;((32*23)-1)
#define SHIP_SCREEN_MEM_START_OFFSET  265   ; this is just a bit higher than the middle of the screen on the left

;D_FILE is location of screen memory (which moves depending on length of basic, but should be fixed after program is loaded
; probably should run some code to detect if this is 1K or 16K as well, or just have 2 verisons 1K and 16K
#define D_FILE 16396

;ship is made up of two blocks
#define SHIP_CHARACTER_CODE 130   ; right facing corner  
#define ENEMY_CHARACTER_CODE 6
#define GROUND_CHARACTER_CODE 61
#define GROUND_CHARACTER_CODE_2 189

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
#define KEYBOARD_READ_PORT_A_TO_G	$FD
#define KEYBOARD_READ_PORT_ENTER_TO_H $BF 
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 

	jp setHighScoreZero

var_ship_pos 
	DEFB 0,0
to_print_mem
	DEFB 0,0
groundCharacter
	DEFB 0
groundLevelMemoryLocationNow
    DEFB 0,0
row_counter    
    DEFB 0,0
col_counter        
    DEFB 0,0
screen_content_at_current
    DEFB 0    
enemyStartRowPosition
    DEFB 0    
currentRowOffset    
    DEFB 0,0
vertPosition    
    DEFB 0,0
numberOfGroundBlocksMoved    
    DEFB 0,0
crash_message_txt
	DEFB	_G,_A,_M,_E,__,_O,_V,_E,_R,$ff	
title_screen_txt
	DEFB	_Z,_X,_8,_1,__,_F,_L,_Y,__,_L,_O,_W,26,__,_H,_I,_T,__,_H,_A,_R,_D,$ff
keys_screen_txt
	DEFB	_S,__,_S,_T,_A,_R,_T,26,__,_X,__,_U,_P,__,26,_N,__,_D,_O,_W,_N,__,_S,_P,_A,_C,_E,__,_F,_I,_R,_E,$ff
keys_screen_txt_2
	DEFB	$10,_O,_R,__,_J,_O,_Y,_S,_T,_I,_C,_K,__,_P,_R,_E,_S,_S,__,_J,__,_F,_I,_R,_E,$11,$ff    
using_joystick_string
	DEFB	$10,_J,_O,_Y,_S,_T,_I,_C,_K,__,_S,_E,_T,$11,$ff        
not_using_joystick_string
	DEFB    $10,_U,_S,_I,_N,_G,__,_K,_E,_Y,_S,$11,$ff        
last_Score_txt
	DEFB	21,21,21,21,_L,_A,_S,_T,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff	
high_Score_txt
	DEFB	21,21,21,21,_H,_I,_G,_H,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff		
title_screen_edge		
	DEFB	18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,$ff
test_str		
	DEFB	6,$ff			
missileFired    
    DEFB 0
score_mem_tens
	DEFB 0
score_mem_hund
	DEFB 0
score_mem_thou
	DEFB 0
high_score_mem_tens
	DEFB 0
high_score_mem_hund
	DEFB 0		
last_score_mem_tens
	DEFB 0
last_score_mem_hund
	DEFB 0			
speedUpLevelCounter	
	DEFB 0,0
credits_and_version_2
	DEFB _B,_Y,__,_A,__,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,$ff
var_keys_or_joystick
	DEFB 0	
to_print .equ to_print_mem ;use printByte16

;; note on the zx81 display 
; from previous crashes and experimenting with printing characters to the screen;; 
; and also some forums, it's clear that the zx81 has 32 column* 24 rows of printable/addressable
; screen area, but at the end of each row is is a chr$127, which if overritten
;; can cause unpredictable behavoir and system instabiltiy . It also menas calculating 
;; addresses/offsets to print to is not as straightforward as say c64
;; printing to very last column and row, is 32col * 24row + (24"end of lines" - 1)
;; printing to [row][col], use (row * 33) + col, 
;; (row is 0 to 23 for addressing purposes, and column 1 to 32)
;;
;; 1k is different to 16K, on 1K system saves space by putting "end of row markers" chr$127
;; on every line until there is something else on it. 16K preallocates whole display
;; 16K zx81 offsets from D_FILE
;; 1  = top row, first column 
;; 32 = top right, last column
;; 760 = bottom row, first column
;; 791 = bottom row, last column
	
;set b to row, c to col	
printByte 		;;http://swensont.epizy.com/ZX81Assembly.pdf?i=1
    PUSH AF ;store the original value of A for later

    CALL PRINTAT ;
    POP AF 
    PUSH AF ;store the original value of A for later
    AND $F0 ; isolate the first digit
    RRA
    RRA
    RRA
    RRA
    ADD A,$1C ; add 28 to the character code
    CALL PRINT
    POP AF ; retrieve original value of A
    AND $0F ; isolate the second digit
    ADD A,$1C ; add 28 to the character code
    CALL PRINT
    RET

introWaitLoop
    ld bc,$00ff ;max waiting time
introWaitLoop_1
    dec bc
    ld a,b
    or c
    jr nz, introWaitLoop_1
    jp read_start_key

	
setHighScoreZero
    xor a
    ld (high_score_mem_tens), a
    ld (high_score_mem_hund), a
    ld (last_score_mem_tens), a
    ld (last_score_mem_hund), a	
	
	
intro_title
    call CLS	
    ld bc,1
    ld de,title_screen_edge
    call printstring	
    ld bc,34
    ld de,title_screen_edge
    call printstring		
    ld bc,105
    ld de,title_screen_txt
    call printstring
    ld bc,199
    ld de,keys_screen_txt
    call printstring	

    ld bc,235
    ld de,keys_screen_txt_2
    call printstring
    ld bc,436
    ld de,last_Score_txt
    call printstring	
    ld b, 14			; b is row to print in
    ld c, 13			; c is column
    ld a, (last_score_mem_hund) ; load hundreds
    call printByte    
    ld b, 14			; b is row to print in
    ld c, 15			; c is column
    ld a, (last_score_mem_tens) ; load tens		
    call printByte	
    ld bc,570	
    ld de,credits_and_version_2
    call printstring		
    ld bc,727
    ld de,title_screen_edge
    call printstring		
    ld bc,760
    ld de,title_screen_edge
    call printstring	
    ld c, $1f
    xor a
    out (c),a
    ld a, 1 
    ld (var_keys_or_joystick), a    

read_start_key
    ; read fire button to start , works on real zx81 but not on EightyOne emulator
    ; comment out for version on github until work out a way of stopping EightyOne
    ; always returning bit set
    ld a, KEYBOARD_READ_PORT_ENTER_TO_H	
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 3, a									; check  key pressed
    jp nz, carryOnCheckingStart

    xor a
    ld (var_keys_or_joystick), a    ; zero the flag for using joystick, so use keys
    ld bc,298
    ld de,using_joystick_string
    call printstring    

    
carryOnCheckingStart 
    ld a, (var_keys_or_joystick)    ; keys = 1
    and 1
    jp nz, dont_check_fire_button

    ld b, 00010000b   ; 16 decimal, fire button
    in a,($1F)       ; a now has the input byte from the port 1f (which is the joystick port)
    and b 
    jp nz, main
    
dont_check_fire_button    

    ld a, KEYBOARD_READ_PORT_A_TO_G	
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 1, a									; check S key pressed
    jp nz, introWaitLoop
    ; this means that they pressed  s to start so only use keys
    ld a, 1
    ld (var_keys_or_joystick), a   ; keys = 1
main
    call CLS

    xor a 						; initialise score to zero, and 0 results in a equal to zero
    ld (score_mem_tens),a	
    ld (score_mem_hund),a
    ld (score_mem_thou),a	    

    ld bc, $00ff					; set initial difficulty
    ld (speedUpLevelCounter), bc
       
    ;ld b, COL_IN_SCREEN         ; initialise loop counter (used by djnz)       
    ld hl, 0
    ld (numberOfGroundBlocksMoved),hl

    ld de, groundLevelMemory
    ld (groundLevelMemoryLocationNow), de
    ld b, 31       ; initialise loop counter (used by djnz)       

    ld a, 0
    ld (missileFired), a

    
initialiseGround        
    ;; we use a pre initialise memory blcok to control the height of each 
    ;; ground level. The far right column is drawn first then "scrolled" left 
    ;; on each update of the game loop. The idea is to have multiple banks of
    ;; landscape which can be used as different "game levels"        


    ;; the aim of the next 8 lines is to get register "hl" to have the 
    ;; thing that register "de" points to, but due to limits of z80, 
    ;; have to use register indirect via two ld a, (de) !!!
    ;; unless I'm missing something!! :-/
    ld de, (groundLevelMemoryLocationNow)
    ld a,(de)      
    ld l, a        
    inc de         
    ld a, (de)
    ld h, a    
    inc de   ; increment again to get to the next location
    ld (groundLevelMemoryLocationNow), de

    push hl         ;; you can't just do "ld hl, de", that instruction doesn't exist, 
    ;; would be too easy, so have to use stack "push and pop"
    pop de

    ld hl,(D_FILE) 

    add hl,de       ;; add the start of display memory to the offset 
    ;; (note the groundLevelMemory already has the plus 1 added to push one after D_FILE)

    ld a, GROUND_CHARACTER_CODE
    ld (hl),a    

    djnz initialiseGround    ; djnz decrements b and jumps if not zero

    ;;;;;;;;;;;;;;;initialise ship
    ld hl,(D_FILE) 
    ld de, SHIP_SCREEN_MEM_START_OFFSET
    add hl, de	
    ld a,SHIP_CHARACTER_CODE 
    ld (hl),a
    ld (var_ship_pos),hl ;save ship posn       
    
    ld a, 10 
    ld (vertPosition),a
    
mainGameLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scroll ground left           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ld b, 22
    ld hl,(D_FILE)          
    inc hl
    push hl
    pop de
screenScrollLeftRowLoop
    push bc       
    
    ;; this is the super fast way to copy  the columns to the left
    ld bc, 31   
    inc hl    
    ldir            ;; this is where the magic happens!!! 
                    ;; LDIR : Repeats LDI (LD (DE),(HL), then increments DE, HL, and decrements BC) until BC=0.
    inc hl
    push hl
    pop de
    pop bc
    djnz screenScrollLeftRowLoop


    ; as we scrolled whole screen left, the ship has to be moved right one column to maintain it's position
    ;; even without user input
    ld hl,(var_ship_pos) ; var_ship_pos already has the D_FILE offset added    
    ld a,SHIP_CHARACTER_CODE 
    ld (hl),a              

    ;; before we add the next ground block at the far right we need to zero the column to preven block 
    ;;drag over all the columns
    ld b, 20     
    ld hl,(D_FILE)
    inc hl
    ld de, 31   
    add hl, de	
    
zeroLastColumnLoop    	
    xor a    ; character zero blank space
    ld (hl),a
    ld de, 33
    add hl,de
    djnz zeroLastColumnLoop    


    ;;; now reuse some of the code from initialising the gorund in a one shot to draw next ground
    ld de, (groundLevelMemoryLocationNow)
    ld a,(de)      
    ld l, a        
    inc de         
    ld a, (de)
    ld h, a    
    inc de   ; increment again to get to the next location
    ld (groundLevelMemoryLocationNow), de

    push hl         ;; you can't just do "ld hl, de", that instruction doesn't exist, 
                    ;; would be too easy, so have to use stack "push and pop"
    pop de

    ld hl,(D_FILE) 

    add hl,de       

    ld a, GROUND_CHARACTER_CODE
    ld (hl),a 
    push hl
    ;;; this code to fill up all the characters below ground level crashes at moment
fillGroundToBottom
    and a
    or a
    ld de,-725
    add hl,de
    ld a, l
    cp 0  
    jr z, fillGroundToBottomLoopExit
    pop hl
    ld de, 33
    add hl, de    
    ld a, GROUND_CHARACTER_CODE_2    
    ld (hl),a    
    push hl
    
    jr fillGroundToBottom
    
fillGroundToBottomLoopExit
    pop hl ; clear stack (has to be equal push and pop or will blow
 
    ; force remove the bottom left character which is being shift (and is score shifted)
    ld hl,(D_FILE) 
    ld de, 694
    add hl, de	
    xor 0 
    ld (hl),a 

    ld hl,(numberOfGroundBlocksMoved)
    inc hl
    ld (numberOfGroundBlocksMoved), hl
    and a   
    or a          
    ld de,-320
    add hl,de    
    jr nc,afterCheckingGroundIndex

    jr resetGound   ; Yes I know it's the next line but  
    
resetGound
    ld hl, 0 
    ld (numberOfGroundBlocksMoved),hl  

    ld de, startOfNormalGround    
    ld (groundLevelMemoryLocationNow), de

afterCheckingGroundIndex      
    
    ;; keyboard OR joystick input

    ;; move ship up / down
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 3, a					        ; N
    jp z, drawDown
    
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 2, a					        ; X
    jp z, drawUp  

    jp afterDrawUpDown  ; no key pressed ship move


drawDown    
    ld a,(vertPosition)         ; check vertical position is within limits
    inc a
    cp 22
    jp z, skipMove

    ld a,(vertPosition)     ; the cp 1 mangles a so reload
    inc a
    ld (vertPosition),a

    ;overrite existing ship pos with blank
    ld hl,(var_ship_pos)    
    ld a, 0
    ld (hl),a

    ld de,33
    add hl, de    
    ld (var_ship_pos), hl        
    ld a,SHIP_CHARACTER_CODE     
    ld (hl),a
        

    jp afterDrawUpDown
    
drawUp
    ld a,(vertPosition)         ; check vertical position is within limits
    dec a
    cp 1
    jp z, skipMove

    ld a,(vertPosition)     ; the cp 1 mangles a so reload
    dec a
    ld (vertPosition),a


    ;overrite existing ship pos with blank
    ld hl, (var_ship_pos)
    ld a, 0
    ld (hl),a

    and a
    or a
    ld de,33    
    sbc hl, de    

    ld (var_ship_pos), hl        
    ld a,SHIP_CHARACTER_CODE 
    ld (hl),a
       
    jp afterDrawUpDown  
       
skipMove    
afterDrawUpDown

    ;; move ship up / down
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		  
    bit 0, a        ; space key
    jp z, fireMissile
    
    jp afterDrawUpDownFire

fireMissile
    ld b,15
    ld hl,(var_ship_pos)    
    inc hl
    ld a,22 ;; minus sign symbol
LoopfireMissile
    inc hl 
    ld (hl),a        
    djnz LoopfireMissile   
    ld a, 1
    ld (missileFired), a
    jp afterDrawUpDownFire

afterDrawUpDownFire

; create a random number and enemy position row number between 0 and 6
tryAnotherR                             ; generate random number to index shape memory
    ld a, r                             
    and %00001111        
    ld (enemyStartRowPosition), a
    cp 11
    jp nc, tryAnotherR                  ; loop when nc flag set ie not less than 5 try again        

    ;; cut down number of enemies coming in
    cp 0
    jp z, skipAddEnemy
    cp 2
    jp z, skipAddEnemy
    cp 5
    jp z, skipAddEnemy
    cp 8
    jp z, skipAddEnemy
    cp 10
    jp z, skipAddEnemy
    
    ld a, (enemyStartRowPosition)
    ld b, a    
    ld hl, (D_FILE)
    ld de, 32   
calculateRowForEnemey
    add hl, de 
    djnz calculateRowForEnemey

    ld a,ENEMY_CHARACTER_CODE     
    ld (hl),a     

skipAddEnemy

    ;;;;;;; Check for collision with anything 
    ld hl,(var_ship_pos) 
    inc hl
    ld a, (hl)
    cp 0
	jp nz,gameover

preWaitloop
    ld a,(score_mem_tens)				   ;;; need to check for kills and then add one to score
    add a,1	
    daa									; z80 daa instruction realigns for BCD after add or subtract
    ld (score_mem_tens),a	
    cp 153
    jr z, addOneToHund
    jr skipAddHund
addOneToHund
    ld a, 0
    ld (score_mem_tens), a
    ld a, (score_mem_hund)
    add a, 1
    daa
    ld (score_mem_hund), a
skipAddHund	

printScoreInGame
    ld b, 21		; b is row to print in
    ld c, 1			; c is column
    ld a, (score_mem_hund) ; load hundreds
    call printByte    
    ld b, 21			; b is row to print in
    ld c, 3			; c is column
    ld a, (score_mem_tens) ; load tens		
    call printByte

    ;ld bc, (speedUpLevelCounter)
	;ld hl, (speedUpLevelCounter)   ; makes it more difficult as you progress
	;ld a, h
	;cp 0
	;jr z, waitloop
	;dec hl 
	;ld (speedUpLevelCounter), hl

	;ld bc, (speedUpLevelCounter)
    
    ld hl,(var_ship_pos) ; var_ship_pos already has the D_FILE offset added    
    ld a,SHIP_CHARACTER_CODE 
    ld (hl),a              
    
    ld bc, $00ff
waitloop
	dec bc
	ld a,b
	or c
	jr nz, waitloop
    

;;; clear missile (mainly to prevent it "shooting" own ship down)
;;; only if missile fire flag set    
    ld a, (missileFired)
    cp 1
    jp nz, mainGameLoop
    
    ld b,15
    ld hl,(var_ship_pos)    
    inc hl
    ld a,0 ;;blank character
LoopfireMissileClear
    inc hl 
    ld (hl),a        
    djnz LoopfireMissileClear
    ld a, 0
    ld (missileFired), a
	jp mainGameLoop
	
gameover
	ld bc,10
	ld de,crash_message_txt
	call printstring
	; copy the current score to high score, need to check it is higher!!
	
	ld a, (score_mem_tens) ; load tens		
	ld (last_score_mem_tens),a 
	ld a, (score_mem_hund) ; load tens		
	ld (last_score_mem_hund),a	



    ld bc, $ffff   ;; wait max time for 16bits then go back to intro	
    
waitLoopEndGame
    dec bc
    ld a,b
    or c
    jp nz, waitLoopEndGame
    jp intro_title

    ret  ; never return to basic

; original game written by Jon Kingsman, for zx spectrum, ZX81 port/rework by Adrian Pilkington 


; this prints at top any offset (stored in bc) from the top of the screen D_FILE
printstring
	ld hl,(D_FILE)
	add hl,bc	
printstring_loop
	ld a,(de)
	cp $ff
	jp z,printstring_end
	ld (hl),a
	inc hl
	inc de
	jr printstring_loop
printstring_end	
	ret
    
    
hprint 		;;http://swensont.epizy.com/ZX81Assembly.pdf?i=1
	PUSH AF ;store the original value of A for later
	AND $F0 ; isolate the first digit
	RRA
	RRA
	RRA
	RRA
	ADD A,$1C ; add 28 to the character code
	CALL PRINT ;
	POP AF ; retrieve original value of A
	AND $0F ; isolate the second digit
	ADD A,$1C ; add 28 to the character code
	CALL PRINT
	RET
    
    
;include our variables
#include "vars.asm"

; ===========================================================
; code ends
; ===========================================================
;end the REM line and put in the RAND USR line to call our 'hex code'
#include "line2.asm"

;display file defintion
#include "screen.asm"               

;close out the basic program
#include "endbasic.asm"
