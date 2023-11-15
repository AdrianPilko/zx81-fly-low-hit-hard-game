;;;;;;;;;;;;;;;;;;;;;
;;; ZX81 fly low hit hard game by Adrian Pilkington 02/12/2022
;;; uses simple charcter/block graphics and requires 16K ZX81
;;;;;;;;;;;;;;;;;;;;;

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
#define ENEMY_CHARACTER_CODE_1   128 
#define ENEMY_CHARACTER_CODE_2   136 
#define ENEMY_CHARACTER_CODE_3   129 
#define ENEMY_CHARACTER_CODE_4   132 
 
#define SPECIAL_ENEMY_CHARACTER_CODE_1 137
#define SPECIAL_ENEMY_CHARACTER_CODE_2 134
#define SPECIAL_ENEMY_CHARACTER_CODE_3 138
#define SPECIAL_ENEMY_CHARACTER_CODE_4 6
#define GROUND_CHARACTER_CODE 61
#define GROUND_CHARACTER_CODE_2 189
#define SURFACE_MISSILE 5  ; vertical bar
#define SURFACE_MISSILE_SMOKE 8  ; grey block, shame no grey vertical bar!!
#define MAX_MISSILES 1    ; currently set to one whilst we debug the code to fire missile
#define SLOW_FRAME_RESET_THRESHOLD  11
#define VERY_SLOW_FRAME_RESET_THRESHOLD  27

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
#define KEYBOARD_READ_PORT_A_TO_G	$FD
#define KEYBOARD_READ_PORT_ENTER_TO_H $BF 
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 

	jp setHighScoreZero

playersLives
    DEFB 0
livesText
    DEFB _L,_I,_V,_E,_S,$ff
hitEnemy
    DEFB 0
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
    DEFB 0
numberOfGroundBlocksMoved    
    DEFB 0,0
missileColCounter      
    DEFB 0,0,0,0,0,0
missileRowCounter      
    DEFB 0,0,0,0,0,0    
missileOnOffState      
    DEFB 0,0,0,0,0,0            
missileCalculatedDisplayPos
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0    
missileIndex
    DEFB 0
missileDeleteAfterNext
    DEFB 0
slowFrameCount_1      ; this is to control things that should only happen slower than the game loop 
                    ; like drawing a new enemy
    DEFB 0             
slowFrameCount_2      ; this is to control things that should only happen _even_ slower than the game loop 
                    ; like drawing a surface launched missile
    DEFB 0      
groundResetCountDown
    DEFB 0
surfaceMissileInFlight  
    DEFB 0      
surfaceMissileCurrentPos
    DEFB 0,0          
surfaceMissileRowCountDown
    DEFB 0
launchDetected
    DEFB 0
specialEnemyNow
    DEFB 0
byteAlign
    DEFB 0    
specialEnemyNowPos
    DEFB 0,0
scoreText
    DEFB    _S,_C,_O,_R,_E,$ff
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
restartText    
    DEFB    _G,_E,_T,__,_R,_E,_A,_D,_Y,__,_P,_L,_A,_Y,_E,_R,__,_1,$ff    
wonTheGameText_1    
    DEFB	18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,18,19,$ff
wonTheGameText_2        
    DEFB    _Y,_O,_U,__,_W,_I,_N,__,_A,_N,_D,__,_A,_R,_E,__,_T,_H,_E,__,_B,_E,_S,_T,__,_P,_I,_L,_O,_T,$ff        
SAMLaunchText    
    DEFB    _W,_R,_R,_N,_I,_N,_G,__,_S,_A,_M,__,_L,_A,_U,_N,_C,_H,__,_D,_E,_T,_E,_C,_T,_E,_D,$ff        
BlankSAMLaunchText
    DEFB    __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$ff        
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
	DEFB 0,0
to_print .equ to_print_mem ;use printByte16
VSYNCLOOP     .equ      5

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
    jp nz, introWaitLoop_1
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

	ld b,VSYNCLOOP
waitForTVSync	
	call vsync
	djnz waitForTVSync

    xor a 						; initialise score to zero, and 0 results in a equal to zero
    ld (score_mem_tens),a	
    ld (score_mem_hund),a
    ld (score_mem_thou),a	    
    ld (hitEnemy), a
    ld (launchDetected), a
    ld a, 5    
    ld (playersLives), a	
    ld (groundResetCountDown), a  ;; wrap arounds to complete level    

restartAfterLivesLost           ;; reset evverything except score and lives
    call CLS
    ld bc,335
    ld de,restartText
    call printstring	
    xor a

restartWaitLoop
    ld bc,$ffff ;max waiting time
restartWaitLoop_1
    dec bc
    ld a,b
    or c
    jp nz, restartWaitLoop_1
    call CLS    
    
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

    ex de, hl         ;; optimised from push hl ; pop de 21clock cycles to  register exhange, 4clock cycles!!!   
    
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
    
    ld a, 9 
    ld (vertPosition),a
    
    xor a           ; set number of missiles in flight to zero
    ld (missileIndex), a    
    ld (missileOnOffState), a    
    ld (missileDeleteAfterNext), a    
    ld (slowFrameCount_1), a
    ld (slowFrameCount_2), a
    ld hl, (D_FILE+1)
    ld (missileCalculatedDisplayPos),hl   ;; for muliple missile the needs indexing    
       
mainGameLoop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scroll ground left           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ld b, 20
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

printScoreInGame
    ld bc,661	
    ld de,scoreText
    call printstring		
    
    ld b, 20		; b is row to print in
    ld c, 7			; c is column
    ld a, (score_mem_hund) ; load hundreds
    call printByte    
    ld b, 20			; b is row to print in
    ld c, 9			; c is column
    ld a, (score_mem_tens) ; load tens		
    call printByte

    ;;; print player lives
    ld bc,673	
    ld de,livesText
    call printstring
    ld b, 20		; b is row to print in
    ld c, 18			; c is column
    ld a, (playersLives)
    call printByte  



   
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

    ex de, hl         ;; optimised from push hl ; pop de 21clock cycles to register exhange, 4clock cycles!!!
    
    ;ld de, (groundLevelMemoryLocationNow)
    ;inc de
    ;inc de
    ;ld (groundLevelMemoryLocationNow), de

    ld hl,(D_FILE) 

    add hl,de       

    ld a, GROUND_CHARACTER_CODE
    ld (hl),a 
    push hl
    ;;; this code to fill up all the characters below ground level crashes at moment
fillGroundToBottom
    and a
    or a
    ld de,-626
    add hl,de
    ld a, l
    cp 0  
    jp z, fillGroundToBottomLoopExit
    pop hl
    ld de, 33
    add hl, de    
    ld a, GROUND_CHARACTER_CODE_2    
    ld (hl),a    
    push hl
    
    jp fillGroundToBottom
    
fillGroundToBottomLoopExit
    pop hl ; clear stack (has to be equal push and pop or will blow
 
    ld hl,(numberOfGroundBlocksMoved)
    inc hl
    ld (numberOfGroundBlocksMoved), hl
    and a   
    or a          
    ;ld de,-320    ;; neat way of checking if a 16bit limit reached load negative constant then add and check "no carry"
    ld de,-608    ;; neat way of checking if a 16bit limit reached load negative constant then add and check "no carry"
    add hl,de    
    jp nc,afterCheckingGroundIndex

    jp resetGound   ; Yes I know it's the next line but  
    
resetGound
    ld hl, 0 
    ld (numberOfGroundBlocksMoved),hl  

    ld de, startOfNormalGround    
    ld (groundLevelMemoryLocationNow), de
    
    ld a, (groundResetCountDown)
    dec a
    ld (groundResetCountDown), a
    cp 0
    jp z, endOfLevel
    

afterCheckingGroundIndex      
    
    ;; keyboard OR joystick input
    ld a, (var_keys_or_joystick)    ; keys = 1
    and 1
    jp nz, use_keys_only
    
    ; this works if the joystick is plugged in, and if not then the keys still work, ace :)
    ld b, 00000100b   ; 1 , joystick down
    in a,($1F)       ; a now has the input byte from the port 1f (which is the joystick port)
    ld c, a
    and b 
    jp nz, drawDown
    ld a, c
    ld b, 00001000b   ; 2 decimal, joystick up
    and b
    jp nz, drawUp
    
use_keys_only
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


    ;; check for fire pressed, but restrick joystick only if was selected (compatibility with emulators!)
    ld a, (var_keys_or_joystick)    ; keys = 1
    and 1
    jp nz, justCheckKeysForFire

    ld b, 00010000b   ; 16 decimal, fire button
    in a,($1F)       ; a now has the input byte from the port 1f (which is the joystick port)
    and b 
    jp nz, fireMissile
    
justCheckKeysForFire    
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		  
    bit 0, a        ; space key
    jp z, fireMissile
    
    jp missileUpdates



;missileColCounter and missileRowCounter are used to keep track of the missile positions
; FOR now just work with one missile!!
; after the shot is fired the screen would scroll the missile left we need to scroll it right by 2
; to compensate

;; fire missile will set the initial conditions of row, column and the active on off state
fireMissile

    ;ld a, MAX_MISSILES
    ;ld b, (missileIndex)
    ;cp b
    ;jp z, missileUpdates
    ld a, (missileIndex)        ; this make you only allowed one missile 
    cp 1
    jp z, missileUpdates
    
    ld a, 1
    ld (missileOnOffState), a    
    ld a, 1
    ld (missileIndex), a           ; for now just get one missile working!!!
    ;inc a
    ;cp 6
    ;jp nz, afterCycleMissileIndex
    ;xor a ; zero a 
;afterCycleMissileIndex
    
    ;ld (missileIndex), a    
    ld a, (vertPosition)   ; vert position is the current row the ship is at is +1 
    dec a
    ld (missileRowCounter), a
    ld a, 2                 ; start at column 2
    ld (missileColCounter), a    

missileUpdates
;; draw missiles currently in flight and update positions    
;;;;;;;;;;;;
    ld bc, (missileIndex)
    cp 0
    jp z, afterDrawUpDownFire
;missileUpdateLoop    
 ;   push bc
    ; calculate the screen position to draw at and draw each, update after
    ld a, (missileOnOffState)
    cp 0
    jp z, noUpdateForThisOne 
    
    ld a, (missileRowCounter)
    ld b, a        
    ld hl, (D_FILE)
    inc hl
    ld de, 33  
loopCalcMissileScreenOffset
    add hl, de 
    djnz loopCalcMissileScreenOffset
    ld de, (missileColCounter)    ; now add the row  
    add hl, de
    ;call debugPrintRegisters
    ld (missileCalculatedDisplayPos),hl  
        
    ;we now should have the current offset to the display memory for the missile so draw on screen
             
    ; we need to check for collision with anything, if it has then, mark the missile for deletion later
    
    ;; another instance of collision detection
    ld a,(hl)        
    cp ENEMY_CHARACTER_CODE_1    ;;the enemy character (should be #define really to aid readablity
    jp z, markForDelete 
    cp ENEMY_CHARACTER_CODE_2    ;;the enemy character (should be #define really to aid readablity
    jp z, markForDelete 
    cp ENEMY_CHARACTER_CODE_3    ;;the enemy character (should be #define really to aid readablity
    jp z, markForDelete 
    cp ENEMY_CHARACTER_CODE_4    ;;the enemy character (should be #define really to aid readablity
    jp z, markForDelete 
    cp SPECIAL_ENEMY_CHARACTER_CODE_1    ;;the enemy character (should be #define really to aid readablity        
    jp z, markForDelete 
    cp SPECIAL_ENEMY_CHARACTER_CODE_2    ;;the enemy character (should be #define really to aid readablity        
    jp z, markForDelete 
    cp SPECIAL_ENEMY_CHARACTER_CODE_3    ;;the enemy character (should be #define really to aid readablity        
    jp z, markForDelete     
    cp SPECIAL_ENEMY_CHARACTER_CODE_4    ;;the enemy character (should be #define really to aid readablity        
    jp z, markForDelete     
    
    ld a,(hl)        
    cp SURFACE_MISSILE_SMOKE    ;; allow player to shoot through the missile smoke, but will die if hits 
    jp z, markForDelete         ;; will also die if hits the missile and cannot shoot that down!
    
    cp GROUND_CHARACTER_CODE   ;; check the ground as well (both types :)
    jp z, deleteMissileButNotGround 
    cp GROUND_CHARACTER_CODE_2     ;; check the ground as well (both types :)
    jp z, deleteMissileButNotGround
    
    jp skipPastMarkDelete    
    
deleteMissileButNotGround
    ld a, 1
    ld (missileDeleteAfterNext), a
    xor a
    ld (missileIndex),a
    ld (missileOnOffState),a 
    ld (missileDeleteAfterNext), a  
    jp loopBeforeEndMissileUpdate  
markForDelete 
    
    ld a, 1
    ld (missileDeleteAfterNext), a
    ld (hitEnemy), a
    ;; so here we want to make it look like an explosion, lets use * :)
    ld a,23 ;; star for missile hit
    ld (hl),a        
    jp loopBeforeEndMissileUpdate    
skipPastMarkDelete    
    
    ld a, 4 ;; was minus sign symbol for missile , now smal block
    ld (hl),a        

   
loopBeforeEndMissileUpdate   
noUpdateForThisOne
  
afterDrawUpDownFire

    ld a, (launchDetected)
    cp 1
    jp nz,checkSurfaceMissile 
    
    ld bc,694
    ld de,SAMLaunchText
    call printstring		
    

checkSurfaceMissile
;;; check if missile in flight already, if so update
    ld a, (surfaceMissileInFlight)
    cp 0
    jp z, checkSlowFrameCount_2

    ;; check surface missile isn't at top of it's flight
    ld a, (surfaceMissileRowCountDown)
    dec a
    cp 0
    jp z, explodeSurfaceMissile
    
    jp continueDrawSufaceMissile
    ;; code to explode missile at end of it's flight
explodeSurfaceMissile
    ld bc,694
    ld de,BlankSAMLaunchText
    call printstring		
    
    ld hl, (surfaceMissileCurrentPos)
    ; replace current position minus 1 with smoke trail (because screen scroll)
    ld a, 0 ; blank smoke
    dec hl     
    ld (hl), a
    
    ld a,23   ;; star for missile explosion
    
    ld de, -33  
    add hl, de  ; add -33  gets round that you can't subtract 16bit !!
    ld (hl),a

    ld de, 33  
    add hl, de  ; add -33  gets round that you can't subtract 16bit !!
    ld (hl),a

    dec hl    
    ld (hl),a
    
    inc hl    
    ld (hl),a
    
    inc hl
    ld (hl),a
    dec hl
    
    ld de, 33  
    add hl, de  ; add -33  gets round that you can't subtract 16bit !!
    
    ld a,23   ;; star for missile explosion
    ld (hl),a
    
    ld a, 1
    ld (surfaceMissileRowCountDown), a
    ld a, 0
    ld (surfaceMissileInFlight), a
    ld (launchDetected), a
    jp checkSlowFrameCount_2    
    
continueDrawSufaceMissile        
    ld (surfaceMissileRowCountDown), a
        
    ld hl, (surfaceMissileCurrentPos)
    ; replace current position minus 1 with smoke trail (because screen scroll)
    ld a, 0 ; blank smoke
    dec hl     
    ld (hl), a
    
    ld a,SURFACE_MISSILE_SMOKE ;; grey block for smoke
    ld (hl), a
    ld a,0 ;; blank previous smoke - 2    
    ld de, 33  
    add hl, de      
    add hl, de  
    add hl, de  
    ld (hl), a
    
    ld (hl), a

    
    ld de, -33  ; bit clunky but add -33 to move back to one about previous location
    add hl, de  ; add -33  gets round that you can't subtract 16bit !!
    add hl, de  ; add -33  gets round that you can't subtract 16bit !!
    add hl, de  ; add -33  gets round that you can't subtract 16bit !!
    add hl, de  
    ld a,SURFACE_MISSILE ;; vertical bar for missile
    ld (hl),a
    ld (surfaceMissileCurrentPos),hl  ; save new SAM position
    
;; Launch Surface to Air Missile  (SAM), not good at tracking so only SAM 1, not SAM 2 :)
checkSlowFrameCount_2
    ld a, (slowFrameCount_2)
    cp (VERY_SLOW_FRAME_RESET_THRESHOLD-1)
    jp z, noSkipAddSurfaceLaunched
    jp checkIfTimeToAddEnemyShip  

noSkipAddSurfaceLaunched
    ld a, 1
    ld (specialEnemyNow), a
    
    xor a
    ld (slowFrameCount_2),a  
    ld a, 1
    ld (surfaceMissileInFlight), a
    
    ld a,SURFACE_MISSILE ;; vertical bar for missile

    ld de, 615  ; start SAM this position relative to D_FILE (of course); maybe later add random column    
    ld hl, (D_FILE)        
    inc hl
    add hl, de    
    ld (hl),a
    ld (surfaceMissileCurrentPos),hl
      
    ld a, (vertPosition) 
    push af ; ld b, (vertPosition) directly didn't work??!
    pop bc
    ld a, 19
    sub b       
    ld (surfaceMissileRowCountDown), a 
    ld a,1
    ld (launchDetected),a
    

checkIfTimeToAddEnemyShip
    ld a, (slowFrameCount_1)
    cp (SLOW_FRAME_RESET_THRESHOLD-1)
    jp z, noSkipAddEnemy
    jp skipAddEnemy
    ; create a random number and enemy position row number between 0 and 6
noSkipAddEnemy    
tryAnotherR                             
    ld a, r                             
    and %00001111        
    ld (enemyStartRowPosition), a
    cp 10
    jp nc, tryAnotherR                  

    ld a,(specialEnemyNow)    
    cp 0
    jp z, justCalcNormalEnemyRow
    ld a, (vertPosition)
    
    jp addSpecialEnemy
    
justCalcNormalEnemyRow
    ld a, (enemyStartRowPosition)    
    ld b, a    
    ld hl, (D_FILE)
    ld de, 33
calculateRowForEnemey
    add hl, de 
    djnz calculateRowForEnemey
    ld de, 31       ; put them on last column
    add hl, de
    jp justAddEnemy
    
addSpecialEnemy        
    ld b, a    
    ld hl, (D_FILE)
    ld de, 33
calculateRowForSpecEnemey
    add hl, de 
    djnz calculateRowForSpecEnemey
    ld de, 31       ; put them on last column
    add hl, de
        
    xor a
    ld (specialEnemyNow), a
    ;ld a, 30
    ;ld (specialEnemyColumn), a    
    ld (specialEnemyNowPos), hl
    ; so if we have a none zero special enemy (the store start position in specialEnemyNow)
    ld a,SPECIAL_ENEMY_CHARACTER_CODE_1     
    
    ld (hl),a       ;; make the special enemy a bigger block
    dec hl
    ld (hl),a     
    dec hl
    ld a, SPECIAL_ENEMY_CHARACTER_CODE_4
    ld (hl),a     
    
    ld de, 33    
    add hl, de
    ld a, SPECIAL_ENEMY_CHARACTER_CODE_2        
    ld (hl),a     
    ld a,SPECIAL_ENEMY_CHARACTER_CODE_3     
    inc hl
    ld (hl),a
    inc hl
    ld (hl),a
    
    ;call debugPrintRegisters
    ;ret
    jp skipAddEnemy
    
justAddEnemy
    ;ld a,ENEMY_CHARACTER_CODE     
    ;ld (hl),a     
    
    ld a,ENEMY_CHARACTER_CODE_1     
    
    ld (hl),a       ;; make the special enemy a bigger block
    dec hl
    ld a,ENEMY_CHARACTER_CODE_2
    ld (hl),a     
    dec hl
    ld a, ENEMY_CHARACTER_CODE_3
    ld (hl),a     
    
    ld de, 33    
    add hl, de
    ld a, ENEMY_CHARACTER_CODE_4    
    ld (hl),a     
    ld a,ENEMY_CHARACTER_CODE_1
    inc hl
    ld (hl),a
    ld a,ENEMY_CHARACTER_CODE_2
    inc hl
    ld (hl),a    

skipAddEnemy

    ;;;;;;; Check for collision with anything 
    ld hl,(var_ship_pos) 
    inc hl
    ld a, (hl)
    cp 0
	jp nz,checkGameOver

    
    jp skipCheckGameOver
checkGameOver
    ld a, (playersLives)    
    dec a
    cp 0
    jp z, gameover
    ld (playersLives), a
    ; now reset game to start apart from initialising player live
    jp restartAfterLivesLost
    
skipCheckGameOver
    ld a, (hitEnemy)
    cp 0
    jp z,preWaitloop   ; we didn't get a hit so score still same
    
    xor a
    ld (hitEnemy), a
    
    ld a,(score_mem_tens)				   ;;; need to check for kills and then add one to score
    add a,1	
    daa									; z80 daa instruction realigns for BCD after add or subtract
    ld (score_mem_tens),a	
    cp 153
    jp z, addOneToHund
    jp skipAddHund
addOneToHund
    ld a, 0
    ld (score_mem_tens), a
    ld a, (score_mem_hund)
    add a, 1
    daa
    ld (score_mem_hund), a
    ;; if by a stroke of luck the player gets to 100 then add a life
    ld a, (playersLives)
    inc a
    ld (playersLives), a 
skipAddHund	

preWaitloop    
    ;ld bc, $01ff
    ld bc, $00bf
    ;ld bc, $ffff   ; for debug long wait
     ;ld bc, $000f   ; fastest in testing
waitloop
	dec bc
	ld a,b
	or c
	jp nz, waitloop
    
;;; clear missile (mainly to prevent it "shooting" own ship down)
;;; only if missile fire flag set    7

    ld a, (missileIndex)
    cp 1
    jp nz, lastThings    

    ; blank the missiles last position, we can do this because the wait loop has run and given it time to be shown
    xor a       
    ld hl, (missileCalculatedDisplayPos)    ;; for muliple missile the needs indexing        
    ld (hl),a    
    inc hl
    ld (hl),a    

    ; this is the code that actually moves the missile accross the screen
    ld a, (missileColCounter)
    inc a    
    cp 17 ; missile range limit to 17
    jp z, disableMissile   
    ld (missileColCounter), a
    
    ld a, (missileDeleteAfterNext)
    cp 1
    jp z,disableMissile
    jp lastThings
disableMissile

    xor a
    ld (missileIndex),a
    ld (missileOnOffState),a 
    ld (missileDeleteAfterNext), a


;last Things in game loop
lastThings
    ld a, (slowFrameCount_1)
    inc a
    cp SLOW_FRAME_RESET_THRESHOLD
    jp z, zeroslowFrameCount_1
    
    ld (slowFrameCount_1),a 
    jp mainGameLoop
        
zeroslowFrameCount_1    
    xor a       ;; zero a then slowFrameCount_1
    ld (slowFrameCount_1),a   
    
    ld a, (slowFrameCount_2)            ; this does at half rate again of slowFrameCount_1
    inc a
    cp VERY_SLOW_FRAME_RESET_THRESHOLD
    jp z, zeroslowFrameCount_2
    
    ld (slowFrameCount_2),a   
    
    jp mainGameLoop
    
zeroslowFrameCount_2
    xor a
    ld (slowFrameCount_2),a
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

    ret  ; never return to basic, but will never get here

endOfLevel
    call CLS
    ld bc,232
    ld de, wonTheGameText_1    
    call printstring

    ld bc,265
    ld de, wonTheGameText_1
    call printstring
    
    ld bc,332
    ld de, wonTheGameText_2
    call printstring

    ld bc,397
    ld de, wonTheGameText_1
    call printstring    
    ld bc,430
    ld de, wonTheGameText_1    
    call printstring
    
    ld bc, $ffff   ;; wait max time for 16bits then go back to intro	    
waitLoopEndOfLevel
    dec bc
    push bc
    ld bc, $000f   ;; wait max time for 16bits then go back to intro	    
waitLoopEndOfLevelInner
    dec bc
    ld a,b
    or c    
    jp nz, waitLoopEndOfLevelInner

    pop bc
    ld a,b
    or c    
    jp nz, waitLoopEndOfLevel
    ;; bonus + 500 score
    ld a, 0
    ld (score_mem_tens), a
    ld a, (score_mem_hund)
    add a, 5
    daa
    ld (score_mem_hund), a    

	ld a, (score_mem_tens) ; load tens		
	ld (last_score_mem_tens),a 
	ld a, (score_mem_hund) ; load tens		
	ld (last_score_mem_hund),a	
    
    jp intro_title

;check if TV synchro (FRAMES) happend
vsync	
	ld a,(FRAMES)
	ld c,a
sync
	ld a,(FRAMES)
	cp c
	jr z,sync
	ret



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

debugPrintRegisters
    ; take copy of all the registers
    push hl
    push de
    push af    
    push bc
    
    ; position the cursor
    ;set b to row, c to first col, which is the last row
    ld b, 21
    ld c, 0    
    call PRINTAT
    pop bc
    pop af
    pop de
    pop hl    
    
    push hl
    push de
    push af    
    push bc
    
    ld a, a
    call hprint    
    ld a, 14
    call PRINT  

    ld a, h
    call hprint    
    ld a, l    
    call hprint
    ld a, 14
    call PRINT
       
    ld a, d
    call hprint
    ld a, e
    call hprint
    ld a, 14
    call PRINT

    ld a, b
    call hprint
    ld a, c
    call hprint    
   
   
    ;restore registers (in correct reverse order!)        
    pop bc
    pop af
    pop de
    pop hl
    
    ret
    
    
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
