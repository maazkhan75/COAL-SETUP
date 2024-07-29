[org 0x0100]     ;;;( TETRIS GAME MADE BY MAAZ KHAN in Nov,2023 as a coal project ) github profile: https://github.com/maazkhan75

jmp start

developedBy: db ' D E V E L O P E D   B Y',0    ;null terminated string..  (we will use printStr with block instrc. because it is fast)
developerName: db '( M  A  A  Z   -   K  H  A  N )',0    ;null terminated string..  (we will use printStr with block instrc. because it is fast)
gameInfo_A: db'I M P L E M E N T E D    I N',0 
gameInfo_B: db'( 0 x 8 8    A S S E M B L Y    L A N G U A G E  )',0
copyrightNotice: db'(c) CopyRight 2024 - maazkhan75  ( ALL__RIGHTS__RESERVED! )',0
instructionsLabel: db '-<<-  G A M E P L A Y     I N S T R U C T I O N S  ->>-', 0    
instruction1:db '  Total Game_Time = 15 minutes (900sec)', 0
instruction2:db '  Game will be over if shapes touch the roof or time is completed', 0
instruction3:db '  Use <- key for moving shape (left)', 0
instruction4:db '  Use -> key for moving shape (right)', 0
instruction5:db '  Use up key for opposing the motion of falling', 0
instruction6:db '  Use down key to accelerate falling', 0
instruction7:db '  press (SPACE) to rotate the shape', 0
instruction8:db '  press (R SHIFT) to erase all the shapes', 0
instruction9:db '  press (ESC) to quit the game', 0
instruction10:db '--------------   PRESS  ANY  KEY  TO  CONTINUE   --------------', 0
gameQuitedMesg:db '{   G A M E    Q U I T E D    S U C C U S S F U L L Y   }', 0
gameDificultyLabel:db '{   S E L E C T     D I F I C U L T Y     L E V E L    }', 0
dificulty_type1:db '    ->  E A S Y',0
dificulty_type2:db '    ->  M E D I U M',0
dificulty_type3:db '    ->  H A R D',0
selectionInstruction_1:db ' USE UP DOWN KEYS TO NAVIGATE',0
selectionInstruction_2:db '--------------   PRESS  (ENTER)  KEY  TO  CONFIRM   --------------',0

delayProfile: dw 1   ;by default it is easy   if the end user simply hit space without selecting lev of dificulty ;will be used in making dificulty levels..

bullet:db '*',0
blank:db ' '
symbol:db '-'

shapeNo: dw 0   ;randomNo generated for choice of the shape
nextShapeNo: dw 0
currShapeNo: dw 0

;for boundary making
currentRowL: dw 0
currentColL: dw 0
currentRowR: dw 0
currentColR: dw 0

scoreLabel: db 'SCORE : ',0
timerLabel: db 'TIME : ',0
secLabel: db ' sec',0
dashes: db '-----------------',0

currScore: dw 0
NextShapeLabel: db ' ( N E X T ) ',0

;coordinates for shape printing
shapeCurrRow: dw 0
shapeCurrCol: dw 0				 
 
gameOverLabel:db '----------  G A M E   O V E R  ----------',0
GameResultsLabel:db ' _________________  G A M E   R E S U L T S  _________________ ',0
GameResultsInstruction1: db 'PRESS  ( R )  TO  PLAY  AGAIN',0
GameResultsInstruction2: db 'PRESS  ( Q )  TO  QUIT',0

curtainEffectCurrRow:dw 0
curtainEffectCol1:dw 0
curtainEffectCol2:dw 79

printAllow:dw 0  ;flag for falling
decisionForReplay:dw 0    ;initally 0 then 1 for replay and 2 for quit

tickCount: dw 0
currSeconds: dw 0

gameRunnningFlag: dw 1   ;it will be false when the game is over to stop timer seconds 
consecutiveLocation: dw 0

horizontalKeyPress: db ' ' 
verticalKeyPress: db ' ' 

totalRowsOfMainFrameToBeScan: dw 0
gameTimerFlag: dw 0
keyboardFlag: dw 1 ;for the purpose of removing glitch of shape remains in first row and it can also move left right so will disable keyboard inputs for some time after game over..
quickClearShapesFlag: dw 0
choiceConfirmedFlag: dw 0
selectionIterator: dw 1
gameStartedFlag: dw 0

shapeTypeIterator: dw 0
oldShapeType: dw 0

gamePause: dw 0
horizontalMovBlockFlag: dw 0

clrScr:
		push es
		push ax
		push di
		
		mov ax,0xb800
		mov es,ax
		mov di,0
			
	nextLoc:
		mov word[es:di],0x0020
		add di,2
		cmp di,4000
		jnz nextLoc

       ;;;PROBLEM DONE ON SMALL SCALE FOR COLLISION HANDING TO READ AND COMPARE DATA PRINTED ON SCREEN.....

		; mov di,20
		; cmp word[es:di],0x0020
		; jne notFind
		
		; ;----------------------------------testing-----------
      ; mov ax,2
	  ; push ax                      ;[bp+10]
	  ; mov ax,7
	  ; push ax                      ;[bp+8]
	  ; mov ax,0x0E
	  ; push ax                      ;[bp+6]
	  ; mov ax,instruction1    
	  ; push ax                      ;[bp+4]
	  ; call printStrWBI		
		; ;---------------------------------------------------
		
		; notFind:
		
		pop di
		pop ax
		pop es
		ret
				
delay:

	push bx
	push cx
	
	mov cx,0xffff
	mov bx,0
	
	delaying: 
			add bx,1
			loop delaying
	
	pop cx
	pop bx
	ret

shortDelay:

	push bx
	push cx
	
	mov cx,0x5555
	mov bx,0
	
	shortDelaying: 
			add bx,1
			loop shortDelaying
	
	pop cx
	pop bx
	ret
	
	


makeGameLogo:

push bp
push ax
push bx
push cx
push dx
push si
push di
push es
push ds

mov ax,0xb800
mov es,ax
mov al,0x20  ;ASCII SET OF SPACE

;;  IMPORTANT LEARNING NOTE : even hi hona chahiya cell location x  in ( es:x ) warna wo ghalat 1 1 bytes pichla  ur agala cell ki utha leta hai.... 




;;--------------------------making T -----------------------
mov ah,0xA0

mov dx,8
mov di,1796
L1:
mov [es:di],ax
call shortDelay
add di,2
dec dx
jnz L1



mov dx,4
mov di,1962
L2:
mov [es:di],ax
call shortDelay
mov [es:di+2],ax
call shortDelay
add di,160
dec dx
jnz L2


call delay
call delay
call delay
call delay
;;--------------------------making E -----------------------
mov ah,0xC0

mov dx,6
mov di,1816
L3:
mov [es:di],ax
call shortDelay
add di,2
dec dx
jnz L3

mov dx,5
mov di,1814
L4:
mov [es:di],ax
call shortDelay
mov [es:di+2],ax
call shortDelay
add di,160
dec dx
jnz L4


mov dx,5
mov di,2136
L5:
mov [es:di],ax
call shortDelay
mov [es:di+2],ax
call shortDelay
add di,2
dec dx
jnz L5


mov dx,5
mov di,2456
L6:
mov [es:di],ax
call shortDelay
mov [es:di+2],ax
call shortDelay
add di,2
dec dx
jnz L6


call delay
call delay
call delay
call delay
; ; ;------------------- making T -----------------------------
mov ah,0x90

mov dx,8
mov di,1830
L7:
mov [es:di],ax
call shortDelay
add di,2
dec dx
jnz L7



mov dx,4
mov di,1996
L8:
mov [es:di],ax
call shortDelay
mov [es:di+2],ax
call shortDelay
add di,160
dec dx
jnz L8


call delay
call delay
call delay
call delay
;;-------------------- making R -----------------------
mov ah,0x70

mov [es:2008],ax
call shortDelay
mov [es:2008+2],ax
call shortDelay
mov [es:2008+160],ax
call shortDelay
mov [es:2008+160+2],ax
call shortDelay
mov [es:2008+160+160],ax
call shortDelay
mov [es:2008+160+160+2],ax
call shortDelay
mov [es:2008+160+160+160],ax
call shortDelay
mov [es:2008+160+160+160+2],ax


mov [es:1848],ax
call shortDelay
mov [es:1848+2],ax
call shortDelay
mov [es:1848+2+2],ax
call shortDelay
mov [es:1848+2+2+2],ax
call shortDelay
mov [es:1848+2+2+2+2],ax
call shortDelay
mov [es:1848+2+2+2+2+2],ax
call shortDelay
mov [es:1848+2+2+2+2+2+2],ax
call shortDelay
mov [es:1848+2+2+2+2+2+2+2],ax


mov [es:2020],ax
call shortDelay
mov [es:2020+2],ax
call shortDelay
mov [es:2020+160],ax
call shortDelay
mov [es:2020+160+2],ax
call shortDelay
mov [es:2020+160+2-2],ax
call shortDelay
mov [es:2020+160+2-2-2],ax
call shortDelay
mov [es:2020+160+2-2-2-2],ax
call shortDelay
mov [es:2020+160+2-2-2-2-2],ax
call shortDelay
mov [es:2020+160+2-2-2-2-2-2],ax


mov [es:2338],ax
call shortDelay
mov [es:2338+2],ax
call shortDelay
mov [es:2338+160],ax
call shortDelay
mov [es:2338+160+2],ax
call shortDelay


call delay
call delay
call delay
call delay
;;----------------- making I --------------------------
mov ah,0x60

mov [es:1866],ax
call shortDelay
mov [es:1868],ax
call shortDelay
mov [es:1866+160],ax
call shortDelay
mov [es:1868+160],ax
call shortDelay
mov [es:1866+160+160],ax
call shortDelay
mov [es:1868+160+160],ax
call shortDelay
mov [es:1866+160+160+160],ax
call shortDelay
mov [es:1868+160+160+160],ax
call shortDelay
mov [es:1866+160+160+160+160],ax
call shortDelay
mov [es:1868+160+160+160+160],ax


call delay
call delay
call delay
call delay
;;--------------------- making S -------------------
mov ah,0x50

mov [es:1872],ax
call shortDelay
mov [es:1872+2],ax
call shortDelay
mov [es:1872+2+2],ax
call shortDelay
mov [es:1872+2+2+2],ax
call shortDelay
mov [es:1872+2+2+2+2],ax
call shortDelay
mov [es:1872+2+2+2+2+2],ax    ;;we can also write +2+2+2+2+2 as +10   :) happy coding...
call shortDelay


mov [es:1872+160],ax
call shortDelay
mov [es:1872+160+2],ax
call shortDelay
mov [es:1872+160+160],ax   
call shortDelay
mov [es:1872+160+160+2],ax


mov [es:2194],ax
call delay
mov [es:2194+2],ax
call delay
mov [es:2194+2+2],ax
call delay
mov [es:2194+2+2+2],ax
call delay
mov [es:2194+2+2+2+2],ax
call delay
mov [es:2194+2+2+2+2+160],ax
call delay
mov [es:2194+2+2+2+2+160-2],ax
call delay
mov [es:2194+2+2+2+2+160+160],ax
call delay
mov [es:2194+2+2+2+2+160+160-2],ax
call delay
mov [es:2194+2+2+2+2+160+160-2-2],ax
call delay
mov [es:2194+2+2+2+2+160+160-2-2-2],ax
call delay
mov [es:2194+2+2+2+2+160+160-2-2-2-2],ax
call delay
mov [es:2194+2+2+2+2+160+160-2-2-2-2-2],ax
call delay
call delay
call delay


      push 11    ;x  or col  or x-pos
	  push 23   ;y  or row  or y-pos
	  push 0x07
	  mov ax,copyrightNotice
	  push ax
	  call printStrWBI


mov cx,150
stuckOnLogo:
call delay
loop stuckOnLogo

call curtainEffect

pop ds
pop es
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp
ret 

	
	
generateRandomNumber:
	push ax
	push cx
	push dx
	
	mov cx,10
delayInGeneration:
	call delay
	loop delayInGeneration
	
	
again:

	mov ax,0
	mov cx,0
	mov dx,0

    ; Get the current time stamp counter value
    mov ah, 2Ch      ; DOS function to get system time
    int 21h          ; Call DOS interrupt
	
	
	;after calling the interrupt dl will then contain 100th value of second 0-99
	
	cmp dl,20
	jbe shapeOneSelect
	
	cmp dl,40
	jbe shapeTwoSelect
	
	cmp dl,60
	jbe shapeThreeSelect
	
	cmp dl,80
	jbe shapeFourSelect
	
	cmp dl,99
	jbe shapeFiveSelect
	
	
	
shapeOneSelect:
	mov word[shapeNo],1
	jmp finded
	
shapeTwoSelect:
	mov word[shapeNo],2
	jmp finded
	
shapeThreeSelect:
	mov word[shapeNo],3
	jmp finded
	
shapeFourSelect:
	mov word[shapeNo],4
	jmp finded
	
shapeFiveSelect:
	mov word[shapeNo],5
	jmp finded
		
finded:

	;;;THE RANDOM NO HAS BEEN SET IN RandomNumber variable

	;;;just for checking the generation of random is working properly or not by printing it on screen of dosbox
	
	; push 71   ;col or x
	; push 2    ;row or y
	; mov ax,[shapeNo]
	; push ax
	; call printNum

	; mov cx,3
	; slowww:
	; call delay
	; loop slowww
	
; jmp again

	
	pop dx
	pop cx
	pop ax
	ret
	
	
;;;SOUND is not implemented in game cuz of complexities but below code is a simple way to produce beep.... 
;-------------------------------------------------------------------------------------------------------------	
beep_function:           
    ; Parameters:
    ;   AH: Frequency (in Hz) to play
    ;   CX: Duration of the beep in milliseconds

    ; Save registers
    push ax
    push cx

    ; Generate a beep using BIOS (INT 18h)
    mov ah, 0
    mov al, ah        ; Frequency (in Hz) to play
    mov cx, cx        ; Duration of the beep in milliseconds
    int 18h

    ; Restore registers
    pop cx
    pop ax
    ret
	
;-------------------------------------------------------------------------------------------------------------
	
timer: 
	push ax
	
	  cmp word[gameTimerFlag],0
	  je bypassPrint
	
	inc word[cs: tickCount]
	cmp word[cs:tickCount],18
	je printSecondsOnScreen
	
	jmp bypassPrint
	
	printSecondsOnScreen:
	mov word[cs:tickCount],0
	mov ax,[currSeconds]
	inc ax
	mov [currSeconds],ax
	cmp word[currSeconds],900  ;sec of 15 mints
	jne continueToIncreaseTimer
	
	mov word[gameRunnningFlag],0
	mov word[gameTimerFlag],0
	continueToIncreaseTimer:
	call updateTimer
	
	push 0x0f
	mov ax,secLabel
    push ax
	call printStrWBIFastConsecutive
	
	bypassPrint:
	
	;interrupt channing
	mov al, 0x20
	out 0x20,al
	pop ax
	iret



kbisr:
	push ax
	push es
	
	mov ax,0xb800
	mov es,ax
	cmp word[keyboardFlag],0
	jne continues
	jmp bypassedAllKeys
	continues:
	
	cmp word[gameStartedFlag],0
	jne takeInputsOfGameplay
	jmp takeMenuKeys
	takeInputsOfGameplay:
	
	in al,0x60 ;read a character from the keyboard port
	cmp al,0x4b   ;cmp with left arrow key
	je bypass_27
	jmp nextcmp
	bypass_27:
	
	;-----------------------------------
	cmp word[horizontalMovBlockFlag],1
	jne skip_block_l
	jmp bypassedAllKeys
	skip_block_l:
	
	;------------------------------------
	
	;mov byte[horizontalKeyPress],'l'  ;for left movement ( l for left..)
			
		cmp word[currShapeNo],1
		jne bypassKey_1
		jmp leftShape_1
		bypassKey_1:
		
		cmp word[currShapeNo],2
		jne bypassKey_2
		jmp leftShape_2
		bypassKey_2:
		
		cmp word[currShapeNo],3
		jne bypassKey_3
		jmp leftShape_3
		bypassKey_3:
		
		cmp word[currShapeNo],4
		jne bypassKey_4
		jmp leftShape_4
		bypassKey_4:
		
		cmp word[currShapeNo],5
		jne bypassKey_5
		jmp leftShape_5
		bypassKey_5:
		
		
  leftShape_1:
  
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_1
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  sub ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_1
  
  	  cmp word[printAllow],1
	  je leftPossible_1
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x20   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_1
	  jmp leftMovementDone
	  
	  leftPossible_1:
	  ;left movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		sub ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x20   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_1
	  jmp leftMovementDone
	
	
	leftShape_2:
		
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  sub ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_2
  
  	  cmp word[printAllow],1
	  je leftPossible_2
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x30   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
	  jmp leftMovementDone
	  
	  leftPossible_2:
	  ;left movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		sub ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x30   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
	  jmp leftMovementDone	
	
	leftShape_3:
		
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  sub ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_3
  
  	  cmp word[printAllow],1
	  je leftPossible_3
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x40   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
	  jmp leftMovementDone
	  
	  leftPossible_3:
	  ;left movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		sub ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x40   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
	  jmp leftMovementDone	
		
	leftShape_4:
		
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  sub ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_4
  
  	  cmp word[printAllow],1
	  je leftPossible_4
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x60   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
	  jmp leftMovementDone
	  
	  leftPossible_4:
	  ;left movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		sub ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x60   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
	  jmp leftMovementDone	
	  
	leftShape_5:
		
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  sub ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_5
  
  	  cmp word[printAllow],1
	  je leftPossible_5
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x50   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
	  jmp leftMovementDone
	  
	  leftPossible_5:
	  ;left movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		sub ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x50   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
	  jmp leftMovementDone	
	  
	  leftMovementDone:
	  
	nextcmp:
		cmp al,0x4d
		je bypass_28
		jmp nomatch
		bypass_28:
				
				
	;-----------------------------------
	cmp word[horizontalMovBlockFlag],1
	jne skip_block_r
	jmp bypassedAllKeys
	skip_block_r:
	;------------------------------------
	
		;mov byte[horizontalKeyPress],'r'  ;for right movement ( r for right..)
		
	    cmp word[currShapeNo],1
		jne bypassKey_6
		jmp rightShape_1
		bypassKey_6:
		
		cmp word[currShapeNo],2
		jne bypassKey_7
		jmp rightShape_2
		bypassKey_7:
		
		cmp word[currShapeNo],3
		jne bypassKey_8
		jmp rightShape_3
		bypassKey_8:
		
		cmp word[currShapeNo],4
		jne bypassKey_9
		jmp rightShape_4
		bypassKey_9:
		
		cmp word[currShapeNo],5
		jne bypassKey_10
		jmp rightShape_5
		bypassKey_10:
		
	  rightShape_1:
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_1
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  add ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_1
  
  	  cmp word[printAllow],1
	  je rightPossible_1
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x20   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_1
	  jmp rightMovementDone
	  
	  rightPossible_1:
	  ;right movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		add ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x20   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_1
	  jmp rightMovementDone
	
	
	rightShape_2:
		
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  add ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_2
  
  	  cmp word[printAllow],1
	  je rightPossible_2
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x30   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
	  jmp rightMovementDone
	  
	  rightPossible_2:
	  ;right movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		add ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x30   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
	  jmp rightMovementDone	
	
	rightShape_3:
		
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  add ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_3
  
  	  cmp word[printAllow],1
	  je rightPossible_3
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x40   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
	  jmp rightMovementDone
	  
	  rightPossible_3:
	  ;right movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		add ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x40   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
	  jmp rightMovementDone	
		
	rightShape_4:
		
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  add ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_4
  
  	  cmp word[printAllow],1
	  je rightPossible_4
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x60   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
	  jmp rightMovementDone
	  
	  rightPossible_4:
	  ;right movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		add ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x60   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
	  jmp rightMovementDone	
	  
	rightShape_5:
		
      push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
  
		;checking the feasibility of shape movement...
	  mov word[printAllow],1
				
	  mov ax,[shapeCurrCol]
	  add ax,1
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call checkShape_5
  
  	  cmp word[printAllow],1
	  je rightPossible_5
	  
	  ;print the previous instance if movement not feasible or allowed...
	  push 0x20  ;ascii of space
      push 0x50   ;colourful colour
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
	  jmp rightMovementDone
	  
	  rightPossible_5:
	  ;right movement possible to changing the coordinates..
	    mov ax,[shapeCurrCol]
		add ax,1
		mov [shapeCurrCol],ax
		
	  push 0x20  ;ascii of space
      push 0x50   ;colourful color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
	  jmp rightMovementDone
	
	  rightMovementDone:
	  
		
  nomatch:
				
		cmp al,0x19
		jne startPause
		mov word[gamePause],1
		mov word[gameTimerFlag],0
		mov word[horizontalMovBlockFlag],1
		startPause:
		
		cmp al,0x18
		jne stopPause
		mov word[gamePause],0
		mov word[gameTimerFlag],1
		mov word[horizontalMovBlockFlag],0
		stopPause:
		
		cmp al,0xB9   ;SPACE KEY scanCode..
		je rotationTrigerred
		jmp noRotation
	    rotationTrigerred:
		mov ax,[shapeTypeIterator]
		mov [oldShapeType],ax
		
	  push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  
	  ;bypassing labels are irrespective of shapes numbered form 2 to onwards..they donot form semantic meaning with the no of shape...
	  cmp word[currShapeNo],1
	  jne bypassThatShape_1
	  call shape_1
	  bypassThatShape_1:
	  
	  cmp word[currShapeNo],2
	  jne bypassThatShape_2
	  call shape_2
	  
	  bypassThatShape_2:
	  
	  cmp word[currShapeNo],3
	  jne bypassThatShape_3
	  call shape_3
	  bypassThatShape_3:
	  
	  cmp word[currShapeNo],4
	  jne bypassThatShape_4
	  call shape_4
	  jmp setNextType
	  bypassThatShape_4:
	  
	  cmp word[currShapeNo],5
	  jne bypassThatShape_5
	  call shape_5
	  jmp setNextType
	  bypassThatShape_5:
  
  
  setNextType:
	mov ax,[shapeTypeIterator]
    inc ax
	cmp ax,5
	jne simpleShiftToNextType
	mov ax,1
	simpleShiftToNextType:
	mov word[shapeTypeIterator],ax
	
	mov word[printAllow],1
	
	mov ax,[shapeCurrCol]
	push ax
	mov ax,[shapeCurrRow]
	push ax
	
	cmp word[currShapeNo],1
	jne bypassShapeCheck_1
	call checkShape_1
	jmp checkingDone
	bypassShapeCheck_1:
	
	cmp word[currShapeNo],2
	jne bypassShapeCheck_2
	call checkShape_2
	jmp checkingDone
	bypassShapeCheck_2:
  
    cmp word[currShapeNo],3
	jne bypassShapeCheck_3
	call checkShape_3
	jmp checkingDone
	bypassShapeCheck_3:
	
	cmp word[currShapeNo],4
	jne bypassShapeCheck_4
	call checkShape_4
	jmp checkingDone
	bypassShapeCheck_4:
	
	cmp word[currShapeNo],5
	jne bypassShapeCheck_5
	call checkShape_5
	jmp checkingDone
	bypassShapeCheck_5:
	
	checkingDone:
	cmp word[printAllow],0
	jne nextTypeSetted   ;inc type will be used to form corresponding rotations...
	mov ax,[oldShapeType]
	mov [shapeTypeIterator],ax
	
	nextTypeSetted:   ;next type of corresponding shape is setted  successfully.....
	
  noRotation:
  
  
		cmp al,0x50
		jne checkOtherKey
		mov byte[verticalKeyPress],'d'  ;for forced down movement  ( d for down..)
		jmp controlOfVericalMovementIntended
		
		checkOtherKey:
		cmp al,0x48
		jne neitherOfUpOrDownKeyPressed
		mov byte[verticalKeyPress],'u'  ;for opposing down movement ( u for up..)
		jmp controlOfVericalMovementIntended
		
		neitherOfUpOrDownKeyPressed:
		mov byte[verticalKeyPress],' '  ;for normal movement..
		
		controlOfVericalMovementIntended:
		
		cmp al,0x13
		jne skipKeyPress_R
        mov word[decisionForReplay],1
		skipKeyPress_R:
  
  
		cmp al,0x10
		jne skipKeyPress_Q
        mov word[decisionForReplay],2
		skipKeyPress_Q:
		
		cmp al,0x81   ;esc key scanCode
		jne donotQuicklyQuit
        jmp gameQuitedScreen
		donotQuicklyQuit:
		
		cmp al,0xB6  ;space key   scanCode
		jne donotQuicklyClearShapes
        mov word[quickClearShapesFlag],1
		donotQuicklyClearShapes:
		
		jmp bypassedAllKeys
		takeMenuKeys:
		in al,0x60 ;read a character from the keyboard port
		
		cmp al,0x9C   ;enter key  scanCode
		jne donotConfirmChoice
        mov word[choiceConfirmedFlag],1
		mov word[gameStartedFlag],1
		donotConfirmChoice:
		
		cmp al,0x48   ;up key  scanCode
		jne bypassMenuKey_1
		mov ax,[selectionIterator]
		dec ax
		cmp ax,0
		jne simpleDecremented
		mov ax,3
		mov word[selectionIterator],ax
        simpleDecremented:	
        mov word[selectionIterator],ax		
		bypassMenuKey_1:
		
		cmp al,0x50   ;down key  scanCode
		jne bypassMenuKey_2
        mov ax,[selectionIterator]
		inc ax
		cmp ax,4
		jne simpleIncremented
		mov ax,1
		mov word[selectionIterator],ax
        simpleIncremented:	
		mov word[selectionIterator],ax
		bypassMenuKey_2:

		
		bypassedAllKeys:

		mov al,0x20   
		out 0x20,al   ;send EOI ( end of interrupt ) to PIC ( programmable interrupt handler )
		pop es
		pop ax
		iret
		



	
printStrWBI:      ;WBI stands for with block instruction meaning string instructions
		push bp
		mov bp,sp
		push es
		push ax
		push cx
		push si		
		push di
		
		push ds    ;for mapping extra segment to date segment location
		pop es     ;for mapping extra segment to date segment location
		
		mov di,[bp+4]    ;string address
		mov cx,0xffff    ;max length of the segment display memory...
		xor al,al        ;making al zero 
		repne scasb      ;will compare al(0) with the memory locations in es segment and search for zero to determine length of the string..
		mov ax,0xffff
		sub ax,cx        ;finding the length
	    dec ax           ;eliminating the length of the zero 
		jz exit          ;no printing because of empty string...
		
		mov cx,ax        ;actual counter for characters....
		mov ax,0xb800
		mov es,ax        ;display location memory setup!
		mov al,80
		mul byte[bp+8]
		add ax,[bp+10]
		shl ax,1
		mov di,ax         ;by default es:di (i.e. di is mapped with extra segment used for display memory...
		mov si,[bp+4]     ;storing the address of the string in si so that we can use the si reg in lods (load string command)
		mov ah,[bp+6]     ;attribute byte....
		
		cld
		
nextCharWBI:
		lodsb    ;load from memory to reg   (load next char from si(memory) in al) (it also shifts/inc si due to cld)
		stosw    ;store in memory location(display memory) by taking value from the reg...(print char/attt pair on screen)(transfer next byte from  al or ax(reg) in es:di(memory))(it also shifts/inc di due to cld)
		call delay
		loop nextCharWBI
		
		exit: 
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 8
		

printStrWBIFast:      ;WBI stands for with block instruction meaning string instructions
;this print is without delay so it instantly print the string on the desired location of the screen
		push bp
		mov bp,sp
		push es
		push ax
		push cx
		push si		
		push di
		
		push ds    ;for mapping extra segment to date segment location
		pop es     ;for mapping extra segment to date segment location
		
		mov di,[bp+4]    ;string address
		mov cx,0xffff    ;max length of the segment display memory...
		xor al,al        ;making al zero 
		repne scasb      ;will compare al(0) with the memory locations in es segment and search for zero to determine length of the string..
		mov ax,0xffff
		sub ax,cx        ;finding the length
	    dec ax           ;eliminating the length of the zero 
		jz exitFast          ;no printing because of empty string...
		
		mov cx,ax        ;actual counter for characters....
		mov ax,0xb800
		mov es,ax        ;display location memory setup!
		mov al,80
		mul byte[bp+8]
		add ax,[bp+10]
		shl ax,1
		mov di,ax         ;by default es:di (i.e. di is mapped with extra segment used for display memory...
		mov si,[bp+4]     ;storing the address of the string in si so that we can use the si reg in lods (load string command)
		mov ah,[bp+6]     ;attribute byte....
		
		cld
		
nextCharWBIFast:
		lodsb    ;load from memory to reg   (load next char from si(memory) in al) (it also shifts/inc si due to cld)
		stosw    ;store in memory location(display memory) by taking value from the reg...(print char/attt pair on screen)(transfer next byte from  al or ax(reg) in es:di(memory))(it also shifts/inc di due to cld)
		loop nextCharWBIFast
		
		exitFast: 
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 8
		
		
		
	printStrWBIFastConsecutive:      ;WBI stands for with block instruction meaning string instructions
    ;this print is without delay so it instantly print the string on the desired location of the screen
	;consecutive is used for writing on the next location of some digit such has 2 sec
		push bp
		mov bp,sp
		push es
		push ax
		push cx
		push si		
		push di
		
		push ds    ;for mapping extra segment to date segment location
		pop es     ;for mapping extra segment to date segment location
		
		mov di,[bp+4]    ;string address
		mov cx,0xffff    ;max length of the segment display memory...
		xor al,al        ;making al zero 
		repne scasb      ;will compare al(0) with the memory locations in es segment and search for zero to determine length of the string..
		mov ax,0xffff
		sub ax,cx        ;finding the length
	    dec ax           ;eliminating the length of the zero 
		jz exitFastConsecutive          ;no printing because of empty string...
		
		mov cx,ax        ;actual counter for characters....
		mov ax,0xb800
		mov es,ax        ;display location memory setup!
		
		mov di,[consecutiveLocation]         ;by default es:di (i.e. di is mapped with extra segment used for display memory...
		mov si,[bp+4]     ;storing the address of the string in si so that we can use the si reg in lods (load string command)
		mov ah,[bp+6]     ;attribute byte....
		
		cld
		
nextCharWBIFastConsecutive:
		lodsb    ;load from memory to reg   (load next char from si(memory) in al) (it also shifts/inc si due to cld)
		stosw    ;store in memory location(display memory) by taking value from the reg...(print char/attt pair on screen)(transfer next byte from  al or ax(reg) in es:di(memory))(it also shifts/inc di due to cld)
		loop nextCharWBIFastConsecutive
		
		exitFastConsecutive: 
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 4
		
curtainEffect:
		push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si		
		push di
	  

	  mov ax,0xb800
	  mov es,ax
	  
ShiftCols:
	  
	  mov ax,0
	  mov [curtainEffectCurrRow],ax
	  
	  
	verticalLines:
	  mov al,80
	  mul byte[curtainEffectCurrRow]
	  add ax,[curtainEffectCol1]
	  shl ax,1
	  mov di,ax
	  
	  mov ax,0x0020
	  mov [es:di],ax
	  	  
	  
	  mov al,80
	  mul byte[curtainEffectCurrRow]
	  add ax,[curtainEffectCol2]
	  shl ax,1
	  mov di,ax
	  
	  mov ax,0x0020
	  mov [es:di],ax
	  
	  
	  mov ax,[curtainEffectCurrRow]
	  inc ax
	  mov [curtainEffectCurrRow],ax
	  cmp ax,25
	  jne verticalLines
	  call shortDelay
	  
	  mov bx,[curtainEffectCol1]
	  inc bx
	  mov [curtainEffectCol1],bx
	  
	  mov bx,[curtainEffectCol2]
	  dec bx
	  mov [curtainEffectCol2],bx
	  
	  cmp word[curtainEffectCol2],39
	  jne ShiftCols
	  
	  
	  
	  ;resetting curtainEffect initial values of cols and row.......every time we have to do it before calling 
	  ;thats why we write once here for code reuseability and also this is the job of this func to clear up its things
	  ;its not the job of the caller..its the job of callee(this function..) 
	  mov ax,0
	  mov [curtainEffectCurrRow],ax
	  mov ax,0
	  mov [curtainEffectCol1],ax
	  mov ax,79
	  mov [curtainEffectCol2],ax

		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		ret 
		
		
gameQuitedScreen:
		push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si		
		push di
		
	  mov ax,0xb800
	  mov es,ax
	  
	  mov ax,0x4020
	  push ax
	  call screenFlash
				
	  mov ax,11
	  push ax                      ;[bp+10]
	  mov ax,12
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,gameQuitedMesg   
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
	  mov cx,25
	  waitingOnQuitScreen:
	  call delay
	  loop waitingOnQuitScreen	  
	  
	  call curtainEffect
	  
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		ret 
		
		
screenFlash:
		push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si		
		push di
		
	  mov ax,0xb800
	  mov es,ax
	  
	    mov ax,[bp+4]   
		mov di,0
        makingColour:
		mov word[es:di],ax
		add di,2
		cmp di,4000
		jne makingColour
		
		call delay
		call delay	
		
		mov ax,0x0020
        makingBlack:
		mov word[es:di],ax
		sub di,2
		cmp di,0
		jne makingBlack
		mov word[es:di],ax
				
	  
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		ret 2

printNum:
        push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push di

        mov ax,0xb800
		mov es,ax
		mov ax,[bp+4]
		mov bx,10
		mov cx,0
		

		
	nextDigit:
	        mov dx,0
			div bx
			add dl,0x30
			push dx
			inc cx
			cmp ax,0
			jnz nextDigit
			
			;specific screen location finded out 
			mov ax,80
			mul word[bp+6]    ;always assending order if we are following three imp principles
			add ax,[bp+8]
			shl ax,1
			mov di,ax
			
	nextPos:	
			pop dx
			mov dh,0x0F
			mov [es:di],dx
			add di,2
			loop nextPos
			
			mov [consecutiveLocation],di
			
	
	pop di 
	pop dx
	pop cx 
	pop bx
	pop ax
	pop es
	pop bp
	ret 6


drawBoundary:
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push di

	mov ax,0xb800
	mov es,ax
	
	mov ax,0
	
	mov ax,[bp+8]	
	mov [currentRowL],ax
	mov ax,[bp+10]
	mov [currentColL],ax
	
	mov ax,[bp+4]	
	mov [currentRowR],ax
	mov ax,[bp+6]
	mov [currentColR],ax
	
	
	mov cx,24   ;counter for 25 rows	

printVertical:

	mov ax,80
	mul word[currentRowL]
	add ax,[currentColL]
	shl ax,1
	mov di,ax

	mov ah,[bp+12]
	mov al,[symbol]
	mov word[es:di],ax
	
	
	mov ax,80
	mul word[currentRowR]
	add ax,[currentColR]
	shl ax,1
	mov di,ax

	mov ah,[bp+12]
	mov al,[symbol]
	mov word[es:di],ax

	add word[currentRowL],1   ;shift to the other ( new ) row 
	add word[currentRowR],1   ;shift to the other ( new ) row 
	
	call shortDelay
	call shortDelay
	
	
	
	
	dec cx
	jnz printVertical
	
	;---now drawing bottom boundary
	
	;NOW note that in horizontal boundary making:
	;each iteration make two characters so counter will be half the no of cols in between 
	;(means you need to half the diff of cols points in our case 49-22 = 27/2 = 13.5 so we will 14 as counter)
	
	mov cx,14  
	mov word[currentRowL],24
	mov word[currentRowR],24
	mov word[currentColL],22
	mov word[currentColR],49
	

 printHorizontal:

	mov ax,80
	mul word[currentRowL]
	add ax,[currentColL]
	shl ax,1
	mov di,ax

	mov ah,0x70  ;40
	mov al,0x20
	mov word[es:di],ax
	
	
	mov ax,80
	mul word[currentRowR]
	add ax,[currentColR]
	shl ax,1
	mov di,ax

	mov ah,0x70  ;40
	mov al,0x20
	mov word[es:di],ax

	add word[currentColL],1   ;shift to the other ( new ) col
	sub word[currentColR],1   ;shift to the other ( new ) col
	
	call shortDelay
	
	
	dec cx
	jnz printHorizontal
	
	
	;FINDING NO OF THE MAIN FRAME PIXELS OR CELLS IN BETWEEN THE BOUNDARIES FOR USAGE IN SCREEN SCAN CODE....		
		
	; mov ax,0
	; mov al,80
	; mov bl,1
	; mul bl
	; add ax,1
	; shl ax,1
	; mov di,ax
	; mov word[es:di],0x7020
	
	; mov ax,0
	; mov al,80
	; mov bl,1
	; mul bl
	; add ax,60
	; shl ax,1
	; mov di,ax
	; mov word[es:di],0x7020
	
	
	;proof  ( 162 is the starting of main frame of our first required search row )  
	;161 is false location it si actually half byte of 160's character and half of 162's character 
	;( 160 is the first character location and 162 is the second character location )
	;in our case 160 is the boundary cell whereas 162 is the first character in row of main frame...
	; mov di,162    
	; mov word[es:di],0x7020
	
	
			
	pop di
	pop bx
	pop ax
	pop es
	pop bp
	ret 10
	
	
	
makeDashes:
      push bp
      mov bp,sp
	  push ax
	  
	  mov ax,[bp+6]
	  push ax                      
	  mov ax,[bp+4]
	  push ax                      
	  mov ax,[bp+8]
	  push ax                      
	  mov ax,dashes
	  push ax                      
	  call printStrWBI
	  
	  pop ax
	  pop bp
	  ret 6

 
 
updateScore:
      push ax
	  
	  push 72   ;col or x
	  push 2    ;row or y
	  mov ax,[currScore]
	  push ax
	  call printNum
	  
	  pop ax
	  ret

updateTimer:   ;not used very offen in program ( made it in the initial days of game ) 
      push ax

	  push 72   ;col or x
	  push 4    ;row or y
	  mov ax,[currSeconds]
	  push ax
	  call printNum
	  
	  pop ax
	  ret



printScore:   ;in game results
        push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si		
		push di
	  
	  push 3    ;x  or col  or x-pos
	  push 6   ;y  or row  or y-pos
	  push 0x02
	  mov ax,scoreLabel
	  push ax
	  call printStrWBI


	  push word[bp+6]   ;col or x
	  push word[bp+4]    ;row or y
	  mov ax,[currScore]
	  push ax
	  call printNum
	  
	    pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
	    ret 4

printTime:     ;in game results
        push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si		
		push di
	  
	  push 3    ;x  or col  or x-pos
	  push 8   ;y  or row  or y-pos
	  push 0x09
	  mov ax,timerLabel
	  push ax
	  call printStrWBI


	  push word[bp+6]   ;col or x
	  push word[bp+4]    ;row or y
	  mov ax,[currSeconds]
	  push ax
	  call printNum
	  
	  
	  push 0x0f
	  mov ax,secLabel
	  push ax
	  call printStrWBIFastConsecutive
	  
	    pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
	    ret 4
		

printGameNameAndInstructions:   ;print controls or rules of the game 
;reg values preserved!
push ax
push cx

     call clrScr
	  ;printing tetris logo here..
	  
	  
	  
	  
	  ;printing developerInfo
	  
	  push 27    ;x  or col  or x-pos
	  push 11   ;y  or row  or y-pos
	  push 0x07
	  mov ax,developedBy
	  push ax
	  call printStrWBIFast
	  
	  
	  push 24    ;x  or col  or x-pos
	  push 14   ;y  or row  or y-pos
	  push 0x0A
	  mov ax,developerName
	  push ax
	  mov cx,20  
	  waiting1:     ;(take some time before printing and avoid immediate printing for creating seemless user-exprience)
	  call delay
	  loop waiting1
	  call printStrWBI
	  
	  mov cx,60
	  waitForSomeTimeBeforeErasing:
	  call delay
	  loop waitForSomeTimeBeforeErasing
	  
	  
	  call curtainEffect
	  
	  ;printing info regarding game...
	  
	  push 27    ;x  or col  or x-pos
	  push 11   ;y  or row  or y-pos
	  push 0x07
	  mov ax,gameInfo_A
	  push ax
	  mov cx,10  
	  waitForgameInfo_A:     ;(take some time before printing and avoid immediate printing for creating seemless user-exprience)
	  call delay
	  loop waitForgameInfo_A
	  call printStrWBI
	  
	  push 16    ;x  or col  or x-pos
	  push 14   ;y  or row  or y-pos
	  push 0x0c
	  mov ax,gameInfo_B
	  push ax
	  mov cx,10  
	  waitForgameInfo_B:     ;(take some time before printing and avoid immediate printing for creating seemless user-exprience)
	  call delay
	  loop waitForgameInfo_B
	  call printStrWBI
	  
	  mov cx,70  
	  waitBeforeBlack:     ;(take some time before printing and avoid immediate printing for creating seemless user-exprience)
	  call delay
	  loop waitBeforeBlack
	  
	  call curtainEffect
	  
	  
	  
	  ;display instruction (controls for the game...)
	  
	  
	  mov ax,14
	  push ax                      ;[bp+10]
	  mov ax,2
	  push ax                      ;[bp+8]
	  mov ax,0x0B
	  push ax                      ;[bp+6]
	  mov ax,instructionsLabel     
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
	   
	   
	  mov ax,1
	  push ax                      ;[bp+10]
	  mov ax,5
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,bullet    
	  push ax                      ;[bp+4]
	  call printStrWBI
	   
	   
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,5
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,instruction1    
	  push ax                      ;[bp+4]
	  call printStrWBI
	   
	  mov ax,1
	  push ax                      ;[bp+10]
	  mov ax,7
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,bullet    
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,7
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,instruction2     
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
	  mov ax,1
	  push ax                      ;[bp+10]
	  mov ax,9
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,bullet    
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,9
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,instruction3     
	  push ax                      ;[bp+4]
	  call printStrWBI
	  	  
	  mov ax,1
	  push ax                      ;[bp+10]
	  mov ax,11
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,bullet    
	  push ax                      ;[bp+4]
	  call printStrWBI
	   
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,11
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,instruction4   
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
	  mov ax,1
	  push ax                      ;[bp+10]
	  mov ax,13
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,bullet    
	  push ax                      ;[bp+4]
	  call printStrWBI
	   
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,13
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,instruction5   
	  push ax                      ;[bp+4]
	  call printStrWBI
	
	  mov ax,1
	  push ax                      ;[bp+10]
	  mov ax,15
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,bullet    
	  push ax                      ;[bp+4]
	  call printStrWBI
	   
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,15
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,instruction6  
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
	  
	
	  mov ax,1
	  push ax                      ;[bp+10]
	  mov ax,17
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,bullet    
	  push ax                      ;[bp+4]
	  call printStrWBI
	   
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,17
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,instruction7  
	  push ax                      ;[bp+4]
	  call printStrWBI 
	  
	
	  mov ax,1
	  push ax                      ;[bp+10]
	  mov ax,19
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,bullet    
	  push ax                      ;[bp+4]
	  call printStrWBI
	   
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,19
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,instruction8  
	  push ax                      ;[bp+4]
	  call printStrWBI 
	  
	  mov ax,1
	  push ax                      ;[bp+10]
	  mov ax,21
	  push ax                      ;[bp+8]
	  mov ax,0x04
	  push ax                      ;[bp+6]
	  mov ax,bullet    
	  push ax                      ;[bp+4]
	  call printStrWBI
	   
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,21
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,instruction9  
	  push ax                      ;[bp+4]
	  call printStrWBI 
	
	 
	
	
      mov ax,9
	  push ax                      ;[bp+10]
	  mov ax,24
	  push ax                      ;[bp+8]
	  mov ax,0x0C
	  push ax                      ;[bp+6]
	  mov ax,instruction10  
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
	  mov ah,0
	  int 16h
	  call curtainEffect
	



	  
	  pop cx
	  pop ax
	  ret



initialSetupOfGame:
;used reg's (modifying one's) value preserved on stack 
push ax  
push cx
	   
;MAKING INITAL SETUP PRINTING OF BOUNDARIES AND SCORE AND TIMER LABEL PRINTING...  
	  
	  push 0x40
	  push 22    ;x1  or col  or x-pos
	  push 0    ;y1  or row  or y-pos
	  push 49   ;x2  or col  or x-pos
	  push 0    ;y2  or row  or y-pos
	  call drawBoundary
	  
	 
	 
	  push 64    ;x  or col  or x-pos
	  push 2   ;y  or row  or y-pos
	  push 0x0A
	  mov ax,scoreLabel
	  push ax
	  mov cx,15  
	  waiting3:     
	  call delay
	  loop waiting3
	  call printStrWBI
	  
	  mov ax,0x07  ;0x0D	  
	  push ax
	  push 63
	  push 1
	  call makeDashes
	  call updateScore
	  
	  mov ax,0x07  ;0x0E  
	  push ax
	  push 63
	  push 3
	  call makeDashes
	  
	  push 64    ;x  or col  or x-pos
	  push 4   ;y  or row  or y-pos
	  push 0x09  ;0x0c
	  mov ax,timerLabel
	  push ax
	  mov cx,15  
	  waiting4:     
	  call delay
	  loop waiting4
	  call printStrWBI
	  call updateTimer
	  
	  
	  mov ax,0x07  ;0x0F  
	  push ax
	  push 63
	  push 5
	  call makeDashes
	  
	  
	  push 65    ;x  or col  or x-pos   ;67
	  push 14   ;y  or row  or y-pos
	  push 0xC0   ;0x40 to stop blinking 
	  mov ax,NextShapeLabel
	  push ax
	  mov cx,15  
	  waiting5:    
	  call delay
	  loop waiting5
	  call printStrWBI
	  
pop cx
pop ax
ret

gameOver:
	push es
	push ds
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	
	mov ax,0xb800
	mov es,ax
	
	;resetting multiple entities
	mov word[gameRunnningFlag],0
	mov word[decisionForReplay],0
	mov word[keyboardFlag],1
	
	call curtainEffect
	
	call delay
	call delay
	call delay
	call delay	
	call delay
	call delay
			
	  push 19    ;x  or col  or x-pos
	  push 12   ;y  or row  or y-pos
	  push 0x07
	  mov ax,gameOverLabel
	  push ax
	  mov cx,35  
	  waiting6:    
	  call delay
	  loop waiting6
	  call printStrWBI
	    		
	  mov cx,3 
	  waiting7:    
	  call delay
	  loop waiting7
	  		
	  push 19    ;x  or col  or x-pos
	  push 12   ;y  or row  or y-pos
	  push 0x0a
	  mov ax,gameOverLabel
	  push ax
	  call printStrWBIFast
	  
	  mov cx,3 
	  waiting8:    
	  call delay
	  loop waiting8
	  
	  push 19    ;x  or col  or x-pos
	  push 12   ;y  or row  or y-pos
	  push 0x09
	  mov ax,gameOverLabel
	  push ax
	  call printStrWBIFast
	  
	  mov cx,3  
	  waiting9:    
	  call delay
	  loop waiting9
	  
	  push 19    ;x  or col  or x-pos
	  push 12   ;y  or row  or y-pos
	  push 0x0e
	  mov ax,gameOverLabel
	  push ax
	  call printStrWBIFast
	  
	  mov cx,3  
	  waiting10:    
	  call delay
	  loop waiting10
	  
	  push 19    ;x  or col  or x-pos
	  push 12   ;y  or row  or y-pos
	  push 0x04
	  mov ax,gameOverLabel
	  push ax
	  call printStrWBIFast
	  
	  mov cx,50
	  waiting11:    
	  call delay
	  loop waiting11
	  
	  call curtainEffect
	  
	  
	  ;------------------game results printing-------------------
	  
	  mov cx,35  
	  waiting12:    
	  call delay
	  loop waiting12
	  
	  push 9    ;x  or col  or x-pos
	  push 3   ;y  or row  or y-pos
	  push 0x50
	  mov ax,GameResultsLabel
	  push ax
	  call printStrWBI
	  
	  
	  push 11    ;x or col
	  push 6    ;y or row
	  call printScore
	  
	  push 11    ;x or col
	  push 8    ;y or row
	  call printTime
	  
	  
	  
	  
	  ;-----------------------------------------------	 
	  mov cx,35
	  waiting13:    
	  call delay
	  loop waiting13
	  
	  push 4    ;x  or col  or x-pos
	  push 15   ;y  or row  or y-pos
	  push 0x0a
	  mov ax,GameResultsInstruction1
	  push ax
	  call printStrWBI
	  
	  
	  push 4    ;x  or col  or x-pos
	  push 17  ;y  or row  or y-pos
	  push 0x0c
	  mov ax,GameResultsInstruction2
	  push ax
	  call printStrWBI
	  
	  
	
	 waitingForChoice:
	 
	 ;KEYPRESS FUNC
	  xor ax,ax
	  mov es,ax
	  
	  cli
	  mov word[es:9*4],kbisr
	  mov [es:9*4+2],cs
	  sti
	
	cmp word[decisionForReplay],1
	jne checkOtherChoice
    mov ax,0x2020
	push ax
	call screenFlash
	jmp gameStart
	
 	checkOtherChoice:
	
	cmp word[decisionForReplay],2
	je endTheGame
 	
	jmp waitingForChoice
	
endTheGame:
    call gameQuitedScreen	
	
	pop di
	pop si
	pop	dx
	pop cx
	pop bx
	pop ax
	pop ds
	pop es
	ret


shape_1:  ;no need to implement differennt rotating logical codes because it remains same in all possible types of rotation..
    push bp
	mov bp,sp
	push es
	push ds
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	
	
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,2
	
	making_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[bp+10]        ;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_1
	
	
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop ds
	pop es
	pop bp
	ret 8
	
	
shape_2:            ;two different types will be formed due to rotation...
    push bp
	mov bp,sp
	push es
	push ds
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	cmp word[shapeTypeIterator],1
	je shape_2_type_1
	cmp word[shapeTypeIterator],3
	je shape_2_type_1
	jmp shape_2_type_2
	
	
	shape_2_type_1:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,8
	
	making_2_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_2_type_1
	jmp done_2
	;--------------------
	
	shape_2_type_2:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,4
	
	making_2_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	add di,2
	mov word[es:di],ax
	
	inc bx   ;next row shift
	dec si
	cmp si,0
	jne making_2_type_2
	
	done_2
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop ds
	pop es
	pop bp
	ret 8
	
	
	shape_3:
    push bp
	mov bp,sp
	push es
	push ds
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	cmp word[shapeTypeIterator],1
	jne checkOtherType_1
	jmp shape_3_type_1
	checkOtherType_1:
	
	cmp word[shapeTypeIterator],2
	jne checkOtherType_2
	jmp shape_3_type_2
	checkOtherType_2:
	
	cmp word[shapeTypeIterator],3
	jne checkOtherType_3
	jmp shape_3_type_3
	checkOtherType_3:
	
	jmp shape_3_type_4  ;because it is the last one option so need to check...

	
	shape_3_type_1:
	;because we always first push x(col) then y(row) so in our case accessing is like that in asscending order..
	mov bx,[bp+4]    ;bx deals with row ( is row no )
	mov cx,[bp+6]    ;cx deals with col ( is col no )
	mov si,2
	
	making_3a_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_3a_type_1
	
	mov si,2
	sub cx,4
	
	making_3b_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;shift next ( go to next col )
	
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	dec cx  ;reset ( back to previous col )
	
	dec bx   ;prev row shift
	dec si
	cmp si,0
	jne making_3b_type_1
	jmp done_3
	
	;-----------------
	
	shape_3_type_2:
	;because we always first push x(col) then y(row) so in our case accessing is like that in asscending order..
	mov bx,[bp+4]    ;bx deals with row ( is row no )
	mov cx,[bp+6]    ;cx deals with col ( is col no )
	mov si,2
	
	making_3a_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_3a_type_2
	
	mov si,2
	sub cx,4
	
	making_3b_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;shift next ( go to next col )
	
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	dec cx  ;reset ( back to previous col )
	
	inc bx   ;prev row shift
	dec si
	cmp si,0
	jne making_3b_type_2
	jmp done_3
	;------------------
	
	shape_3_type_3:
	;because we always first push x(col) then y(row) so in our case accessing is like that in asscending order..
	mov bx,[bp+4]    ;bx deals with row ( is row no )
	mov cx,[bp+6]    ;cx deals with col ( is col no )
	mov si,2
	
	making_3a_type_3:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_3a_type_3
	
	mov si,2
	
	making_3b_type_3:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;shift next ( go to next col )
	
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	dec cx  ;reset ( back to previous col )
	
	inc bx   ;prev row shift
	dec si
	cmp si,0
	jne making_3b_type_3
	jmp done_3
	;---------------------------
	shape_3_type_4:
	;because we always first push x(col) then y(row) so in our case accessing is like that in asscending order..
	mov bx,[bp+4]    ;bx deals with row ( is row no )
	mov cx,[bp+6]    ;cx deals with col ( is col no )
	mov si,2
	
	making_3a_type_4:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_3a_type_4
	
	mov si,2
	
	making_3b_type_4:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;shift next ( go to next col )
	
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	dec cx  ;reset ( back to previous col )
	
	dec bx   ;prev row shift
	dec si
	cmp si,0
	jne making_3b_type_4
	
	
	done_3:
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop ds
	pop es
	pop bp
	ret 8
	
	shape_4:
    push bp
	mov bp,sp
	push es
	push ds
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	cmp word[shapeTypeIterator],1
	jne checkOtherType_4
	jmp shape_4_type_1
	checkOtherType_4:
	
	cmp word[shapeTypeIterator],2
	jne checkOtherType_5
	jmp shape_4_type_2
	checkOtherType_5:
	
	cmp word[shapeTypeIterator],3
	jne checkOtherType_6
	jmp shape_4_type_3
	checkOtherType_6:
	
	jmp shape_4_type_4  ;because it is the last one option so need to check...

	
	shape_4_type_1:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,6
	
	making_4a_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_4a_type_1
	
	
	mov si,2   ; want to make one block( two characters will be used in our case)
	sub cx,4   ;to go back 4 units ( for making the upper part of the inverted T shape meaning vertical part)  
	dec bx     ;dec row number 
	
	making_4b_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_4b_type_1
	jmp done_4
	;-----------------
	shape_4_type_2:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,3
	
	making_4a_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	add di,2
	mov word[es:di],ax
	
	inc bx   
	dec si
	cmp si,0
	jne making_4a_type_2
	
	
	mov si,2   
	add cx,2   
	sub bx,2     
	
	making_4b_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_4b_type_2
	jmp done_4
	;-----------------
	
	shape_4_type_3:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,6
	
	making_4a_type_3:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_4a_type_3
	
	
	mov si,2   ; want to make one block( two characters will be used in our case)
	sub cx,4   ;to go back 4 units ( for making the upper part of the inverted T shape meaning vertical part)  
	inc bx     ;dec row number 
	
	making_4b_type_3:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_4b_type_3
	jmp done_4
	;-----------------
	
	shape_4_type_4:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,3
	
	making_4a_type_4:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	add di,2
	mov word[es:di],ax
	
	inc bx   
	dec si
	cmp si,0
	jne making_4a_type_4
	
	
	mov si,2  
	sub cx,2   
	sub bx,2    
	
	making_4b_type_4:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_4b_type_4
	
	done_4:
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop ds
	pop es
	pop bp
	ret 8

	
shape_5:
    push bp
	mov bp,sp
	push es
	push ds
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	cmp word[shapeTypeIterator],1
	je shape_5_type_1
	cmp word[shapeTypeIterator],3
	je shape_5_type_1
	jmp shape_5_type_2
	
	
	shape_5_type_1:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,4
	
	making_5a:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_5a
	
	sub cx,3  ;go back to four units in cols    we have not make from second last col thats why we subtracted 3  
	          ;because we want to draw a blank char on third last col and then mov back and make 4 blank chars on previous side
	dec bx    ;go to previous row 
	mov si,4   ; want to make one block( two characters will be used in our case)
	
	making_5b:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	dec cx   ;previous col shift
	dec si
	cmp si,0
	jne making_5b
	jmp done_5
   ;--------------------
   
	shape_5_type_2:
	mov bx,[bp+4]  ;bx is row
	mov cx,[bp+6]  ;cx is col
	mov si,4
	
	making_5a_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_5a_type_2
	
	sub cx,1
	dec bx    ;go to previous row 
	mov si,2   ; want to make one block( two characters will be used in our case)
	
	making_5b_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	dec cx   ;previous col shift
	dec si
	cmp si,0
	jne making_5b_type_2
	
	add bx,2    ;go to previous row 
	mov si,2   ; want to make one block( two characters will be used in our case)
	
	making_5c_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	mov ah,[bp+8]
	mov al,[blank]    ;;can also use blank here...
	
	mov word[es:di],ax
	
	dec cx   ;previous col shift
	dec si
	cmp si,0
	jne making_5c_type_2
	
	
	done_5:
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop ds
	pop es
	pop bp
	ret 8
	
		
	
checkShape_1:     ; THIS IS THE FUNC WHICH IS NOT WORKING OR COMPARING ( it doesnot read the character )
    push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	mov bx,[bp+4]    ;y or row
	mov cx,[bp+6]    ;x or col
	mov si,2        ;counter
		
	
	checkMaking_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax
    


    cmp word[es:di],0x0020    ;work to be done here( mov in ax then compare ah ( att of both colors only not ASCII )
	                          ;because we have printed black text on its previous printing to erase it 
	
	jne stopShape_1
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_1
	
	 jmp possible_1
	
	stopShape_1:     ;set the flag to false or zero means printing is not feasible here....!
	mov word[printAllow],0	
	
    possible_1:
		
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 4

	
		
checkShape_2:
    push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	cmp word[shapeTypeIterator],1
	je shape_2_checkType_1
	cmp word[shapeTypeIterator],3
	je shape_2_checkType_1
	jmp shape_2_checkType_2

	shape_2_checkType_1:
	mov bx,[bp+4]  ;bx is row
	mov cx,[bp+6]  ;cx is col
	mov si,8
	
	checkMaking_2_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_2_type_1
		
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_2_type_1
	jmp possible_2_type_1
	
	stopShape_2_type_1:
	mov word[printAllow],0
	possible_2_type_1:
	jmp checkDone_2
	;--------------------
	
	shape_2_checkType_2:
	mov bx,[bp+4]  ;bx is row
	mov cx,[bp+6]  ;cx is col
	mov si,4
	
	checkMaking_2_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_2_type_2
	add di,2
	cmp word[es:di],0x0020
	jne stopShape_2_type_2
		
	inc bx   ;next row shift
	dec si
	cmp si,0
	jne checkMaking_2_type_2
	jmp possible_2_type_2
	
	stopShape_2_type_2:
	mov word[printAllow],0
	possible_2_type_2:
	
	checkDone_2:
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 4
	
	
checkShape_3: 

    push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	cmp word[shapeTypeIterator],1
	jne checkNextType_1
	jmp shape_3_checkType_1
	checkNextType_1:
	
	
	cmp word[shapeTypeIterator],2
	jne checkNextType_2
	jmp shape_3_checkType_2
	checkNextType_2:
	
	
	cmp word[shapeTypeIterator],3
	jne checkNextType_3
	jmp shape_3_checkType_3
	checkNextType_3:
	
	 jmp shape_3_checkType_4  ;no need to compare for this one because it is left one so obviously it will be the desired case if no above one run's

	
	
	
	shape_3_checkType_1:
	;because we always first push x(col) then y(row) so in our case accessing is like that in asscending order..
	mov bx,[bp+4]    ;bx deals with row ( is row no )
	mov cx,[bp+6]    ;cx deals with col ( is col no )
	mov si,2
	
	checkMaking_3a_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_1
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_3a_type_1
	
	mov si,2
	sub cx,4
	
	checkMaking_3b_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_1
	
	inc cx   ;shift next ( go to next col )
	
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_1
	
	dec cx  ;reset ( back to previous col )
	
	dec bx   ;prev row shift
	dec si
	cmp si,0
	jne checkMaking_3b_type_1
	
	
	jmp possible_3_type_1
	
	stopShape_3_type_1:
	mov word[printAllow],0
	
	possible_3_type_1:
	jmp checkDone_3
	;------------------------
	
	
	shape_3_checkType_2:
	;because we always first push x(col) then y(row) so in our case accessing is like that in asscending order..
	mov bx,[bp+4]    ;bx deals with row ( is row no )
	mov cx,[bp+6]    ;cx deals with col ( is col no )
	mov si,2
	
	checkMaking_3a_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_2
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_3a_type_2
	
	mov si,2
	sub cx,4
	
	checkMaking_3b_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_2
	
	inc cx   ;shift next ( go to next col )
	
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_2
	
	dec cx  ;reset ( back to previous col )
	
	inc bx   ;prev row shift
	dec si
	cmp si,0
	jne checkMaking_3b_type_2
	
	
	jmp possible_3_type_2
	
	stopShape_3_type_2:
	mov word[printAllow],0
	
	possible_3_type_2:
	jmp checkDone_3
	
	;------------------------
	
	shape_3_checkType_3:
	;because we always first push x(col) then y(row) so in our case accessing is like that in asscending order..
	mov bx,[bp+4]    ;bx deals with row ( is row no )
	mov cx,[bp+6]    ;cx deals with col ( is col no )
	mov si,2
	
	checkMaking_3a_type_3:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_3
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_3a_type_3
	
	mov si,2
	
	checkMaking_3b_type_3:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_3
	
	inc cx   ;shift next ( go to next col )
	
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_3
	
	dec cx  ;reset ( back to previous col )
	
	inc bx   ;prev row shift
	dec si
	cmp si,0
	jne checkMaking_3b_type_3
	
	
	jmp possible_3_type_3
	
	stopShape_3_type_3:
	mov word[printAllow],0
	
	possible_3_type_3:
	jmp checkDone_3
	;-------------------
	shape_3_checkType_4:
	;because we always first push x(col) then y(row) so in our case accessing is like that in asscending order..
	mov bx,[bp+4]    ;bx deals with row ( is row no )
	mov cx,[bp+6]    ;cx deals with col ( is col no )
	mov si,2
	
	checkMaking_3a_type_4:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_4
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_3a_type_4
	
	mov si,2
	
	checkMaking_3b_type_4:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_4
	
	inc cx   ;shift next ( go to next col )
	
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_3_type_4
	
	dec cx  ;reset ( back to previous col )
	
	dec bx   ;prev row shift
	dec si
	cmp si,0
	jne checkMaking_3b_type_4

	jmp possible_3_type_4
	
	stopShape_3_type_4:
	mov word[printAllow],0
	
	possible_3_type_4:
	jmp checkDone_3
	
	
	checkDone_3:
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 4
	
	
checkShape_4:
    push bp
	mov bp,sp
	push es
	push ds
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	
	cmp word[shapeTypeIterator],1
	jne checkNextType_4
	jmp shape_4_checkType_1
	checkNextType_4:
	
	
	cmp word[shapeTypeIterator],2
	jne checkNextType_5
	jmp shape_4_checkType_2
	checkNextType_5:
	
	
	cmp word[shapeTypeIterator],3
	jne checkNextType_6
	jmp shape_4_checkType_3
	checkNextType_6:
	
	 jmp shape_4_checkType_4  ;no need to compare for this one because it is left one so obviously it will be the desired case if no above one run's

	shape_4_checkType_1:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,6
	
	checkMaking_4a_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_4_type_1
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_4a_type_1
	
	
	mov si,2   ; want to make one block( two characters will be used in our case)
	sub cx,4   ;to go back 4 units ( for making the upper part of the inverted T shape meaning vertical part)  
	dec bx     ;dec row number 
	
	checkMaking_4b_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_4_type_1
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_4b_type_1
	
	
	jmp possible_4_type_1
	stopShape_4_type_1:
	mov word[printAllow],0
	
	possible_4_type_1:
	jmp checkDone_4
	;--------------------
	shape_4_checkType_2:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,3
	
	checkMaking_4a_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_4_type_2
	add di,2
	cmp word[es:di],0x0020
	jne stopShape_4_type_2
	
	inc bx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_4a_type_2
	
	
	mov si,2   
	add cx,2   
	sub bx,2     
	
	checkMaking_4b_type_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_4_type_2
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_4b_type_2
	
	
	jmp possible_4_type_2
	stopShape_4_type_2:
	mov word[printAllow],0
	
	possible_4_type_2:
	jmp checkDone_4
	;------------------
	
	shape_4_checkType_3:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,6
	
	checkMaking_4a_type_3:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_4_type_3
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_4a_type_3
	
	
	mov si,2   ; want to make one block( two characters will be used in our case)
	sub cx,4   ;to go back 4 units ( for making the upper part of the inverted T shape meaning vertical part)  
	inc bx     ;dec row number 
	
	checkMaking_4b_type_3:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_4_type_3
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_4b_type_3
	
	
	jmp possible_4_type_3
	stopShape_4_type_3:
	mov word[printAllow],0
	
	possible_4_type_3:
	jmp checkDone_4
    ;--------------------
	shape_4_checkType_4:
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,3
	
	checkMaking_4a_type_4:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_4_type_4
	add di,2
	cmp word[es:di],0x0020
	jne stopShape_4_type_4
	
	inc bx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_4a_type_4
	
	
	mov si,2   
	sub cx,2   
	sub bx,2     
	
	checkMaking_4b_type_4:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_4_type_4
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_4b_type_4
	
	
	jmp possible_4_type_4
	stopShape_4_type_4:
	mov word[printAllow],0
	
	possible_4_type_4:

	checkDone_4:
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop ds
	pop es
	pop bp
	ret 4

	
	
		
checkShape_5:
    push bp
	mov bp,sp
	push es
	push ds
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax,0xb800
	mov es,ax
	
	cmp word[shapeTypeIterator],1
	je shape_5_checkType_1
	cmp word[shapeTypeIterator],3
	je shape_5_checkType_1
	jmp shape_5_checkType_2
	
	
	shape_5_checkType_1:
	
	mov bx,[bp+4]
	mov cx,[bp+6]
	mov si,4
	
	checkMaking_5a_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_5_type_1
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne checkMaking_5a_type_1
	
	sub cx,3  ;go back to four units in cols    we have not make from second last col thats why we subtracted 3  
	          ;because we want to draw a blank char on third last col and then mov back and make 4 blank chars on previous side
	dec bx    ;go to previous row 
	mov si,4   ; want to make one block( two characters will be used in our case)
	
	checkMaking_5b_type_1:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_5_type_1
	
	dec cx   ;previous col shift
	dec si
	cmp si,0
	jne checkMaking_5b_type_1
	
	
	jmp possible_5_type_1
	stopShape_5_type_1:
	mov word[printAllow],0
	
	possible_5_type_1:
	jmp doneCheck_5
	
	;--------------------
	
	shape_5_checkType_2:
	mov bx,[bp+4]  ;bx is row
	mov cx,[bp+6]  ;cx is col
	mov si,4
	
	making_5a_checkType_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	
	cmp word[es:di],0x0020
	jne stopShape_5_type_2
	
	inc cx   ;next col shift
	dec si
	cmp si,0
	jne making_5a_checkType_2
	
	sub cx,1
	dec bx    ;go to previous row 
	mov si,2   ; want to make one block( two characters will be used in our case)
	
	making_5b_checkType_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_5_type_2
	
	dec cx   ;previous col shift
	dec si
	cmp si,0
	jne making_5b_checkType_2
	
	
	
	add bx,2    ;go to previous row 
	mov si,2   ; want to make one block( two characters will be used in our case)
	
	making_5c_checkType_2:
	mov ax,80
	mul bx
	add ax,cx
	shl ax,1
	mov di,ax

	cmp word[es:di],0x0020
	jne stopShape_5_type_2
	
	dec cx   ;previous col shift
	dec si
	cmp si,0
	jne making_5c_checkType_2
	
	jmp possible_5_type_2
	stopShape_5_type_2:
	mov word[printAllow],0
	
	possible_5_type_2:
	
	doneCheck_5:
	pop di
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop ds
	pop es
	pop bp
	ret 4
	
	
	
	
	eraseAllPreviousPossiblePrintedShapes:
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	
	  push 0x20
	  push 0x00                                ;[bp+8]  for accessing
	  push 71    ;x  or col  or x-pos           ;[bp+6]  for accessing
	  push 20    ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_1
	  
	
	  push 0x20
	  push 0x00                                ;[bp+8]  for accessing
	  push 68    ;x  or col  or x-pos           ;[bp+6]  for accessing
	  push 20    ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
	  
	  push 0x20
	  push 0x00                                ;[bp+8]  for accessing
	  push 72    ;x  or col  or x-pos           ;[bp+6]  for accessing
	  push 20    ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
	  
	  push 0x20
	  push 0x00                                ;[bp+8]  for accessing
	  push 69    ;x  or col  or x-pos           ;[bp+6]  for accessing
	  push 20    ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
	  
	  push 0x20
	  push 0x00                                ;[bp+8]  for accessing
	  push 71    ;x  or col  or x-pos           ;[bp+6]  for accessing
	  push 20    ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
	  
	pop di
	pop si
	pop	dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret
	  
	
	
	
printShapes:
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	
	
	jmp nextShape
	
	;NEXT SHAPE PRINTING     ;67,66,65 etc they have to be respective to be in center above the label of next shape
	printNextShape_1:
		
	  push 0x20
	  push 0x20                            
	  push 71    ;x  or col  or x-pos          
	  push 20    ;y  or row  or y-pos          
	  call shape_1
	  jmp nextShapePrintedSuccessfully
	  
	printNextShape_2:
	
	  push 0x20
	  push 0x30                                ;[bp+8]  for accessing
	  push 68    ;x  or col  or x-pos           ;[bp+6]  for accessing
	  push 20    ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
	  jmp nextShapePrintedSuccessfully
	   
    printNextShape_3:
	
	  push 0x20
	  push 0x40                                ;[bp+8]  for accessing
	  push 72    ;x  or col  or x-pos           ;[bp+6]  for accessing
	  push 20    ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
	  jmp nextShapePrintedSuccessfully
	  	   
	  
	printNextShape_4:
	
	  push 0x20
	  push 0x60                                ;[bp+8]  for accessing
	  push 69    ;x  or col  or x-pos           ;[bp+6]  for accessing
	  push 20    ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
	  jmp nextShapePrintedSuccessfully
	  
	printNextShape_5:

	  push 0x20
	  push 0x50                                ;[bp+8]  for accessing
	  push 71    ;x  or col  or x-pos           ;[bp+6]  for accessing
	  push 20    ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
	  jmp nextShapePrintedSuccessfully
	  
	  

;we have generated random no form 1-4
;each number will be associated with one specific shape 


nextShape:    ;main fundamental driver func..
    
	
	;-------------------------
	keepGamePaused:
    cmp word[gamePause],1
	jne skipPauseFunc
	jmp keepGamePaused

	skipPauseFunc:
	;-------------------------
	
    cmp word[gameRunnningFlag],0
    jne gameContinues
	jmp timeUpsOrGameLose
	gameContinues:
	
	cmp word[quickClearShapesFlag],1
	jne donotClearShapes
	mov word[quickClearShapesFlag],0
	call clearAllShapes
	call printShapes
	donotClearShapes:
	
	
	mov word[shapeTypeIterator],1
	mov word[gameTimerFlag],1
	  

    call scanScreenWithInBoundary

	mov ax,[nextShapeNo]
	mov [currShapeNo],ax
		
	 ;reset the flag of timer...
	 mov word[gameRunnningFlag],1 
	
	
	;TIMER FUNC...
	  xor ax,ax
	  mov es,ax	  
	  
	  ;hooking  interrupt...
	  cli ;clear interrupt flag (IF=0) ( disallow interrupts )
	  mov word[es:8*4],timer
	  mov [es:8*4+2],cs
	  sti  ;set interrupt flag (IF=1) ( allow interrupts )
	  
	  
	  ;KEYPRESS FUNC
	  xor ax,ax
	  mov es,ax
	  
	  ;hooking interrupt
	  cli
	  mov word[es:9*4],kbisr
	  mov [es:9*4+2],cs
	  sti
	  
	  
	cmp word[currShapeNo],1
	je setNextShape_1
	jmp bypassNextShape_1
	setNextShape_1:
	mov word[nextShapeNo],2  
	jmp nextShapeSelectionDone
	bypassNextShape_1:
	
	cmp word[currShapeNo],2
	je setNextShape_2
	jmp bypassNextShape_2
	setNextShape_2:
	mov word[nextShapeNo],3
	jmp nextShapeSelectionDone
	bypassNextShape_2:
	
	cmp word[currShapeNo],3
	je setNextShape_3
	jmp bypassNextShape_3
	setNextShape_3:
	mov word[nextShapeNo],4
	jmp nextShapeSelectionDone
	bypassNextShape_3:
	
	cmp word[currShapeNo],4
	je setNextShape_4
	jmp bypassNextShape_4
	setNextShape_4:
	mov word[nextShapeNo],5
	jmp nextShapeSelectionDone
	bypassNextShape_4:
	
	cmp word[currShapeNo],5
	je setNextShape_5
	jmp bypassNextShape_5
	setNextShape_5:
	mov word[nextShapeNo],1
	jmp nextShapeSelectionDone
	bypassNextShape_5:
	
	nextShapeSelectionDone:
	
	;ERASING THE PREVIOUSLY PRINTED NEXT SHAPE BEFORE PRINTING THE NEW NEXT SHAPE...
	call eraseAllPreviousPossiblePrintedShapes
	
	cmp word[nextShapeNo],1
	je printNextShape_1
	
	cmp word[nextShapeNo],2
	je printNextShape_2
	
	cmp word[nextShapeNo],3
	je printNextShape_3
	
	cmp word[nextShapeNo],4
	je printNextShape_4
	
	cmp word[nextShapeNo],5
	je printNextShape_5
	
	
	nextShapePrintedSuccessfully:
	
	mov word[shapeCurrRow],1  ;TO AVOID READING BEFORE THE FIRST ROW WHILE CHECKING SHAPE FEASIBILITY!
	mov word[shapeCurrCol],35   ; our center in between red wall boundaries...
		
		
	falling:    

	  mov word[printAllow],1
	  
	  ;FUNCTION FOR CHECKING THE FEASIBILTY OF PRINTING SHAPE ON THAT LOCATION....
	  
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax 
	  
	  cmp word[currShapeNo],1
	  jne bypassFall_1
	  call checkShape_1
	  bypassFall_1:
	  
	  cmp word[currShapeNo],2
	  jne bypassFall_2
	  call checkShape_2
	  bypassFall_2:
	  
	  cmp word[currShapeNo],3
	  jne bypassFall_3
	  call checkShape_3
	  bypassFall_3:
	  
	  cmp word[currShapeNo],4
	  jne bypassFall_4
	  call checkShape_4
	  bypassFall_4:
	  
	  cmp word[currShapeNo],5
	  jne bypassFall_5
	  call checkShape_5
	  bypassFall_5:
	  ;-----------------------------------------------------
	  
	  cmp word[printAllow],0
	  je bypassFall_6
	  jmp printingPossibleOnThatLocation
	  bypassFall_6:
	  
	  
	  ;not feasible to form shape on that location...( to build house on that plot which is already occupied by some other building)
	  mov ax,[shapeCurrRow]
	  sub ax,1
	  mov [shapeCurrRow],ax	  
	  
	  ;checking whether shapes touched the roof or not...
	  cmp word[shapeCurrRow],0
	  jne stopAboveShape
	  mov word[keyboardFlag],0
	  jmp timeUpsOrGameLose

      stopAboveShape:	     ;stoping above the shape
	  
	  cmp word[currShapeNo],1
	  jne bypassFall_7
	  push 0x20  ;ascii of space
	  push 0x20        ;colorful colour        ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_1
	  bypassFall_7:
	  
	 
      cmp word[currShapeNo],2
	  jne bypassFall_8
	  push 0x20  ;ascii of space
	  push 0x30        ;colorful colour        ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
	  bypassFall_8:
	  

      cmp word[currShapeNo],3
	  jne bypassFall_9
	  push 0x20  ;ascii of space
	  push 0x40        ;colorful colour        ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
	  bypassFall_9:
	  

      cmp word[currShapeNo],4
	  jne bypassFall_10
	  push 0x20  ;ascii of space
	  push 0x60        ;colorful colour        ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
	  bypassFall_10:
	  


      cmp word[currShapeNo],5
	  jne bypassFall_11
	  push 0x20  ;ascii of space
	  push 0x50        ;colorful colour        ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
	  bypassFall_11:
	   
	  
	  call nextShape
	  printingPossibleOnThatLocation:
	  
	  
      ;CHECKING COMPLETED NOW PRINTING ON THAT LOCATION....
	
	  cmp word[currShapeNo],1
	  jne bypassFall_12
	  push 0x20        ;ascii of space
	  push 0x20        ;colorful colour       ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_1
	  bypassFall_12:
	  
	  cmp word[currShapeNo],2
	  jne bypassFall_13
	  push 0x20        ;ascii of space
	  push 0x30        ;colorful colour       ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_2
	  bypassFall_13:
	  
	  cmp word[currShapeNo],3
	  jne bypassFall_14
	  push 0x20  ;ascii of space
	  push 0x40        ;colorful colour       ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_3
	  bypassFall_14:
	  
	  cmp word[currShapeNo],4
	  jne bypassFall_15
	  push 0x20  ;ascii of space
	  push 0x60        ;colorful colour       ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_4
	  bypassFall_15:
	  
	  cmp word[currShapeNo],5
	  jne bypassFall_16
	  push 0x20  ;ascii of space
	  push 0x50        ;colorful colour       ;[bp+8]  for accessing
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  call shape_5
	  bypassFall_16:

      cmp word[shapeCurrRow],24    ;does it was the last row or not....to avoid removing the last instance , the preserving instance above another shape is automatically addressed using this logic...
	  je nextShape

;PROFILE OF DELAY WILL BE HERE (s -> slow ,f -> fast and if no character then default behavior{normal delay} )

cmp byte[verticalKeyPress],'d'
je delayLess

cmp byte[verticalKeyPress],'u'
je delayMore    

jmp delayNormal


delayLess:   ;shape will come fast ( down key was pressed )...
 call shortDelay
 jmp delayDone
  
delayMore:   ;shape will come slow ( up key was pressed )...
cmp word[delayProfile],1   ;easy
jne checkOtherProfile_1
 call delay
 call delay 
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 jmp delayDone
 
 checkOtherProfile_1:
 
 cmp word[delayProfile],2    ;medium
 jne checkOtherProfile_2
 call delay
 call delay 
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 jmp delayDone

 checkOtherProfile_2:
 ;for delayProfile_3 no need to check because it is the left one...   ;hard
 call delay   
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 jmp delayDone
 
 
 delayNormal:   ;shape will come normal ( default setting when neither or up or down key is pressed )...
 
cmp word[delayProfile],1
jne checkOtherProfile_3
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 jmp delayDone
 
 checkOtherProfile_3:
 
cmp word[delayProfile],2
jne checkOtherProfile_4
 call delay
 call delay 
 call delay
 call delay
 call delay
 call delay
 call delay
 call delay
 jmp delayDone
 
 checkOtherProfile_4:
 call delay
 call delay
 call delay
 call delay
 call delay
 jmp delayDone
 

 delayDone:  
	
	  push 0x20  ;ascii of space
      push 0x00   ;black color
	  mov ax,[shapeCurrCol]
	  push ax                   ;x  or col  or x-pos           ;[bp+6]  for accessing
	  mov ax,[shapeCurrRow]
	  push ax                   ;y  or row  or y-pos          ;[bp+4]  for accessing
	  
	  cmp word[currShapeNo],1
	  jne bypassFall_17
	  call shape_1
	  bypassFall_17:
	  
	  cmp word[currShapeNo],2
	  jne bypassFall_18
	  call shape_2
	  bypassFall_18:
	  
	  cmp word[currShapeNo],3
	  jne bypassFall_19
	  call shape_3
	  bypassFall_19:
	  
	  cmp word[currShapeNo],4
	  jne bypassFall_20
	  call shape_4
	  bypassFall_20:
	  
	  cmp word[currShapeNo],5
	  jne bypassFall_21
	  call shape_5
	  bypassFall_21:
	  
	  
	  mov ax,[shapeCurrRow]
	  inc ax  ;increasing the row no to show the shape coming down...
	  mov [shapeCurrRow],ax
	  
	  jmp falling
	  ;-----------------------------------------------------------------------
  
	  
terminate:

    pop di
	pop si
	pop	dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret
	
scanScreenWithInBoundary:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	push 0xb800
	pop es
	mov ax,0020h 	;attributes empty space
	mov di,206 		;starting point of main frame ( 206 is the location of first character of our main frame first row)
	mov word[totalRowsOfMainFrameToBeScan],23 		;total rows of main frame between boundaries
	
	scanNextRow:   ;this scans one complete row...
		push di
		push cx
		mov cx,26   	;width of main frame ( a counter for total no of characters in a row )
		cld
		repne scasw ; compare [es:di] with ax
		cmp cx,0   ;does the comlete row contain any empty black space or not if not then it means it is completely filled 
		           ;so remove that row or eliminate it..
		           ; if cx becomes zero it means no black space found in the entire row and it iterates through all 
				   ;different colours
				   ;whenever it finds black the repne command stop and cx is left with its counter uptill now so we can 
				   ;see whether it becomes zero or not
				 
        ;reseting the values				 
		pop cx   
		pop di
		push di   ;here ammendment done...
		jne nextIteration
		
		;ONE COMPLETE ROW FILLED FOUNDED SO ELIMINATE THAT ROW...
	
		push cx      ;to preserve the value the value of 60 because we are running the next loop using this counter which makes cx zero ...
		mov cx,26      ;total characters in row of main frame...
		eliminatingRow:
		mov word[es:di],0x0020
		add di,2		
		call shortDelay		
		loop eliminatingRow
		pop cx               ; again value resetted 
		
		
		
		add word[currScore],10
		call updateScore
		
		push di
		call scrollTheWholeFrame ;this function scrolls the whole frame above that row to cover this eliminated row...		
		
		nextIteration:
		pop di ;here ammendment done...
		
		add di,160   ;point di to the next row of our main frame..
		
		;scannedRows decrement operation will run in both the cases whether row removed or not...
		mov bx,[totalRowsOfMainFrameToBeScan]
		dec bx
		mov [totalRowsOfMainFrameToBeScan],bx
		cmp bx,0
		jne scanNextRow
		;all rows of main frame are scanned successfully!
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret
	
	
scrollTheWholeFrame:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	
	mov ax,0xB800
	mov es,ax ;set ES to video memory
	mov ds,ax ;set DS to video memory
	
	mov di,[bp+4] ;bp+4 holds last point of the black row that is removed
	sub di,2 ;so di points one col before last boundary.. actually last character before boundary because  di was also incremented in the iteration of condition false...
	mov si,di
	sub si,160 ;setting source one row above the fully colored row
	
	scrollingRow:
		push di
		push si
		mov cx,26 ;cx holds width of frame
		
		std ;auto decrement mode
		rep movsw   ;does instant job but we want to it look like animation so making our own animation func :)		
		
		;values resetted (previous pointing to end of rows of si and di achieved through push and pop mechanism)
		pop si   
		pop di
		
		;moving si and di to point one row above the current positions:
		sub di,160
		sub si,160
		
		cmp si,161 ;161 means half byte of 160's character and half of 162's character so this is our terminating condition 
		ja scrollingRow  ;( termination condition si <= 161)
	
	pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
	
	
	printCountDown:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	mov ax,0xb800
	mov es,ax

	mov cx,10
	waitCountDown_1:
	call delay
	loop waitCountDown_1
	
	
	;MAKING THREE	
	mov cx,3
	colouring_3:
    push cx
	
	  mov al,80
	  mov bl,10
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	mov cx,6
	l1a:
	mov word[es:di],0x2020
	add di,2
	loop l1a
;---------------	
	
	  mov al,80
	  mov bl,12
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
    mov cx,6
	l2a:
	mov word[es:di],0x2020
	add di,2
	loop l2a
;---------------	

	  mov al,80
	  mov bl,14
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	mov cx,6
	l3a:
	mov word[es:di],0x2020
	add di,2
	loop l3a
;---------------
	
	  mov al,80
	  mov bl,10
	  mul bl
	  add ax,40
	  shl ax,1
	  mov di,ax
	mov cx,5
	l4a:
	push di
	mov word[es:di],0x2020
	add di,2
	mov word[es:di],0x2020
    pop di	
	add di,160
	loop l4a
	;---------------
	call delay 
	call delay 
	;__________________________________________________________
	 mov al,80
	  mov bl,10
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	mov cx,6
	l1b:
	mov word[es:di],0x7020
	add di,2
	loop l1b
;---------------	
	
	  mov al,80
	  mov bl,12
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
    mov cx,6
	l2b:
	mov word[es:di],0x7020
	add di,2
	loop l2b
;---------------	

	  mov al,80
	  mov bl,14
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	mov cx,6
	l3b:
	mov word[es:di],0x7020
	add di,2
	loop l3b
;---------------
	
	  mov al,80
	  mov bl,10
	  mul bl
	  add ax,40
	  shl ax,1
	  mov di,ax
	mov cx,5
	l4b:
	push di
	mov word[es:di],0x7020
	add di,2
	mov word[es:di],0x7020
    pop di	
	add di,160
	loop l4b
	;---------------
	pop cx
	dec cx
	cmp cx,0
	je colouringDone_3
	jmp colouring_3
	colouringDone_3:
	
	
	
	mov cx,30
	waitCountDown_2:
	call delay
	loop waitCountDown_2
	call clrScr
	
	 ;MAKING TWO
	 mov cx,3
	 colouring_2:
     push cx
	
	  mov al,80
	  mov bl,10
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	mov cx,6
	l5a:
	mov word[es:di],0x2020
	add di,2
	loop l5a
;---------------	

	  mov al,80
	  mov bl,10
	  mul bl
	  add ax,40
	  shl ax,1
	  mov di,ax
	mov cx,2
	l6a:
	push di
	mov word[es:di],0x2020
	add di,2
	mov word[es:di],0x2020
    pop di	
	add di,160
	loop l6a
;--------------------

	  mov al,80
	  mov bl,12
	  mul bl
	  add ax,41
	  shl ax,1
	  mov di,ax
    mov cx,6
	l7a:
	mov word[es:di],0x2020
	sub di,2
	loop l7a
;---------------	

	  mov al,80
	  mov bl,12
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	mov cx,2
	l8a:
	push di
	mov word[es:di],0x2020
	add di,2
	mov word[es:di],0x2020
    pop di	
	add di,160
	loop l8a
;---------------
	
	  mov al,80
	  mov bl,14
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	  mov cx,6
	l9a:
	mov word[es:di],0x2020
	add di,2
	loop l9a
	;---------------
	call delay 
	call delay 
	;__________________________________________________________
	mov al,80
	  mov bl,10
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	mov cx,6
	l5b:
	mov word[es:di],0x7020
	add di,2
	loop l5b
;---------------	

	  mov al,80
	  mov bl,10
	  mul bl
	  add ax,40
	  shl ax,1
	  mov di,ax
	mov cx,2
	l6b:
	push di
	mov word[es:di],0x7020
	add di,2
	mov word[es:di],0x7020
    pop di	
	add di,160
	loop l6b		
;--------------------

	  mov al,80
	  mov bl,12
	  mul bl
	  add ax,41
	  shl ax,1
	  mov di,ax
    mov cx,6
	l7b:
	mov word[es:di],0x7020
	sub di,2
	loop l7b
;---------------	

	  mov al,80
	  mov bl,12
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	mov cx,2
	l8b:
	push di
	mov word[es:di],0x7020
	add di,2
	mov word[es:di],0x7020
    pop di	
	add di,160
	loop l8b
;---------------
	
	  mov al,80
	  mov bl,14
	  mul bl
	  add ax,36
	  shl ax,1
	  mov di,ax
	  mov cx,6
	l9b:
	mov word[es:di],0x7020
	add di,2
	loop l9b
	;---------------
	pop cx
	dec cx
	cmp cx,0
	je colouringDone_2
	jmp colouring_2
	colouringDone_2:
	
	
	
	
	mov cx,30
	waitCountDown_3:
	call delay
	loop waitCountDown_3
	call clrScr
	
	;MAKING ONE
	mov cx,3
	colouring_1:
    push cx

      mov al,80
	  mov bl,10
	  mul bl
	  add ax,38
	  shl ax,1
	  mov di,ax
	mov cx,5
	l10a:
	push di
	mov word[es:di],0x2020
	add di,2
	mov word[es:di],0x2020
    pop di	
	add di,160
	loop l10a
;---------------	
call delay 
call delay 
;__________________________________________________________
 mov al,80
	  mov bl,10
	  mul bl
	  add ax,38
	  shl ax,1
	  mov di,ax
	mov cx,5
	l10b:
	push di
	mov word[es:di],0x7020
	add di,2
	mov word[es:di],0x7020
    pop di	
	add di,160
	loop l10b
;---------------	
    pop cx
	dec cx
	cmp cx,0
	je colouringDone_1
	jmp colouring_1
	colouringDone_1:

	mov cx,30
	waitCountDown_4:
	call delay
	loop waitCountDown_4
	
	
	;for quick smooth black of 1 instead of using clrScr
	mov al,80
	  mov bl,10
	  mul bl
	  add ax,38
	  shl ax,1
	  mov di,ax
	mov cx,5
	l11:
	push di
	mov word[es:di],0x0020
	add di,2
	mov word[es:di],0x0020
    pop di	
	add di,160
	loop l11
	
	call delay
	call delay
	
	
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 



selectDificultyLevs:
    push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es


mov ax,0xb800
mov es,ax

  mov cx,10
	  instructionPrintingWait_1:
	  call delay
	  loop instructionPrintingWait_1
	  
	  
	  mov ax,12
	  push ax                      ;[bp+10]
	  mov ax,3
	  push ax                      ;[bp+8]
	  mov ax,0x0C
	  push ax                      ;[bp+6]
	  mov ax,gameDificultyLabel     
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
	  
	  mov ax,5
	  push ax                      ;[bp+10]
	  mov ax,6
	  push ax                      ;[bp+8]
	  mov ax,0x0E
	  push ax                      ;[bp+6]
	  mov ax,selectionInstruction_1     
	  push ax                      ;[bp+4]
	  call printStrWBI
	 
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,10
	  push ax                      ;[bp+8]
	  mov ax,0x07
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type1    
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,12
	  push ax                      ;[bp+8]
	  mov ax,0x07 
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type2     
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,14
	  push ax                      ;[bp+8]
	  mov ax,0x07
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type3     
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  
	  
	  
	  mov ax,9
	  push ax                      ;[bp+10]
	  mov ax,22
	  push ax                      ;[bp+8]
	  mov ax,0x07
	  push ax                      ;[bp+6]
	  mov ax,selectionInstruction_2     
	  push ax                      ;[bp+4]
	  call printStrWBI
	  
	 
	  ;-----------------------------------------------------
	  selectingDificultyLev:
	  
	  ;KEYPRESS FUNC
	  xor ax,ax
	  mov es,ax
	  
	  ;hooking interrupt
	  cli
	  mov word[es:9*4],kbisr
	  mov [es:9*4+2],cs
	  sti
	  

	  
	  cmp word[choiceConfirmedFlag],1   ;terminating cond of loop when user hit space to confirm choice of dificulty level..
	  jne continueSelectingLevs
	  jmp decisionMade
	  continueSelectingLevs:
	  
	  cmp word[selectionIterator],1       ;if 1 is pressed
	  jne  bypassSelection_1
	  mov word[delayProfile],1
	  ;making selected one green
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,10
	  push ax                      ;[bp+8]
	  mov ax,0x0A
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type1    
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  ;making other white
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,12
	  push ax                      ;[bp+8]
	  mov ax,0x07 
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type2     
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,14
	  push ax                      ;[bp+8]
	  mov ax,0x07
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type3     
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  bypassSelection_1:
	  
	  cmp word[selectionIterator],2     ;if 2 is pressed
	  jne  bypassSelection_2
	  mov word[delayProfile],2
	  ;making selected one green
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,12
	  push ax                      ;[bp+8]
	  mov ax,0x0A 
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type2     
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  ;making other white
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,10
	  push ax                      ;[bp+8]
	  mov ax,0x07
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type1    
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  
      mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,14
	  push ax                      ;[bp+8]
	  mov ax,0x07
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type3     
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  bypassSelection_2:
	  
	  cmp word[selectionIterator],3    ;if 3 is pressed
	  jne  bypassSelection_3
	  mov word[delayProfile],3
	  ;making selected one green
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,14
	  push ax                      ;[bp+8]
	  mov ax,0x0A
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type3     
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  ;making other white
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,10
	  push ax                      ;[bp+8]
	  mov ax,0x07
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type1    
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  
	  mov ax,2
	  push ax                      ;[bp+10]
	  mov ax,12
	  push ax                      ;[bp+8]
	  mov ax,0x07 
	  push ax                      ;[bp+6]
	  mov ax,dificulty_type2     
	  push ax                      ;[bp+4]
	  call printStrWBIFast
	  bypassSelection_3:
	  
	  
	  
	 
	  
	  jmp selectingDificultyLev
	  decisionMade:
	  
	  ;call delay
	  call curtainEffect
	  
    pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 

    
	
	clearAllShapes:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	
	push 0xb800
	pop es
	mov ax,0020h 	;attributes of empty space
	
	mov di,206 		;starting point of main frame ( 210 is the location of first character of our main frame first row)
	mov dx,23 		;total rows of main frame between boundaries
	
	blackNextRow:   ;this scans one complete row...
		push di
		push cx
		mov cx,26   	;width of main frame ( a counter for total no of characters in a row )
		blacking:
		mov word[es:di],ax
		add di,2
		loop blacking
        ;reseting the values				 
		pop cx   
		pop di
		add di,160
		sub dx,1
		cmp dx,0
		jne blackNextRow
		
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret



start:
	call clrScr
	mov cx,50
	justTakeAMomentToStartUpThings:
	call delay
	loop justTakeAMomentToStartUpThings
	
    call makeGameLogo
	call printGameNameAndInstructions
	 
gameStart:	  
	mov word[gameStartedFlag],0
	mov word[choiceConfirmedFlag],0
    call selectDificultyLevs
	call printCountDown
	
	;reset everything 
	mov word[decisionForReplay],0 
	mov word[currSeconds],0
	mov word[currScore],0
	mov word[gameRunnningFlag],1
	mov word[gameTimerFlag],0
	mov word[keyboardFlag],1
	  
	call initialSetupOfGame
	 
	;GENERATING RANDOM NO's IN MAIN (start) for the inital first shape in game...
	call generateRandomNumber
	mov ax,[shapeNo]
	mov word[nextShapeNo],ax
	 	 	 
	call printShapes
	  	  
	timeUpsOrGameLose:
		mov word[gameTimerFlag],0
		mov word[gameRunnningFlag],0
		call gameOver

quitGame:
 call clrScr
	 
mov ah,0x1
int 21h

mov ax,0x4c00
int 21h