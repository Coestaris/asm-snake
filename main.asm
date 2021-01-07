.486

; ==================================================================
; STACK SEGMENT
; ==================================================================
STACK_SEG SEGMENT PARA USE16 PUBLIC 'STACK'
    DW 1000 DUP (?)
STACK_SEG ENDS 
; ==================================================================



; Interval of the system timer
time_slice  EQU     10000   
; Scan-code of the ECS key
esc_key    	EQU     01h   

left_key    EQU     4Bh   
right_key   EQU     4Dh   
up_key    	EQU     48h   
down_key    EQU     50h   

; Window width
win_w       EQU     320
; Window height
win_h       EQU     240

max_snake_len EQU 30

; ==================================================================
; DATA SEGMENT
; ==================================================================
DATA_SEG SEGMENT USE16 PUBLIC 'DATA'
    initvid      DB       (0)  ; Initial video mode
    int8set      DB       (0)  ; Indicates that INT8 hadnler registered 
    int9set      DB       (0)  ; Indicates that INT9 hadnler registered
    int8ptr      DW 2 DUP (0)  ; System INT8 handler
    int9ptr      DW 2 DUP (0)  ; System INT9 handler
    ms_dos_busy  DW 2 DUP (0)  ; Address of the MS-DOS busy flag
    SNAKE        DW max_snake_len * 2 DUP (0)  
    DIRECTION    DB       (0)  ; 0 - Up, 1 - Down, 2 - Left, 3 - Right
    SNAKE_LEN    DB       (1)
DATA_SEG ENDS
; ==================================================================



; ==================================================================
; CODE SEGMENT
; ==================================================================
CODE_SEG SEGMENT USE16 PUBLIC 'CODE'
ASSUME CS:CODE_SEG, DS:DATA_SEG, SS:STACK_SEG
; ==================================================================



; ==================================================================
; SETINT8 - Setups castom INT9 hadnler (timer interrupt)
; ==================================================================
SETINT8 PROC
    pusha                   ; Save register states
    push DS
    
    push DATA_SEG           ; Load data segment
    pop DS

    cmp int8set, 0          ; If handler already registered, skip it
    jne skip_setint8
    
    mov	ah, 35h             ; Get system INT8 handler from DOS. 
                            ; Returned value: ES:BX - address of the system procedure
    mov al, 8
    int 21h  
    mov int8ptr, bx         ; Saving system INT8 handler for future usage and restoring   
    mov int8ptr + 2, ES  
   
    mov dx, offset userint8	; Creating address of the custom handler
    push DS                 ; Save current DS value  
    push CS
    pop DS
    mov ah, 25h             ; Setup custom INT8 handler stored in DS:BX
    mov al, 8   
    int 21h     
    mov ax, time_slice 
    out 40h, al             ; Set timer interval (low byte)  
    
    nop                     ; Wait 4 cycles
    nop         
    nop  
    nop
    
    mov al, ah
    out 40h, al             ; Set timer interval (high byte)
    pop ds                  ; Restore DS value
    mov int8set, 0ffh       ; Mark INT8 as registered

skip_setint8:
    pop DS                  ; Restore registers state
    popa
    ret
SETINT8 ENDP
; ==================================================================



; ==================================================================
;  RETINT8 - Restores system INT8 handler
; ==================================================================
RETINT8 PROC
    pusha                   ; Save register states
    push ds

    push DATA_SEG           ; Load data segment
    pop DS

    cmp int9set, 0          ; If handler is not registered, skip it
    je skip_retint8

    mov al, 0ffh            ; Restore default timer behavior
    out 40h, al             
    
    nop                     ; Wait 4 cycles
    nop         
    nop  
    nop
    out 40h, al             

    mov dx, int8ptr         ; DS:DX - address of the system function
    mov ds, int8ptr + 2
    mov ah, 25h             ; Restore system INT8 handler
    mov al, 8     
    int 21h       

    mov int8set, 0          ; Unmark INT8 as registered          

skip_retint8:
    pop ds                  ; Restore registers state
    popa
    ret
RETINT8 ENDP
; ==================================================================



; ==================================================================
; SETINT9 - Setups castom INT9 hadnler (keyboard interrupt)
; ==================================================================
SETINT9 PROC
    pusha                   ; Save registers state
    push DS

    push DATA_SEG           ; Load data segment
    pop DS

    cmp int9set, 0          ; If handler already registered, skip it
    jne skip_setint9

    mov	ah, 34h             ; Get MS-DOS busy flag. Result stored in ES:BX
    int	21h	        
    mov	ms_dos_busy, bx      ; Store its address and segment selector    
    mov	ms_dos_busy + 2, ES
    
    mov	ah, 35h             ; Get system INT8 handler from DOS. 
                            ; Returned value: ES:BX - address of the system procedure
    mov al, 9
    int 21h  
    mov int9ptr, bx          ; Saving system INT8 handler for future usage and restoring   
    mov int9ptr + 2, ES  

    mov dx, offset userint9	; Creating address of the custom handler
    push DS                 ; Save current DS value  
    push CS
    pop DS
    mov ah, 25h             ; Setup custom INT8 handler stored in DS:BX
    mov al, 9                  
    int 21h  

    pop ds                  ; Restore DS value
    mov int9set, 0ffh       ; Mark INT8 as registered
skip_setint9:
    pop DS                  ; Restore registers state
    popa
    ret
SETINT9 ENDP
; ==================================================================



; ==================================================================
;  RETINT9 - Restores system INT9 handler
; ==================================================================
RETINT9 PROC
    pusha                   ; Save registers state
    push DS
  
    push DATA_SEG           ; Load data segment
    pop DS

    cmp int9set, 0          ; If handler is not registered, skip it
    je skip_retint9

    mov dx, int9ptr         ; Store system handler to ds:dx
    mov DS, int9ptr + 2
    mov ah, 25h             ; Replace custom handler with a system one
    mov al, 9       
    int 21h                   	 

    mov int9set, 0          ; Unmark handler as registered

skip_retint9:
    pop DS                  ; Restore registers state
    popa
    ret
RETINT9 ENDP
; ==================================================================



; ==================================================================
; Update func
; ==================================================================
update PROC
    mov al, DIRECTION
    cmp al, 0
    je update_up
    cmp al, 1
    je update_down
    cmp al, 2
    je update_left
    cmp al, 3
    je update_right
    jmp update_end
update_up:
    cmp SNAKE[2], 0
    jne dont_clip_up
        mov SNAKE[2], win_h
    dont_clip_up:
    dec SNAKE[2]
    jmp update_end
update_down:
    inc SNAKE[2]
    jmp update_end
update_left:
    dec SNAKE[0]
    jmp update_end
update_right:
    inc SNAKE[0]
    jmp update_end

update_end:
    push SNAKE[0]
    push SNAKE[2]
    push 2 
    call PLOT

    
    push SNAKE[0]
    push 100
    call show

    ret
update ENDP
; ==================================================================


; ==================================================================
;  USERINT9 - Custom INT8 Handler (Timer Interrupt)
; ==================================================================
userint8 PROC far
    pusha                   ; Save registers state
    push ds

    push DATA_SEG
    pop ds                  ; Load data segment

    pushf
    call dword ptr int8ptr  ; Call a system timer hadnler 

    call update

    pop ds                  ; Restore registers state
    popa                    
    iret                    ; Exit from the interrupt
userint8 ENDP
; ==================================================================




; ==================================================================
;  USERINT9 - Custom INT9 Handler (Keyboard Interrupt)
; ==================================================================
USERINT9 PROC FAR
    pusha                   ; Save registers state        
    push DS
    push ES

    push DATA_SEG           ; Load data segment
    pop DS

    in al, 60h              ; Input key scan-code. 7th bit - pressing flag
    mov ah, al	            
    and al, 7fh             

    mov bx, ax

    cmp al, esc_key         ; Select keys to handle
    je btn_pressed
    cmp al, left_key
    je btn_pressed
    cmp al, right_key
    je btn_pressed
    cmp al, up_key
    je btn_pressed
    cmp al, down_key
    je btn_pressed

    pop ES                  ; Restore registers state, to exit from the function
    pop DS
    popa
    jmp dword ptr int9ptr   ; Call system handler for the keys we dont care about

btn_pressed:
    in al, 61h              ; Getting initial value of the pulse
    mov ah, al
    or al, 80h              ; Set 7th bit to 1				 
    out 61h, al             ; Sending confirming pulse to keyboard to unlock it (first byte)

    nop                     ; Wait 4 cycles
    nop         
    nop  
    nop	
    
    mov al, ah  				  
    out 61h, al             ; Sending confirming pulse to keyboard to unlock it (first byte)

    mov al, 20h 
    out 20h, al             ; Sending signal to IC to unlock INT8 

    cmp bl, esc_key
    je esc_pressed

    bt bx, 7
    jc userint9

    cmp bl, left_key
    je left_pressed
    cmp bl, right_key
    je right_pressed
    cmp bl, up_key
    je up_pressed
    cmp bl, down_key
    je down_pressed
    
    jmp usr9_end

left_pressed:
    mov DIRECTION, 2
    jmp usr9_end
right_pressed:
    mov DIRECTION, 3
    jmp usr9_end
up_pressed:
    mov DIRECTION, 0
    jmp usr9_end
down_pressed:
    mov DIRECTION, 1
    jmp usr9_end
esc_pressed:
    push ES                 ; Save ES state                 
    
    mov bx, ms_dos_busy     ; Store addres of the flag to ES:DX
    mov ES, ms_dos_busy + 2

    mov	al, es:[bx]			; Get busy flag
    pop	es                  ; Restore ES State
    
    or al, al               ; If MS-DOS is busy right now (we interrupted system interruption), exiting
    jnz usr9_end            

    call exitp              ; Exit the proram

usr9_end:
    pop ES              
    pop DS                  ; Restore registers state 
    popa
    iret                    ; Exit from the interrupt
USERINT9 ENDP
; ==================================================================



; ==================================================================
; EXITP - Restores default interrupt handlers and closes the program
; ==================================================================
EXITP PROC
    push DATA_SEG           ; Load data segment
    pop DS

    call retint8            ; Restore default timer interrupt handler
    call retint9            ; Restore default keyboard interrupt handler

    mov ah, 0               ; Set new video mode
    mov al, initvid         ; Set initial video mode
    int 10h                 

    mov ax, 4c00h
    int 21h                 ; Exit to MS-DOS
EXITP ENDP
; ==================================================================


; ==================================================================
; PLOT - Puts pixel to a specified position. Parameters
;    16bit - X position
;    16bit - Y position
;    16bit - Pixel color
; ==================================================================
PLOT PROC NEAR
    push bp                  ; Save stack state  
    mov bp, sp
    pusha                    ; Save registers state
    push ES

    push 0A000h             ; ES storing graphics address
    pop ES
    
    mov cx, [bp + 8]        ; x
    mov dx, [bp + 6]        ; y

    cmp cx, win_w           ; Check X bounds
    jge plot_end

    cmp dx, win_h           ; Check Y bounds
    jge plot_end
    
    mov bx, dx              ; Calculate memory offset
    shl bx, 8               ; *256
    shl dx, 6               ; *64
    add bx, dx              ; *256 + *64 = 320
    add bx, cx

    mov ax, [bp + 4]        ; color
    mov es:[bx], al         ; Plot the pixel 
    
plot_end:
    pop ES                  ; Restore ES state
    popa                    ; Restore registers
    pop bp                  ; Restore stack
    ret 6      
PLOT ENDP
; ==================================================================



; ==================================================================
; MAIN ROUTINE
; ==================================================================
begin:

    ; Init segments
    push DATA_SEG
    pop DS
    push STACK_SEG
    pop SS

    mov ah, 0Fh
    int 10h 
    mov initvid, al

    ; Init graphic VGA mode
    mov ah, 0h              ; Set video mode
    mov al, 013h            ; VGA mode
    int 10h 

    call SETINT9
    call SETINT8

    push 10
    push -20
    push 4
    call PLOT

    lp:
    jmp lp

    call EXITP              ; Close the program
   
CODE_SEG ENDS
end begin
; ==================================================================