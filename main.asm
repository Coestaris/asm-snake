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



; ==================================================================
; DATA SEGMENT
; ==================================================================
DATA_SEG SEGMENT USE16 PUBLIC 'DATA'
    int8set      DB       (0)  ; Indicates that INT8 hadnler registered 
    int9set      DB       (0)  ; Indicates that INT9 hadnler registered
    int8ptr      DW 2 DUP (0)  ; System INT8 handler
    int9ptr      DW 2 DUP (0)  ; System INT9 handler
    ms_dos_busy  DD       (?)  ; логічна адреса ознаки зайнятості MS-DOS
    X_POS        DW       (10)  
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
    pusha
    push DS

    push DATA_SEG
    pop DS
   
    cmp int8set, 0
    jne zero_8

    mov	ah, 35h ; отримати вектор переривання
    mov al, 8   ; переривання від таймера (8)
    int 21h     ; значення що повертається:
                ; es:bx - логічна адреса системної процедури
                ; обробки переривання від таймера


    mov int8ptr, bx      ; зберегти логічну адресу системної
    mov int8ptr + 2, es  ; процедури в сегменті кодів

    mov dx, offset userint8	; формування в ds:dx логічної
                            ; адреси процедури користувача
                            ; для обробки переривань від таймера

    push ds         ; Збереження вмісту DS

    push cs
    pop ds

    mov ah, 25h ; встановити вектор
    mov al, 8   ; переривання від таймера
    int 21h     ; ds:dx - покажчик на користувацьку
                ; процедуру оброб. переривання від ;таймера


    mov ax, time_slice  ; встановити задану величину кванту часу
    out 40h, al         ; 40h - адреса 8-розрядного порта таймера,
                        ; через який задають період таймера
                        ; спочатку молодший байт,
                        ; а потім старший

    jmp $+2     ; стандартний метод узгодження швидкісного
                ; процесора з більш повільним зовнішнім
                ; пристроєм. Припускаємо, що
                ; "безглузда" команда jmp очищує буфер
                ; попередньої вибірки команд і, тим самим,
                ; уповільнює роботу процесора. Тим часом
                ; зовнішній пристрій буде готовий
                ; прийняти наступний байт

    nop

    mov al, ah ; (старший байт)
    out 40h, al

    pop ds ; Відновлюємо зміст DS

    mov int8set, 0ffh ; заборона повторних входжень

zero_8:
    pop DS
    popa

    ret
SETINT8 ENDP
; ==================================================================



; ==================================================================
;  RETINT8 - Restores system INT8 handler
; ==================================================================
RETINT8 PROC
    pusha
    push ds

    mov al, 0ffh    ; відновити нормальну роботу
    out 40h, al     ; системного таймера
    
    jmp	$+2
    nop
    
    out 40h, al

    mov dx, int8ptr
    mov ds, int8ptr + 2

    mov ah, 25h   ; відновити початковий вектор
    mov al, 8     ; переривання від таймера
    int 21h       ; ds:dx - вказівник (логічна адреса) 
                ; на початкову (системну) процедуру
                ;	оброб. переривання від таймера

    mov int8set,0h ; дозвіл наступних "перехоплень"

    pop ds
    popa

    ret
RETINT8 ENDP
; ==================================================================



; ==================================================================
; SETINT9 - Setups castom INT9 hadnler (keyboard interrupt)
; ==================================================================
SETINT9 PROC
    pusha
    push DS

    push DATA_SEG
    pop DS

    mov	al, int9set
    or al, al
    jnz zero_9

    mov	ah, 34h
    int	21h	        ; es:bx - адреса ознаки зайнятості MS-DOS
    mov	word ptr ms_dos_busy, bx
    mov	word ptr ms_dos_busy + 2, es
    
    mov ah, 35h ; отримати вектор переривання
    mov al, 9   ; переривання від клавіатури (9)
    int 21h     ; значення що повертається:
                ; es:bx - вказівник на системну процедуру
                ; обробки переривання від клавіатури

    mov int9ptr, bx      ; зберегти в сегменті кодів вказівник 
    mov int9ptr + 2, es  ; на системну процедуру

    mov dx, offset userint9
    push ds
    push cs         ; ds:dx - вказівник на процедуру користувача
    pop ds			; оброб. переривання від клавіатури

    mov ah, 25h  ; встановити вектор "перехоплення"
    mov al, 9    ; переривання від клавіатури (9)
    int 21h       

    pop ds

    mov int9set,0ffh        	; заборона повторних входжень
zero_9:

    pop DS
    popa
    ret
SETINT9 ENDP
; ==================================================================



; ==================================================================
;  RETINT9 - Restores system INT9 handler
; ==================================================================
RETINT9 PROC
    pusha
    push DS
  
    push DATA_SEG
    pop DS

    mov dx, int9ptr      ; ds:dx - покажчик на початкову (системну)
    mov DS, int9ptr+2    ; процедуру обробки переривання від
                            ; клавіатури

    mov ah, 25h     ; встановити вектор системної процедури
    mov al, 9       ; обробки переривання від клавіатури
    int 21h                   	 

    mov int9set, 0h ; дозвіл наступних "перехоплень"
    
    pop DS
    popa
    ret

RETINT9 ENDP
; ==================================================================



; ==================================================================
;  USERINT9 - Custom INT8 Handler (Timer Interrupt)
; ==================================================================
userint8 PROC far

    pushad      ; збереження РОН в стеку перерваної задачі
    push ds
    pushf

    push DATA_SEG
    pop ds       ; в перерваній програмі вміст сегментного регістра

    call dword ptr int8ptr


    inc X_POS

    push X_POS
    push 20
    push X_POS 
    call PLOT

    pop ds
    popad                   ; продовжити виконання перерваної задачі
    iret

userint8 ENDP
; ==================================================================




; ==================================================================
;  USERINT9 - Custom INT9 Handler (Keyboard Interrupt)
; ==================================================================
USERINT9 PROC FAR
    pusha
    push DS
    push ES

    in al, 60h      ; ввести скан-код - розряди 0-6
    mov ah, al	    ; 7-ий розряд дорівнює 0 при натисканні
    and al, 7fh     ; клавіші, 1- при відтисканні

    push DATA_SEG
    pop DS

    cmp al, esc_key
    je btn_pressed

        

        ; ПЕРЕДАЧА ВИРІШЕННЯ ПРОБЛЕМ З КЛАВІАТУРОЮ СИСТЕМІ (Варіант 2)
        pop ES
        pop DS
        popa
        jmp dword ptr int9ptr ; перехід на системну
                                    ; процедуру обробки
                                    ; переривань від клавіатури, яка
                                    ; виконає всі необхідні дії, включаючи
                                    ; повернення в перервану програму

    btn_pressed:
        ; САМОСТІЙНЕ ВИРІШЕННЯ ПРОБЛЕМ З КЛАВІАТУРОЮ (Варіант 1)
        mov bx, ax
        in al, 61h  ; біт 7 порта 61h призначений для введення
                    ; підтверджуючого імпульсу в клавіатуру ПЕОМ.
                    ; Клавіатура блокується поки не надійде
                    ; підтверджуючий імпульс
                    
        mov ah, al
        or al, 80h  ; set 7th bit to 1				 
            
        out 61h, al ; виведення на клавіатуру 1й байт	 

        jmp $ + 2	
        
        mov al, ah  ; виведення на клавіатуру 2й байт					  
        out 61h, al ; підтверджуючого імпульсу

        mov al, 20h ; розблокувати в контролері переривання
                    ; проходження запитів на переривання 
                    ; поточного та меншого рівнів пріоритету,
        
        out 20h, al ; що забезпечить можливість наступного 
                    ; переривання від клавіатури

        mov ax, bx
        cmp ah, al  ; перевірка події переривання - від натискання
                    ; чи від відтискання клавіші клавіатури
        je usr9_end ; відтискання клавіші

    
        push es
        les	bx, ms_dos_busy	; es:bx - адреса ознаки 
                                ; зайнятості MS-DOS
        mov	al, es:[bx]			; al - ознака зайнятості MS-DOS
        pop	es
        
        or al, al   ; перевірка якщо була перервана робота MS-DOS
                    ; в "невдалий" момент то не можна від неї вимагати
                    ; виконання ряду функцій (в загальному випадку MS-DOS 
                    ; не забезпечує повторне входження)
        jnz usr9_end

        call retint8
        call retint9

        ; Clear screen
        mov ax, 3
        int 10h

        mov ax, 4c00h
        int 21h         ; ЗАКІНЧИТИ РОБОТУ
                        ; БАГАТОПРОГРАМНОЇ МОДЕЛІ

usr9_end:
        
    pop ES ; відновити стек перерваної програми
    pop DS ; відновити стек перерваної програми
    popa
    
    iret ; закінчити обробку переривання

userint9 ENDP
; ==================================================================



; ==================================================================
; PLOT - Puts pixel to a specified position. Parameters
;    16bit - X position
;    16bit - Y position
;    16bit - Pixel color
; ==================================================================
PLOT PROC NEAR
    ; Save stack state
    push bp         
    mov bp, sp
    ; Save registers state
    pusha
    push ES
    
    ; ES storing graphics address
    push 0A000h
    pop ES
    
    mov cx, [bp + 8] ; x
    mov dx, [bp + 6] ; y

    ; Calculate memory offset
    mov bx, dx
    shl bx, 8   ; *256
    shl dx, 6   ; *64
    add bx, dx
    add bx, cx

    mov ax, [bp + 4] ; color
    ; Plot the pixel
    mov byte ptr es:[bx], al
    
    pop ES

    ; Restore registers
    popa
    ; Restore stack
    pop bp
    ret 6      
PLOT ENDP



; ==================================================================
; MAIN ROUTINE
; ==================================================================
begin:

    ; Init segments
    push DATA_SEG
    pop DS
    push STACK_SEG
    pop SS

    ; Init graphic VGA mode
    mov ax, 013h
    int 10h 

    call SETINT9
    call SETINT8

    lp:
    jmp lp

    ; Return default video mode
    mov ax, 3h 	
    int 10h

    ; Exit to DOS
    mov ax,4C00h 	
    int 21h

CODE_SEG ENDS
end begin
; ==================================================================