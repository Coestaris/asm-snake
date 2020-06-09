.486

; ==================================================================
; STACK SEGMENT
; ==================================================================
STACK_SEG SEGMENT PARA USE16 PUBLIC 'STACK'
    DW 1000 DUP (?)
STACK_SEG ENDS 

; ==================================================================
; DATA SEGMENT
; ==================================================================
DATA_SEG SEGMENT USE16 PUBLIC 'DATA'
DATA_SEG ENDS

; ==================================================================
; CODE SEGMENT
; ==================================================================
CODE_SEG SEGMENT USE16 PUBLIC 'CODE'
ASSUME CS:CODE_SEG, DS:DATA_SEG, SS:STACK_SEG
begin:

    ; Exit to DOS
    mov ax,4C00h 	
    int 21h

CODE_SEG ENDS
end begin