                         
 INCLUDE emu8086.inc
data segment
nombre Dw 16 dup(?),'$'
rom dw 'M','D','C','L','X','V','I','.'      
arab  dw  1000,500,100,50,10,5,1
precedent dw ? 
arabe dw ?
r dw ?                  
romain dw ?
exist dw ? 
avant dw ?
msg_rom DB 13,10,'Veuillez entrer un nombre  romain  :  ',13,10,13,10,'     ->  $' 
 msg DB 13,10,' ERREUR ! le nombre entre est invalide.',13,10,'Veuillez entrer un autre nombre  :  ','$'
cpt dw ?
cpt2 dw ?
resultat db 4 dup(?),'$'
msg_affch_romain  DB 13,10,'La conversion en chiffres arabes donne :  ','$'
a   dw   ?   
msg_ara DB 13,10,'Veuillez entrer un nombre  en arabe  :  ',13,10,13,10,'     ->  $' 



 ; add your data here!
    pkey db "press any key...$"
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax 

 ; arabe romain --------
  

 

arabe_romain proc
    
 PRINT ' donner le nombre que vous voulez convertir en romain :  '
call scan_num

 mov ax,cx
; cmp ax,0

mov si, 0h
mov di, 0h
si9: cmp ax,900                                    ;       jle err
jl si10

cmp ax,1000
jge si10

sub ax,900
mov romain[di],"C"

inc di

mov romain[di],"M"
inc di
si10: cmp ax,400
jl si11
cmp ax,500
jge si11
sub ax,400
mov romain[di],"C"
inc di
mov romain[di],"D"
inc di

si11:  cmp ax,90
jl  si12
cmp ax,100
jge si12
sub ax,90
mov romain[di],"X"
inc di
mov romain[di],"C"
inc di

si12: cmp ax,40
jl si13
cmp ax,50
jge si13
sub ax,40
mov romain[di],"X"
inc di
mov romain[di],"L"
inc di

si13:cmp ax,9
jne si14
sub ax,9

mov romain[di],"I"
inc di
mov romain[di],"X"
inc di
si14: cmp ax,4
jne tq1
sub ax ,4
mov romain[di],"I"
inc di
mov romain[di],"V"
inc di


tq1:
cmp ax, 0
jle ftq1
tq2:
cmp ax,arab[si]
jl  ftq2
sub ax,arab[si]
mov dx,rom[si]
mov romain[di],dx
inc di

si1: cmp ax,900
jl si2

cmp ax,1000
jge si2

sub ax,900
mov romain[di],"C"

inc di

mov romain[di],"M"
inc di
si2: cmp ax,400
jl si3
cmp ax,500
jge si3
sub ax,400
mov romain[di],"C"
inc di
mov romain[di],"D"
inc di

si3:  cmp ax,90
jl  si4
cmp ax,100
jge si4
sub ax,90
mov romain[di],"X"
inc di
mov romain[di],"C"
inc di

si4: cmp ax,40
jl si5
cmp ax,50
jge si5
sub ax,40
mov romain[di],"X"
inc di
mov romain[di],"L"
inc di

si5:cmp ax,9
jne si6
sub ax,9

mov romain[di],"I"
inc di
mov romain[di],"X"
inc di
si6: cmp ax,4
jne tq1
sub ax,4
mov romain[di],"I"
inc di
mov romain[di],"V"
inc di



ftq2: add si,2
jmp tq1
ftq1:
PRINT  'le resultat de la conversion arabe _ romain est : '


LEA DX,romain
MOV AH,9h
INT 21h 


 RET
    
arabe_romain ENDP 

 
   
 ; validation d'un nombre romain
   
validation proc
  
    ;Verifions que les caracteres sont I,V,X,L,C,D,M 
                                         
           MOV DI,offset nombre
        
lect:                   
           MOV SI,0
           MOV AH,1
           INT 21H
                     
           CMP Al,13
           JE flect
                        
                        
compar:      CMP rom[SI],'.'
           JE INVALIDE2
           
           CMP byte rom[si],al
           JE fsi
           ADD si,2
           JMP compar 
                    
fsi:        MOV [DI],Al
           add di,2
           jmp lect 

INVALIDE:  MOV DX,offset msg
           MOV AH,9
           INT 21H 
           JMP LECT
 
flect:  Mov [DI],'$' 


           ;verifions que les regles de construction sont respectee


           MOV SI,offset nombre
            
           
          ;certains caracteres ne peuvent etre repetes plus de trois fois 
          
          
            MOV [cpt],0 
            MOV cpt2 , 0 
ttq1:   
            MOV DI,SI
            ADD DI,2 
            CMP [DI],'$'
            JE conversion
            
            MOV AX,[SI]
            CMP AX,[DI] 
            JNE non
            INC [cpt]  
            CMP [cpt],3
            JGE INVALIDE2
            JMP e1
            
           
   
           ;si la valeur est un I, elle ne peut etre suivie que de X ou V ou I

non:      
            MOV [cpt],0 

e1:
            CMP [SI],'I'
            JNE e2
            CMP [DI],'I'
            JE e7
            CMP [DI],'X'
            JE e6
            CMP [DI],'V'
            JE e6
            
            JMP INVALIDE2
           
          ;si la valeur est un V, elle ne peut etre suivie que de I 
         
e2:       CMP [SI],'V'
          JNE e3 
          CMP [DI],'V'
          JE INVALIDE2
          CMP [DI],'I'
          JE e6
           
          JMP INVALIDE2
          
          ;si la valeur est un X, elle ne peut etre suivie d'un D ou d'un M 
          
e3:    CMP [SI],'X'
          JNE e4 
          CMP [DI],'X'
          JE e7  
          CMP [DI],'V'
          JE e8 
          CMP [DI],'I'
          JE e8   
          CMP [DI],'D'
          JE INVALIDE2
          CMP [DI],'M'
          
          JE INVALIDE2
          
          ;si la valeur est un L, elle ne peut etre suivie que par un X , V ou I
        
e4:       CMP [SI],'L'
          JNE e5 
          CMP [DI],'L'
          JE INVALIDE2 
          CMP [DI],'X'
          JE e6  
          CMP [DI],'V'
          JE  e6 
          CMP [DI],'I'
          JE e6 
          
          JMP INVALIDE2         
                   
          ;si la valeur est un D, elle ne peut etre suivie que par un C
        
e5:       CMP [SI],'D'
          JNE e6  
          CMP [DI],'D'
          JE INVALIDE2
          CMP [DI],'C'
          JE e6 
          CMP [DI],'L'
          JE e6 
          CMP [DI],'X'
          JE e6
          CMP [DI],'V'
          JE e6  
          CMP [DI],'I'
          JE e6 
          
          JMP INVALIDE2
            

INVALIDE2: 
           MOV SI,offset nombre

ttq2:       Cmp [SI],'$' 
           JNE efface 
           MOV [SI],0
           JMP INVALIDE
efface:    MOV [SI],0
           INC SI
           JMP ttq2 

e8: 
           MOV cpt2,0
           ADD SI,2
           JMP ttq1
           
e7:        INC cpt2
           ADD SI,2
           JMP ttq1            
           
e6:        CMP cpt2,  1
           JGE INVALIDE2
           ADD SI,2
           JMP ttq1 
             
           
           RET
validation ENDP
          
          
          
  ; affichage du resultat romain_arabe
  
  Affichage_romain_arabe  PROC
 
        MOV SI,offset resultat  
                                   
        MOV CX,00h
        MOV BX,0ah
loop1:
                MOV DX,0
                DIV BX
                ADD DL,'0'
                PUSH DX
                INC CX
                CMP AX,0ah
                JGE loop1
                ADD AL,'0'
                MOV [SI],AL
loop2:
                POP AX
                INC SI
                MOV [SI],AL
                LOOP loop2
                INC SI
                MOV AL,'$'
                MOV [SI],AL
                
                


    MOV AH,,09h     
    MOV DX,offset msg_affch_romain
    INT 21h
    
    MOV AH,,09h     
    MOV DX,offset resultat
    INT 21h          
 

    
    RET
    
Affichage_romain_arabe ENDP 


 ; le programme de conversion ùromain->arabeù
    
romain_arabe proc  
    

     MOV DX,offset msg_rom
      MOV AH,9
      INT 21H 
      
      call validation
    
    
 
 conversion: 
 
   
    MOV SI,0   ; indice de la chaine(le nombre)
    MOV [precedent],-1 ; l'indice de la lettre precedente
    MOV [arabe],0  ; la valeur en chiffres arabes
    MOV [romain],0 ; la valeur du nombre en final
     
    
TQ:
    CMP nombre[SI],'$' ; tq le nombre n'arrive pas a sa fin
    JE FTQ
    
    MOV [r],0
    MOV DI,[r]  ; 
    mov [exist],0; 
    
TQe2:
    CMP DI,14
    JA ftqe2
    CMP [exist],0  ; si exist=faux             
    JNE ftqe2
    MOV Bx,nombre[SI]
    CMP BX,rom[DI]  ;si nombre[i]<>rom[r] 
    JE Sinon
    ADD DI,2  ; i++     
    
    JMP TQe2
    
Sinon:
 
    MOV [exist],1  ; cette lettre existe dans rom
    JMP TQe2
    
ftqe2: 
    ; construire un booleen "avant"=precedent<=r et ca pour voir si
    ; les chiffre composes de deux lettre sont une add ou sub(exp IV)  
    MOV DX,[precedent] ;si precedent<=r
    CMP DX,DI
    JG sii1     ; si precedent>r ( si la lettre precedente est sup alors on effectue une add)
    MOV [avant],1 ; avant=vrai
    JMP sii2
     
sii1:
    MOV [avant],0 ; avant=faux
    
   sii2:
    
    CMP [avant],1  ; si le precedent est sup alors avant=vrai
    JNE else 
    
    MOV AX,[romain] 
    ADD AX,[arabe]  ;romain=romain+arabe 
    JMP FSIi
    
else: 
    MOV AX,[romain]
    SUB AX,[arabe]     ; si le precedent est inferieur on fait une sub
    
FSIi: 
    MOV BX,arab[DI] ; arabe= arab[r]
    MOV [arabe],BX
    MOV [precedent],DI ; precedent=r
    
    add SI,2 ; r++   
    mov [romain],AX  ; la valeur de romain
    Jmp TQ
    
FTQ: 
    ADD AX,[arabe]  ;romain=romain+arabe
    MOV [romain],AX   
    call affichage_romain_arabe
    ret
 
 
 romain_arabe endp 


; prog prin
    
    
DEBUT:

          
      MOV AX, data
      MOV DS, AX 
      PRINT ' La conversion arabe romain  :   '
      call arabe_romain 
      PRINT ' la conversion romain arabe    :  '
      call romain_arabe  
      DEFINE_SCAN_NUM 
           
quit: MOV AH,4ch
      INT 21h   
      
code ENDS
      END DEBUT
    
  
  
  
    
; add your code here
            
    lea dx, pkey
    mov ah, 9
    int 21h        ; output string at ds:dx
    
    ; wait for any key....    
    mov ah, 1
    int 21h
    
    mov ax, 4c00h ; exit to operating system.
    int 21h 
    
    
DEFINE_SCAN_NUM   
ends

end start ; set entry point and stop the assembler.
