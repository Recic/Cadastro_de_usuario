     ******************************************************************
      * AUTHOR: RENAN CICERO
      * DATE: 08/03/2023
      * PURPOSE: Modulo de Listagem
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGLIST.
       
       
      ******************************************************************
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USUARIOS ASSIGN TO 
           '/home/recic/Dev/PROG01/Dados/USUARIOS.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL 
           RECORD KEY IS ID-USUARIO
           FILE STATUS IS WS-FS.
           
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD  USUARIOS.
           COPY 
           '/home/recic/Dev/PROG01/Dados/FD-USUARIOS.cpy'.
       

       
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WS-REGISTRO-US.
           03 WS-ID                    PIC 9(02).
           03 WS-EMAIL                 PIC X(30).
           03 WS-NOME                  PIC X(30).
           03 WS-SENHA                 PIC X(08).
           03 WS-TELEFONE.
              05 WS-PAIS               PIC 9(02).
              05 WS-DDD                PIC 9(02).
              05 WS-TEL                PIC 9(09).


       77  WS-FS                       PIC 99.
           88 FS-OK                    VALUE 0.  

       77  WS-EXT                      PIC X.
           88 EXT-OK                   VALUE 'F' FALSE 'N'.
        
       77  WS-EOF                      PIC X.
           88 EOF-OK                   VALUE 'F' FALSE 'N'.

       77  WS-CONT                     PIC 99.




      ******************************************************************
       PROCEDURE DIVISION.
           
           
           
           DISPLAY '*-------------------------------------------------*'
           DISPLAY ' '
           DISPLAY '          LISTA DE USUARIOS CADASTRADOS' 
           DISPLAY ' '
           DISPLAY '*-------------------------------------------------*'

                
              SET EOF-OK TO FALSE
              SET FS-OK TO TRUE
              SET WS-CONT TO 0
       
              OPEN INPUT USUARIOS
                 PERFORM UNTIL EOF-OK
                 DISPLAY '             ID'
                         '   EMAIL'
                         '                            NOME'
                         '                             SENHA'
                         '      TELEFONE'
       
                 IF FS-OK 
                    PERFORM UNTIL EOF-OK
                       READ USUARIOS INTO WS-REGISTRO-US
                          AT END 
                             SET EOF-OK TO TRUE
                          NOT AT END 
                          ADD 1 TO WS-CONT 

                          DISPLAY 'CADASTRO '
                                   WS-CONT
                                   ': '
                                   WS-ID
                                   ' - '
                                   WS-EMAIL
                                   ' - '
                                   WS-NOME
                                   ' - '
                                   WS-SENHA
                                   ' - '
                                   WS-PAIS
                                   ' '
                                   WS-DDD
                                   ' '
                                   WS-TEL
                                  
                                   
                       END-PERFORM
                 ELSE
                    DISPLAY 'ERRO AO ABRIR O ARQUIVO DE USUARIOS.'
                    DISPLAY 'FILE STATUS ERROR: ' WS-FS
                    GOBACK
                 END-IF
       
                 END-PERFORM
          
                 CLOSE USUARIOS

           GOBACK.
       END PROGRAM PROGLIST.
