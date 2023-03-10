      ******************************************************************
      * Author: Renan Cicero
      * Date: //2023
      * Purpose: 
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGID.
       
       
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
           03 WS-ID                    PIC 99.
           03 WS-EMAIL                 PIC X(30).
           03 WS-NOME                  PIC X(30).
           03 WS-SENHA                 PIC X(8).
           03 WS-TELEFONE              PIC 9(13).

       77  WS-FS                       PIC 99.
           88 FS-OK                    VALUE 0.  

       77  WS-EXT                      PIC X.
           88 EXT-OK                   VALUE 'F' FALSE 'N'.
        
       77  WS-EOF                      PIC X.
           88 EOF-OK                   VALUE 'F' FALSE 'N'.

       77  WS-CONT                     PIC 99. 


      ******************************************************************
       LINKAGE SECTION.
       01  LK-GERA-ID              PIC 99.

           

      ******************************************************************
       PROCEDURE DIVISION USING LK-GERA-ID.
       MAIN-PROCEDURE.
           
           SET EOF-OK TO FALSE
           SET FS-OK TO TRUE
           SET WS-CONT TO 0
       
           OPEN INPUT USUARIOS
              PERFORM UNTIL EOF-OK
                 IF FS-OK 
                    PERFORM UNTIL EOF-OK
                       READ USUARIOS INTO WS-REGISTRO-US
                          AT END 
                             SET EOF-OK TO TRUE
                          NOT AT END 
                          ADD 1 TO WS-CONT 
           
                       END-PERFORM
                 ELSE
                    DISPLAY 'ERRO AO ABRIR O ARQUIVO DE USUARIOS.'
                    DISPLAY 'FILE STATUS ERROR: ' WS-FS
                    GOBACK
                 END-IF
       
                 END-PERFORM
          
           CLOSE USUARIOS

           COMPUTE LK-GERA-ID = WS-CONT + 1

           GOBACK.
       END PROGRAM PROGID.
       