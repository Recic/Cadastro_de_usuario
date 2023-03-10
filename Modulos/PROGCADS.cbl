      ******************************************************************
      * AUTHOR: RENAN CICERO
      * DATE: 08/03/2023
      * PURPOSE: Modulo de cadastro
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCADS.
       
       
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
           ACCESS MODE IS RANDOM 
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
       77  WS-FS                       PIC 99.
           88 FS-OK                    VALUE 0.  

       77  WS-EXT                      PIC X.
           88 EXT-OK                   VALUE 'F' FALSE 'N'.   

       77  WS-GERA-ID                  PIC 99 VALUE 0.        


      ******************************************************************
       LINKAGE SECTION.
       01  LK-COM-AREA.
           03 LK-EMAIL                 PIC X(30).
           03 LK-NOME                  PIC X(30).
           03 LK-SENHA                 PIC X(8).
           03 LK-TELEFONE              PIC 9(13). 

      ******************************************************************
       PROCEDURE DIVISION USING LK-COM-AREA.

        
           SET EXT-OK TO TRUE

           OPEN I-O USUARIOS

           IF WS-FS EQUAL TO 35
              OPEN OUTPUT USUARIOS
              ADD 1 TO WS-GERA-ID
           ELSE
              CALL '/home/recic/Dev/PROG01/Modulos/PROGID'
              USING WS-GERA-ID
                 IF WS-GERA-ID > 0 
                    DISPLAY 'ID GERADO COM SUCESSO'
                 ELSE
                    DISPLAY 'FALHA AO GERAR ID'
                    GOBACK
                 END-IF
           END-IF 

           IF FS-OK

              MOVE WS-GERA-ID   TO ID-USUARIO
              MOVE LK-EMAIL     TO EMAIL
              MOVE LK-NOME      TO NOME
              MOVE LK-SENHA     TO SENHA
              MOVE LK-TELEFONE  TO TELEFONE

              WRITE REG-USUARIO
                 INVALID KEY 
                    DISPLAY 'USUARIO JA CADASTRADO'
                 NOT INVALID KEY 
                    DISPLAY 'USUARIO CADASTRADO OM SUCESSO'

           ELSE 
              DISPLAY 'ERRO AO ABRIR O ARQUIVO DE USUARIOS'
              DISPLAY 'FILE STATUS: 'WS-FS
           END-IF 

           CLOSE USUARIOS
           
           .

           GOBACK.
       END PROGRAM PROGCADS.
