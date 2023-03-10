      ******************************************************************
      * Author: Renan Cicero
      * Date: 08/03/2023
      * Purpose: Programa para cadasreo de usuario
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG.
       
       
      ******************************************************************
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       
       
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  WS-iTEM                  PIC 9.

       77  WS-OPCAO                 PIC X.
       



      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           
                      DISPLAY ' '
           DISPLAY '*-------------------------------------------------*'
           DISPLAY '                                                   '
           DISPLAY '                    BEM VINDO:                     '
           DISPLAY '          1 - CADASTRAR NOVO USUARIO               '
           DISPLAY '          2 - LISTAR USUARIOS CADASTRADOS          '
           DISPLAY '                                                   '
           DISPLAY '                3 - PARA ENCERRAR                  '
           DISPLAY '                                                   '        
           DISPLAY '*-------------------------------------------------*'
                    ACCEPT WS-iTEM
           DISPLAY ' '
           DISPLAY ' '
           
           EVALUATE WS-iTEM
              WHEN '1'
                 CALL
                 '/home/recic/Dev/PROG01/Modulos/PROGCOLE'
                 
              WHEN '2'
                 CALL 
                 '/home/recic/Dev/PROG01/Modulos/PROGLIST'
              WHEN '3'
                 DISPLAY 'ATE MAIS'
                 PERFORM ENCERRAR
              WHEN OTHER
                 DISPLAY 'OPCAO INVALIDA'
                 PERFORM MAIN-PROCEDURE
           END-EVALUATE 

           DISPLAY ' '
           DISPLAY 'DESEJA REALIZAR UMA NOVA OPERACAO ?'
           DISPLAY ' '
           DISPLAY 'SE SIM PRESSIONE QUALQUER TECLA, CASO CONTRARIO'
                   ' PRESSIONE <F> OU <f>'
           ACCEPT WS-OPCAO
              IF WS-OPCAO = 'F' OR 'f'
                 DISPLAY 'ATE MAIS'
                 PERFORM ENCERRAR 
              ELSE 
                 PERFORM MAIN-PROCEDURE
              END-IF
                 
           
           .
       ENCERRAR.
           STOP RUN.
       END PROGRAM PROG.
      