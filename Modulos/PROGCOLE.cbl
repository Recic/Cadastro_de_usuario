     ******************************************************************
      * AUTHOR: RENAN CICERO
      * DATE: 09/03/2023
      * PURPOSE: Modulo de coleta de dados
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOLE.
       
       
      ******************************************************************
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WS-COM-AREA.
           03 WS-EMAIL                 PIC X(30) VALUE SPACE.
           03 WS-NOME                  PIC X(30).
           03 WS-SENHA                 PIC X(8).
           03 WS-TELEFONE              PIC 9(13).

       01  CONT.
           03  CONT1                   PIC 99.
           03  CONT2                   PIC 99.
           03  CONT3                   PIC 99.
           03  CONT4                   PIC 99.
           03  CONT5                   PIC 99.

       77  WS-PRIMEIRO-NOME            PIC X(50).

       77  WS-SEGUNDO-NOME             PIC X(50).     

       77  WS-TL                       PIC X(13).

       77  WS-FS                       PIC 99.
           88 FS-OK                    VALUE 0.  

       77  WS-EXT                      PIC X.
           88 EXT-OK                   VALUE 'F' FALSE 'N'.


       01  WS2-COM-AREA.
           03 WS2-NUM                 PIC 9.


      ******************************************************************
       PROCEDURE DIVISION.
       
       CADASTRA-USUARIO.

           DISPLAY ' '
           DISPLAY '*-------------------------------------------------*'
           DISPLAY ' '
           DISPLAY '      PARA REGISTRAR UM NOVO USUARIO INFORME: '
           DISPLAY '                 1 - Email                         '
           DISPLAY '                 2 - Nome Completo                 '
           DISPLAY '                 3 - Senha                         '
           DISPLAY '                 4 - telefone                      '
           DISPLAY ' '
           DISPLAY '*-------------------------------------------------*'
           DISPLAY ' '
           DISPLAY ' '

           
           PERFORM VALIDA-EMAIL
           PERFORM VALIDA-NOME
           PERFORM VALIDA-SENHA
           PERFORM VALIDA-TELEFONE
           PERFORM REGISTRA-CADASTRO
            
           
       .
           
       VALIDA-EMAIL.
           INITIALIZE CONT
           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY '|O EMAIL DEVE CONTER:                             |'
           DISPLAY '|                                                 |'
           DISPLAY '| - NO MINIMO, OITO CARACTERES                    |'
           DISPLAY '| - NO MINIMO, UM @;                              |'
           DISPLAY '| - NO MINIMO, UM CARACTER ANTES DO @             |'
           DISPLAY '| - TER DOMINIO bradesco.com OU capgemini.com     |'
           DISPLAY '***************************************************'
           DISPLAY 'INSIRA UM EMAIL: 'ACCEPT WS-EMAIL
           DISPLAY ' '

           INSPECT WS-EMAIL TALLYING CONT1 FOR CHARACTERS BEFORE ' '
           IF CONT1 > 10

              INSPECT WS-EMAIL TALLYING CONT2 FOR ALL '@' 
              IF CONT2 = 1   

                 INSPECT WS-EMAIL TALLYING CONT3 FOR CHARACTERS
                 BEFORE '@'
                 IF CONT3 NOT= 0
                    
                    INSPECT WS-EMAIL TALLYING CONT4 FOR ALL   
                    'capgemini.com', ALL 'bradesco.com'
                    
                    IF CONT4 = 1
                       DISPLAY 'EMAIL VALIDO'
                    ELSE 
                       DISPLAY 'EMAIL INVALIDO'
                       PERFORM VALIDA-EMAIL
                    END-IF

                 ELSE
                    DISPLAY 'EMAIL INVALIDO'
                    PERFORM VALIDA-EMAIL
                 END-IF

              ELSE
                 DISPLAY 'EMAIL INVALIDO'
                 PERFORM VALIDA-EMAIL
              END-IF
          
           ELSE 
              DISPLAY 'EMAIL INVALIDO'
              PERFORM VALIDA-EMAIL
           END-IF
              
       .     
                 
       VALIDA-NOME.
           INITIALIZE CONT
           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY '|O NOME DEVE CONTER:                              |'
           DISPLAY '|                                                 |'
           DISPLAY '| - NO MINIMO, 2 PALAVRAS                         |'
           DISPLAY '***************************************************'
           DISPLAY 'INSIRA UM NOME: ' ACCEPT WS-NOME
           DISPLAY ' '

           UNSTRING WS-NOME DELIMITED BY SPACE 
           INTO WS-PRIMEIRO-NOME WS-SEGUNDO-NOME 
           
           INSPECT WS-SEGUNDO-NOME TALLYING CONT1 FOR CHARACTERS 
           BEFORE SPACE
           IF CONT1 > 0
               DISPLAY 'NOME VALIDO'
           ELSE
              DISPLAY 'NOME INVALIDO'
              PERFORM VALIDA-NOME 
           END-IF

       .
           
       VALIDA-SENHA.
           INITIALIZE CONT
           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY '|A SENHA DEVE CONTER:                             |'
           DISPLAY '|                                                 |'
           DISPLAY '| - 8 CARACTERES;                                 |'
           DISPLAY '| - NO MINIMO, UMA LETRA MAIUSCULA;               |'
           DISPLAY '| - NO MINIMO, UMA LETRA MINUSCULA; E             |'
           DISPLAY '| - NO MINIMO, UM NUMERO                          |'
           DISPLAY '************************ **************************'
           DISPLAY 'INSIRA UMA SENHA: ' ACCEPT WS-SENHA
           DISPLAY ' '

           INSPECT WS-SENHA TALLYING CONT1 FOR CHARACTERS BEFORE SPACE
           IF CONT1 = 8

              INSPECT WS-SENHA TALLYING CONT2 FOR ALL 'A',ALL 'B',
              ALL 'C',ALL 'D',ALL 'E',ALL 'F',ALL 'G',ALL 'H',ALL 'I',
              ALL 'J',ALL 'K',ALL 'L',ALL 'M',ALL 'N',ALL 'O',ALL 'P',
              ALL 'Q',ALL 'R',ALL 'S',ALL 'T',ALL 'U',ALL 'V',ALL 'X',
              ALL 'W',ALL 'Y',ALL 'Z' 
              BEFORE SPACE 
              IF CONT2 >= 1

                 INSPECT WS-SENHA TALLYING CONT3 FOR ALL 'a',ALL 'b',
                 ALL 'c',ALL 'd',ALL 'e',ALL 'f',ALL 'g',ALL 'h',
                 ALL 'i',ALL 'j',ALL 'k',ALL 'l',ALL 'm',ALL 'n',
                 ALL 'o',ALL 'p',ALL 'q',ALL 'r',ALL 's',ALL 't',
                 ALL 'u',ALL 'v',ALL 'x',ALL 'w',ALL 'y',ALL 'z',
                 BEFORE  SPACE 
                 IF CONT3 >= 1
                    
                    INSPECT WS-SENHA TALLYING CONT4 FOR ALL '0',
                    ALL '1',ALL '2',ALL '3',ALL '4',ALL '5',ALL '6',
                    ALL '7',ALL '8',ALL '9',
                    BEFORE SPACE 
                    IF CONT4 >= 1
                       
                       INSPECT WS-SENHA TALLYING CONT5 FOR ALL '!',
                       ALL '@',ALL '#',ALL '$',ALL '%',ALL '&',ALL '*',
                       ALL '(',ALL ')',ALL '_',ALL '-',ALL '\',ALL '|',
                       ALL '?',ALL '/',ALL '>',ALL '<',ALL '[',ALL ']',
                       ALL '{',ALL '}',ALL ';',ALL ':',ALL '+',ALL '=',
                       BEFORE SPACE 
                       IF CONT5 >= 1
                           DISPLAY 'SENHA VALIDA'
                       ELSE
                          DISPLAY 'SENHA INVALIDA'
                          PERFORM VALIDA-SENHA
                       END-IF

                    ELSE
                       DISPLAY 'SENHA INVALIDA'
                       PERFORM VALIDA-SENHA
                    END-IF

                 ELSE
                    DISPLAY 'SENHA INVALIDA'
                    PERFORM VALIDA-SENHA
                 END-IF
              
              ELSE
                 DISPLAY 'SENHA INVALIDA'
                 PERFORM VALIDA-SENHA
              END-IF

           ELSE
              DISPLAY 'SENHA INVALIDA'
              PERFORM VALIDA-SENHA 
           END-IF
           
       .

       VALIDA-TELEFONE.
           
           INITIALIZE CONT
           DISPLAY ' '
           DISPLAY '***************************************************'
           DISPLAY '|O TELEFONE DEVE CONTER:                          |'
           DISPLAY '|                                                 |'
           DISPLAY '| - NO MAXIMO, 13 DIGITOS                         |'
           DISPLAY '| - NO MINIMO, 11 DIGITOS;                        |'
           DISPLAY '***************************************************'
           DISPLAY 'INSIRA UM NUMERO DE TELEFONE' ACCEPT WS-TL
           DISPLAY ' '

           INSPECT WS-TL TALLYING CONT1 FOR CHARACTERS BEFORE SPACE 
           DISPLAY CONT1

           IF CONT1 >= 11 AND <= 13
               DISPLAY 'TELEFONE VALIDO'
               MOVE WS-TL TO WS-TELEFONE
           ELSE
              DISPLAY 'TELEFONE INVALIDO'
              PERFORM VALIDA-TELEFONE
           END-IF
            
                     
       .
           
       REGISTRA-CADASTRO.
    
           CALL '/home/recic/Dev/PROG01/Modulos/PROGCADS'
           USING WS-COM-AREA

       .

       
           GOBACK.
       END PROGRAM PROGCOLE.
