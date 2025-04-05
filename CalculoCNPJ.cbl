       IDENTIFICATION DIVISION.
       PROGRAM-ID. CalculoCNPJ.
       AUTHOR.     Moises. 
       ********    ABR/25.
       ********    Este programa Calcula o DV dos CNPJs com Letras.
       ********    Tentei ate usar a LLMs, sem sucesso, entao eu fiz
       ********    este programa(que pode ser modelo de modulo) 
       ********    meio na brincadeira.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CNPJ-BASE-13A.
           02 CNPJ-BASE-12N  	PIC X(12).
           02 CNPJ-BASE-12NR REDEFINES CNPJ-BASE-12N OCCURS 12 TIMES
              			    PIC X(01).
           02 DIGITO-13    	PIC 9(01).
       01  CNPJ-BASE-13AR REDEFINES CNPJ-BASE-13A OCCURS 13 TIMES 
           			        PIC X.
       01  CNPJ-1           PIC 9(01) VALUE 0.
       01  CNPJ-BASE        PIC X(12).
       01  DIGITO-VERIFICADOR-1 PIC 9.
       01  DIGITO-VERIFICADOR-2 PIC 9.
       01  PESOS-1 		    PIC 9(12) VALUE '543298765432'.
       01  PESOS-1R REDEFINES PESOS-1 OCCURS 12 TIMES 
                                PIC 9.
       01  PESOS-2 		    PIC 9(13) VALUE '6543298765432'.
       01  PESOS-2R REDEFINES PESOS-2 OCCURS 13 TIMES 
                                PIC 9. 
       01  SOMA-1 	    	PIC 9(5) VALUE 0.
       01  SOMA-2 	    	PIC 9(5) VALUE 0.
       01  RESULT-1 	  	PIC 9(05) VALUE 0.
       01  RESULT-2 	  	PIC 9(05) VALUE 0.
       01  RESTO-1  	  	PIC 9(05) VALUE 0.
       01  RESTO-2        	PIC 9(05) VALUE 0.
       01  I              	PIC 9(02) VALUE 0.
       01  WS-LETRA       	PIC X(01) VALUE SPACES.
       01  WS-IND         	PIC 9(02) VALUE 0.
       01  WS-VRASCII-48  	PIC 9(02) VALUE 0.
       01  WS-NAO-ACHOU-NADA    PIC 9 VALUE 0.
       01  CNPJ-NRW         PIC 9(05) VALUE 0.  

       01  TAB-ASCII.
           02 TAB-ALFA-ASCII.
              10 FILLER   PIC X(03) VALUE "A65".
              10 FILLER   PIC X(03) VALUE "B66".
              10 FILLER   PIC X(03) VALUE "C67".
              10 FILLER   PIC X(03) VALUE "D68".
              10 FILLER   PIC X(03) VALUE "E69".
              10 FILLER   PIC X(03) VALUE "F70".
              10 FILLER   PIC X(03) VALUE "G71".
              10 FILLER   PIC X(03) VALUE "H72".
              10 FILLER   PIC X(03) VALUE "I73".
              10 FILLER   PIC X(03) VALUE "J74".
              10 FILLER   PIC X(03) VALUE "K75".
              10 FILLER   PIC X(03) VALUE "L76".
              10 FILLER   PIC X(03) VALUE "M77".
              10 FILLER   PIC X(03) VALUE "N78".
              10 FILLER   PIC X(03) VALUE "O79".
              10 FILLER   PIC X(03) VALUE "P80".
              10 FILLER   PIC X(03) VALUE "Q81".
              10 FILLER   PIC X(03) VALUE "R82".
              10 FILLER   PIC X(03) VALUE "S83".
              10 FILLER   PIC X(03) VALUE "T84".
              10 FILLER   PIC X(03) VALUE "U85".
              10 FILLER   PIC X(03) VALUE "V86".
              10 FILLER   PIC X(03) VALUE "W87".
              10 FILLER   PIC X(03) VALUE "X88".
              10 FILLER   PIC X(03) VALUE "Y89".
              10 FILLER   PIC X(03) VALUE "Z90".
           02 TAB-ALFAR-ASCII-R REDEFINES TAB-ALFA-ASCII  
                                OCCURS 26 TIMES INDEXED BY IDZ.
              10 LETRA    PIC X(01).
              10 VRASCII  PIC 9(02).    

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY 'Digite os 12 primeiros dígitos do CNPJ: '.
           ACCEPT CNPJ-BASE.
           MOVE FUNCTION UPPER-CASE(CNPJ-BASE) TO CNPJ-BASE-12N

           MOVE 0 TO CNPJ-NRW        
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
            IF CNPJ-BASE-12NR(I) IS ALPHABETIC
                MOVE CNPJ-BASE-12NR(I) TO WS-LETRA
                PERFORM P-SEARCH THRU S-SEARCH
                MOVE PESOS-1R(I)  TO CNPJ-NRW
                COMPUTE SOMA-1 = SOMA-1 + (WS-VRASCII-48 * 
                                 CNPJ-NRW)
            ELSE
            MOVE CNPJ-BASE-12NR(I) TO CNPJ-NRW
            COMPUTE SOMA-1 = SOMA-1 + (PESOS-1R(I) * CNPJ-NRW)
            END-IF
           END-PERFORM

           DIVIDE 11 INTO SOMA-1 GIVING RESULT-1 REMAINDER RESTO-1

           IF RESTO-1 < 2 THEN
               MOVE 0 TO DIGITO-VERIFICADOR-1
           ELSE
               COMPUTE DIGITO-VERIFICADOR-1 = 11 - RESTO-1
           END-IF.

           MOVE DIGITO-VERIFICADOR-1 TO DIGITO-13     
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 13
            IF CNPJ-BASE-13AR(I) IS ALPHABETIC
                MOVE CNPJ-BASE-13AR(I) TO WS-LETRA
                PERFORM P-SEARCH THRU S-SEARCH
                MOVE PESOS-2R(I)  TO CNPJ-NRW
                COMPUTE SOMA-2 = SOMA-2 + (WS-VRASCII-48 * 
                                 CNPJ-NRW)
            ELSE
            MOVE CNPJ-BASE-13AR(I) TO CNPJ-NRW
            COMPUTE SOMA-2 = SOMA-2 + (PESOS-2R(I) * CNPJ-NRW)
            END-IF
            END-PERFORM.
           
           DIVIDE 11 INTO SOMA-2 GIVING RESULT-2 REMAINDER RESTO-2

           IF RESTO-2 < 2 THEN
               MOVE 0 TO DIGITO-VERIFICADOR-2
           ELSE
               COMPUTE DIGITO-VERIFICADOR-2 = 11 - RESTO-2
           END-IF.

           DISPLAY 'O dígito verificador do CNPJ é: ' 
                       DIGITO-VERIFICADOR-1 DIGITO-VERIFICADOR-2.

           STOP RUN.

       P-SEARCH. 
           SET IDZ TO 1.
           SEARCH TAB-ALFAR-ASCII-R                  
                  AT END
                    MOVE 1 TO WS-NAO-ACHOU-NADA
                WHEN LETRA(IDZ) EQUAL WS-LETRA
                     COMPUTE WS-VRASCII-48 = VRASCII(IDZ) - 48
           END-SEARCH 
           IF WS-NAO-ACHOU-NADA = 1
              DISPLAY "NÃO ACHOU O CHARACTER " WS-LETRA
           END-IF.
       S-SEARCH.
           EXIT.
