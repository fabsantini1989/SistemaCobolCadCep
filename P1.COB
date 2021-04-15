       IDENTIFICATION DIVISION.
       PROGRAM-ID. P1.
       AUTHOR. FABIANO SANTINI MARQUES.
      ***********************************************
      * MANUTENCAO DO CADASTRO CEP   *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CEP-LOGRA
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CEP-NUMCEP          PIC 9(08).
                03 CEP-LOGRA           PIC X(35).
                03 CEP-BAIRRO          PIC X(20).
                03 CEP-CIDADE          PIC X(20).
                03 CEP-UF              PIC X(02).
                03 CEP-REF             PIC X(35).
                03 CEP-STATUS          PIC X(01).
                03 CEP-OBS             PIC X(20).
                03 CEP-DATA            PIC 9(08).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL          PIC 9(01) VALUE ZEROS.
       77 W-CONT         PIC 9(06) VALUE ZEROS.
       77 W-OPCAO        PIC X(01) VALUE SPACES.
       77 ST-ERRO        PIC X(02) VALUE "00". 
       77 W-ACT          PIC 9(02) VALUE ZEROS.
       77 MENS           PIC X(50) VALUE SPACES.
       77 LIMPA          PIC X(50) VALUE SPACES.
      *
       01 TXUF.
            03 UF-SIGLA   PIC X(02) VALUE SPACES.
            03 UF-TEXTO   PIC X(20) VALUE SPACES.
       01 TBUF.
             03 FILLER     PIC X(22) VALUE "ACRE".
             03 FILLER     PIC X(22) VALUE "ALAGOAS".
             03 FILLER     PIC X(22) VALUE "AMAZONAS".
             03 FILLER     PIC X(22) VALUE "AMAPA".
             03 FILLER     PIC X(22) VALUE "BAHIA".
             03 FILLER     PIC X(22) VALUE "CEARA".
             03 FILLER     PIC X(22) VALUE "DISTRITO FEDERAL".
             03 FILLER     PIC X(22) VALUE "ESPIRITO SANTO".
             03 FILLER     PIC X(22) VALUE "GOIAS".
             03 FILLER     PIC X(22) VALUE "MARANHÃO".
             03 FILLER     PIC X(22) VALUE "MATO GROSSO".
             03 FILLER     PIC X(22) VALUE "MATO GROSSO DO SUL".
             03 FILLER     PIC X(22) VALUE "MINAS GERAIS".
             03 FILLER     PIC X(22) VALUE "PARA".
             03 FILLER     PIC X(22) VALUE "PARAIBA".
             03 FILLER     PIC X(22) VALUE "PARANÁ".
             03 FILLER     PIC X(22) VALUE "PERNAMBUCO".
             03 FILLER     PIC X(22) VALUE "PIAUI".
             03 FILLER     PIC X(22) VALUE "RIO DE JANEIRO".
             03 FILLER     PIC X(22) VALUE "R. GRANDE DO NORTE".
             03 FILLER     PIC X(22) VALUE "RIO GRANDE DO SUL".
             03 FILLER     PIC X(22) VALUE "RONDONIA".
             03 FILLER     PIC X(22) VALUE "RORAIMA".
             03 FILLER     PIC X(22) VALUE "SANTA CATARINA".
             03 FILLER     PIC X(22) VALUE "SAO PAULO".
             03 FILLER     PIC X(22) VALUE "SERGIPE".
             03 FILLER     PIC X(22) VALUE "TOCANTINS".
      *		   
       01 TBUFR REDEFINES TBUF.
	03 TABUF    PIC X(22) OCCURS 27 TIMES.
       01 IND            PIC 9(02) VALUE ZEROS.
      *=========================================================
       01 TSBUF.
             03 FILLER     PIC X(24) VALUE "ACALAMAPBACEDFESGOMAMTMS".
             03 FILLER     PIC X(24) VALUE "MGPAPBPRPEPIRJRNRSRORRSC".
             03 FILLER     PIC X(06) VALUE "SPSETO".
      *		   
       01 TSBUFR REDEFINES TSBUF.
          03 TSABUF    PIC X(02) OCCURS 27 TIMES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 01  COLUMN 41 
               VALUE  "---------------------------------------".
           05  LINE 02  COLUMN 01 
               VALUE  "------------------------- MANUTENCAO DO".
           05  LINE 02  COLUMN 41 
               VALUE  "CADASTRO DE CEP -----------------------".
           05  LINE 03  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 03  COLUMN 41 
               VALUE  "---------------------------------------".
           05  LINE 04  COLUMN 01 
               VALUE  " CODIGO CEP       :".
           05  LINE 06  COLUMN 01 
               VALUE  " LOGRADOURO       :".
           05  LINE 07  COLUMN 01 
               VALUE  " BAIRRO           :".
           05  LINE 08  COLUMN 01 
               VALUE  " CIDADE           :".
           05  LINE 09  COLUMN 01 
               VALUE  " UF               :".
           05  LINE 10  COLUMN 01 
               VALUE  " PONTO REFERENCIA :".
           05  LINE 11  COLUMN 01 
               VALUE  " STATUS           :".
           05  LINE 12  COLUMN 01 
               VALUE  " OBSERVACAO       :".
           05  LINE 13  COLUMN 01 
               VALUE  " DATA INCL MANUT  :".
           05  LINE 14  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 14  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 15  COLUMN 01 
               VALUE  "---------  ----  ------  ---  ----  ---".
           05  LINE 15  COLUMN 41 
               VALUE  "  ---  ------".
           05  LINE 16  COLUMN 01 
               VALUE  "--------- --  -- --  ---  -  --  -- ----".
           05  LINE 16  COLUMN 41 
               VALUE  "  --- ---  ---".
           05  LINE 17  COLUMN 01 
               VALUE  "---       ------ -----   --- ------ ----".
           05  LINE 17  COLUMN 41 
               VALUE  "- --- ---  ---".
           05  LINE 18  COLUMN 01 
               VALUE  "------    ------ ------- --- ------ ----".
           05  LINE 18  COLUMN 41 
               VALUE  "----- ---  ---".
           05  LINE 19  COLUMN 01 
               VALUE  "---       --  -- --  --- --- --  -- ---".
           05  LINE 19  COLUMN 41 
               VALUE  " ---- ---  ---".
           05  LINE 20  COLUMN 01 
               VALUE  "---       --  -- ------  --- --  -- ---".
           05  LINE 20  COLUMN 41 
               VALUE  "  ---  ------".
           05  LINE 21  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 21  COLUMN 41 
               VALUE  "----------------------------------------".   
           05  TCEP-NUMCEP
               LINE 04  COLUMN 21  PIC 99999.999
               USING  CEP-NUMCEP.			   
           05  TCEP-LOGRA
               LINE 06  COLUMN 21  PIC X(35)
               USING  CEP-LOGRA.			   
           05  TCEP-BAIRRO
               LINE 07  COLUMN 21  PIC X(20)
               USING  CEP-BAIRRO.			   
           05  TCEP-CIDADE
               LINE 08  COLUMN 21  PIC X(20)
               USING  CEP-CIDADE.			   
           05  TCEP-UF
               LINE 09  COLUMN 21  PIC X(02)
               USING  CEP-UF.
           05  TUF-TEXTO
               LINE 09  COLUMN 24  PIC X(20)
               USING  UF-TEXTO.
           05  TCEP-REF
               LINE 10  COLUMN 21  PIC X(35)
               USING  CEP-REF.
           05  TCEP-STATUS
               LINE 11  COLUMN 21  PIC X(01)
               USING  CEP-STATUS.
           05  TCEP-OBS
               LINE 12  COLUMN 21  PIC X(20)
               USING  CEP-OBS.
           05  TCEP-DATA
               LINE 13  COLUMN 21  PIC 99.99.9999
               USING  CEP-DATA.				   
			   
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCEP
                      CLOSE CADCEP
                      MOVE "* ARQUIVO CEP SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.

       INC-001.
                MOVE SPACES TO TXUF CEP-UF CEP-LOGRA CEP-BAIRRO
                MOVE SPACES TO CEP-CIDADE CEP-REF CEP-STATUS 
                MOVE ZEROS  TO CEP-NUMCEP CEP-DATA.
                DISPLAY TELACEP.

       INC-002.
                ACCEPT TCEP-NUMCEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADCEP
                   GO TO ROT-FIM.
                IF CEP-NUMCEP = SPACES 
                   MOVE "*** CEP INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.

       LER-CEP01.
                MOVE 0 TO W-SEL
                READ CADCEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-006A THRU INC-006B
                      DISPLAY TELACEP
                      MOVE "*** CEP JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. CEP"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.

       INC-003.
                ACCEPT TCEP-LOGRA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF CEP-LOGRA = SPACES 
                   MOVE "LOGRADOURO NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.

       INC-004.
                ACCEPT TCEP-BAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF CEP-BAIRRO = SPACES
                   MOVE "BAIRRO NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.

       INC-005.
                ACCEPT TCEP-CIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004
                IF CEP-CIDADE = SPACES
                   MOVE "CIDADE NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.

       INC-006.
                ACCEPT TCEP-UF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO INC-005.
       INC-006A.
               MOVE 1 TO IND.
       INC-006B.
               IF CEP-UF NOT = TSABUF(IND)
	   ADD 1 TO IND
                    IF IND > 27
                         MOVE "UF NAO EXISTE" TO MENS
                         PERFORM ROT-MENS THRU ROT-MENS-FIM
                         GO TO INC-006
                    ELSE
                         GO TO INC-006B
               ELSE
                     MOVE TABUF(IND) TO UF-TEXTO 
                     DISPLAY TUF-TEXTO.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.
       INC-007.
                ACCEPT TCEP-REF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                IF CEP-REF = SPACES
                   MOVE "REFERENCIA NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-006.
       INC-008.
                ACCEPT TCEP-STATUS
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF CEP-STATUS = SPACES
                   MOVE "STATUS NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-007.	
       INC-009.
                ACCEPT TCEP-OBS
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO INC-008.	
       INC-010.
                ACCEPT TCEP-DATA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
                IF CEP-DATA = ZEROS
                   MOVE "DATA NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.					
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.

       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS SALVOS COM SUCESSO *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE " * JÁ EXISTE NO BANCO DE DADOS * " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE " * ERRO NA GRAVACAO DO ARQUIVO * "
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.

       EXC-DL1.
                DELETE CADCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE " * ERRO NA ALTERACAO DO REGISTRO * "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
     