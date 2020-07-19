000100*================================================================*
000200 IDENTIFICATION DIVISION.                                         
000300*================================================================*
000400  PROGRAM-ID. UUGPB910.                                           
000500*----------------------------------------------------------------*
000600*  ESTAGIARIO        : DENIO BARBOSA JUNIOR                      *
000700*  DATA DE INICIO    : 08/11/2016                                *
000800*  DATA DE CONCLUSAO :                                           *
000900*----------------------------------------------------------------*
000900*  PROGRAMA PARA GERAR RELATORIO LISTANDO AS CONTAS LEVANTADAS   *
000900*  PELO BRADESCO EM SITUACAO DE SALDO DEVEDOR ATE 100 REAIS.     *
000900*  ESTE RELATORIO SERA GERADO SOMENTE APOS A DESAVERBACAO DAS    *
000900*  CONTAS.                                                       *
000900*  QUEBRA: LOGO                                                  *
000900*  CLASSIFICACAO: LOGO E CONTA                                   *
000900*  TOTALIZADORES: CONTAS E VALOR TOTAL DO SALDO DEVEDOR DA LOGO. *
000900*                 CONTAS E VALOR TOTAL DO SALDO DEVEDOR DA ORG.  *
000900*                 TOTAL DE CONTAS LOCALIZADAS NA MESMA ORG.      *
000900*----------------------------------------------------------------*
003200*================================================================*
003300 ENVIRONMENT DIVISION.                                            
003400*================================================================*
003500 CONFIGURATION SECTION.                                           
003600 SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.                          
003700*----------------------------------------------------------------*
003800 INPUT-OUTPUT SECTION.                                            
003900*----------------------------------------------------------------*
004000 FILE-CONTROL.                                                    
004100*----------------------------------------------------------------*
004200     SELECT SAIDA    ASSIGN      TO SAIDA                         
004300            FILE STATUS          IS FS-SAIDA.                     
004400                                                                  
004500     SELECT UUGBF122  ASSIGN      TO UUGBF122.                    
004600*           FILE STATUS          IS FS-UUGBF122.                  
004700*================================================================*
004800 DATA DIVISION.                                                   
004900*================================================================*
005000 FILE SECTION.                                                    
005100                                                                  
004700 FD UUGBF122.                                                     
002000 01 UUGBF122-RECORD.                                              
002100    03 UUGBF122-KEY.                                              
002200       05 UUGBF122-ORG-X.                                         
002300          07 UUGBF122-ORG       PIC  9(003).                      
002400             88 UUGBF122-HEADER-REC         VALUE 000.            
002500             88 UUGBF122-TRAILER-REC        VALUE 999.            
002600       05 UUGBF122-REQUISITO-FUNCIONAL                            
002700                                PIC  X(004).                      
002800       05 UUGBF122-REQUISITO-NEGOCIAL                             
002900                                PIC  X(004).                      
003000       05 UUGBF122-CONTA        PIC  X(019).                      
003100*                                                                 
003200    03 UUGBF122-DADOS.                                            
003300       05 UUGBF122-LOGO         PIC  9(003).                      
003400       05 UUGBF122-CPF-CLIENTE  PIC  X(011).                      
003500       05 UUGBF122-NOME-CLIENTE PIC  X(030).                      
003600       05 UUGBF122-NUMERO-BENEFICIO                               
003700                                PIC  X(019).                      
003800       05 UUGBF122-CODIGO-BLOQUEIO-1                              
003900                                PIC  X(001).                      
004000       05 UUGBF122-CODIGO-BLOQUEIO-2                              
004100                                PIC  X(001).                      
004200       05 UUGBF122-DIAS-ATRASO  PIC  9(005).                      
004300       05 UUGBF122-SALDO-CONTA  PIC  9(011)V99.                   
004400       05 UUGBF122-SINAL-SALDO-CONTA                              
004500                                PIC  X(001).                      
004600       05 UUGBF122-PRIORIDADE-BLOQUEIO-1                          
004700                                PIC  9(002).                      
004800       05 UUGBF122-PRIORIDADE-BLOQUEIO-2                          
004900                                PIC  9(002).                      
005000       05 UUGBF122-COD-BLOQ-CANCELAMENTO                          
005100                                PIC  X(001).                      
005200       05 UUGBF122-PRI-BLOQ-CANCELAMENTO                          
005300                                PIC  9(002).                      
005400       05 UUGBF122-DATA-VENCIMENTO                                
005500                                PIC  9(007).                      
005600       05 UUGBF122-EVITA-EXPURGO                                  
005700                                PIC  X(001).                      
005800       05 UUGBF122-INT-STATUS   PIC  X(001).                      
005810       05 UUGBF122-DATA-ULT-COMPRA   PIC  9(007) COMP-3.          
005820       05 UUGBF122-SALDO-ATUAL       PIC  9(009)V99 COMP-3.       
005830       05 UUGBF122-DESCONSIGNADO     PIC  X(001).                 
005900       05 FILLER                PIC  X(009).                      
004900                                                                  
003000 FD SAIDA                                                         
003100     RECORD CONTAINS 133 CHARACTERS                               
003200     BLOCK CONTAINS 0 RECORDS                                     
003300     RECORDING MODE F.                                            
003400 01 SAIDA-RELATORIO       PIC X(133).                             
003500*----------------------------------------------------------------*
006900 WORKING-STORAGE SECTION.                                         
007000*----------------------------------------------------------------*
007100* VARIAVEIS PARA MONTAGEM DA DATA E HORA DE CRIACAO DO ARQUIVOS   
007200*----------------------------------------------------------------*
006600 01 CABECALHO.                                                    
006700    03 PRIMEIRA-LINHA.                                            
006800       05 FILLER              PIC X(014) VALUE 'XXXXXXX - R01'.   
006900       05 FILLER              PIC X(034) VALUE SPACES.            
007000       05 FILLER              PIC X(036) VALUE                    
007100          'FIDELITY PROCESSADORA E SERVI OS S/A'.                 
007200       05 FILLER              PIC X(052) VALUE SPACES.            
007900       05 FILLER              PIC X(026) VALUE                    
008000             '               FILE DATE: '.                        
008100       05 WS-DATA-SISTEMA.                                        
008200          07 WS-DIA-SISTEMA   PIC 9(002) VALUE ZEROS.             
008300          07 FILLER           PIC X(001) VALUE '/'.               
008400          07 WS-MES-SISTEMA   PIC 9(002) VALUE ZEROS.             
008500          07 FILLER           PIC X(001) VALUE '/'.               
008600          07 WS-ANO-SISTEMA   PIC 9(004) VALUE ZEROS.             
008700       05 FILLER              PIC X(008) VALUE '    PAGE'.        
008800       05 WS-PAG              PIC Z.ZZZ.ZZ9.                      
007300    03 SEGUNDA-LINHA.                                             
007400       05 SAIDA-ORG           PIC 9(003) VALUE ZEROS.             
007500       05 FILLER              PIC X(003) VALUE ' - '.             
008100       05 SAIDA-DESC-ORG      PIC X(030) VALUE SPACES.            
007500       05 FILLER              PIC X(007) VALUE SPACES.            
000000       05 FILLER              PIC X(046) VALUE                    
000000           'AJUSTES SALDO DEVEDOR PRODUTO INSS VISION PLUS'.      
007600       05 FILLER              PIC X(017) VALUE                    
000000           '        PROC DATE'.                                   
010300       05 SAIDA-PROC-DATA.                                        
010400          07 SAIDA-PROC-DIA   PIC 9(002) VALUE ZEROS.             
010500          07 FILLER           PIC X(001) VALUE '/'.               
010600          07 SAIDA-PROC-MES   PIC 9(002) VALUE ZEROS.             
010700          07 FILLER           PIC X(001) VALUE '/'.               
010800          07 SAIDA-PROC-ANO   PIC 9(004) VALUE ZEROS.             
010000       05 FILLER              PIC X(008) VALUE '  TIME: '.        
010100       05 WS-HORA-SISTEMA.                                        
010200          07 WS-HH-SISTEMA    PIC 9(002) VALUE ZEROS.             
010300          07 FILLER           PIC X(001) VALUE ':'.               
010400          07 WS-MM-SISTEMA    PIC 9(002) VALUE ZEROS.             
010500          07 FILLER           PIC X(001) VALUE ':'.               
010600          07 WS-SS-SISTEMA    PIC 9(002) VALUE ZEROS.             
008900    03 TERCEIRA-LINHA.                                            
010800       05 FILLER              PIC X(133) VALUE SPACES.            
010700    03 QUARTA-LINHA.                                              
010800       05 FILLER              PIC X(005) VALUE 'LOGO '.           
009600       05 SAIDA-LOGO          PIC 9(003) VALUE ZEROS.             
000000       05 FILLER              PIC X(002) VALUE ': '.              
008100       05 SAIDA-DESC-LOGO     PIC X(030) VALUE SPACES.            
010900    03 QUINTA-LINHA.                                              
011000       05 FILLER              PIC X(133) VALUE SPACES.            
000000    03 SEXTA-LINHA.                                               
000000       05 FILLER              PIC X(005) VALUE SPACES.            
000000       05 FILLER              PIC X(005) VALUE 'CONTA'.           
000000       05 FILLER              PIC X(017) VALUE SPACES.            
000000       05 FILLER              PIC X(003) VALUE 'CPF'.             
000000       05 FILLER              PIC X(021) VALUE SPACES.            
000000       05 FILLER              PIC X(007) VALUE 'NOME DO'.         
000000       05 FILLER              PIC X(022) VALUE SPACES.            
000000       05 FILLER              PIC X(009) VALUE 'NÚMERO DO'.       
000000       05 FILLER              PIC X(008) VALUE SPACES.            
000000       05 FILLER              PIC X(004) VALUE 'COD.'.            
000000       05 FILLER              PIC X(004) VALUE SPACES.            
000000       05 FILLER              PIC X(004) VALUE 'COD.'.            
000000       05 FILLER              PIC X(010) VALUE SPACES.            
000000       05 FILLER              PIC X(005) VALUE 'SALDO'.           
000000    03 SETIMA-LINHA.                                              
000000       05 FILLER              PIC X(051) VALUE SPACES.            
000000       05 FILLER              PIC X(007) VALUE 'CLIENTE'.         
000000       05 FILLER              PIC X(022) VALUE SPACES.            
000000       05 FILLER              PIC X(009) VALUE 'BENEFICIO'.       
000000       05 FILLER              PIC X(008) VALUE SPACES.            
000000       05 FILLER              PIC X(005) VALUE 'BLOQ1'.           
000000       05 FILLER              PIC X(003) VALUE SPACES.            
000000       05 FILLER              PIC X(005) VALUE 'BLOQ1'.           
000000       05 FILLER              PIC X(008) VALUE SPACES.            
000000       05 FILLER              PIC X(007) VALUE 'DEVEDOR'.         
012500 01 RELATORIO-CORPO.                                              
012700    03 SAIDA-CONTA            PIC X(019) VALUE SPACES.            
012800    03 FILLER                 PIC X(005) VALUE SPACES.            
012900    03 SAIDA-CPF              PIC X(011) VALUE SPACES.            
013000    03 FILLER                 PIC X(005) VALUE SPACES.            
013100    03 SAIDA-NOME             PIC X(030) VALUE SPACES.            
013200    03 FILLER                 PIC X(005) VALUE SPACES.            
013300    03 SAIDA-NUM-BENEFICIO    PIC X(019) VALUE SPACES.            
013400    03 FILLER                 PIC X(005) VALUE SPACES.            
013500    03 SAIDA-BC1              PIC X(001) VALUE SPACES.            
013600    03 FILLER                 PIC X(007) VALUE SPACES.            
013500    03 SAIDA-BC2              PIC X(001) VALUE SPACES.            
013600    03 FILLER                 PIC X(006) VALUE SPACES.            
013700    03 SAIDA-SALDO-CONTA      PIC ZZ.ZZZ.ZZZ.ZZ9.99.              
016000*----------------------------------------------------------------*
016100* VARIAVEIS CONTROLE                                              
016200*----------------------------------------------------------------*
016300 01 CONTROLADORES.                                                
016400    03 FS-ENTRADA               PIC X(002) VALUE '00'.            
016500    03 FS-SAIDA                 PIC X(002) VALUE '00'.            
016600    03 FS-UUGBF122              PIC X(002) VALUE '00'.            
016700*                                                                 
017000    03 WS-FIM-UUGBF122          PIC X(001) VALUE 'N'.             
017100*                                                                 
017200    03 WS-ORG-ANT               PIC 9(003) VALUE ZEROS.           
017300    03 WS-LOGO-ANT              PIC 9(003) VALUE ZEROS.           
017400*                                                                 
017500 01 CONTADORES.                                                   
017600    03 WS-CONT-REG              PIC 9(007) VALUE ZEROS.           
017700    03 WS-CONT-REG-ORG          PIC 9(007) VALUE ZEROS.           
017800    03 WS-CONT-REG-LOGO         PIC 9(007) VALUE ZEROS.           
017900    03 WS-CONT-LINHAS           PIC 9(002) VALUE 00.              
018000    03 WS-CONT-PAG              PIC 9(007) VALUE ZEROS.           
018100    03 WS-MAX-LINHAS            PIC 9(002) VALUE 60.              
000000    03 WS-DEV-LOGO              PIC 9(013) VALUE ZEROS.           
000000    03 WS-DEV-ORG               PIC 9(013) VALUE ZEROS.           
018400*                                                                 
018500 01 IMPRESSAO.                                                    
019400    03 WS-IMPRIME-TOTAL-REG.                                      
019500       05 FILLER                  PIC X(028) VALUE                
019600       'TOTAL DE CONTAS LIDAS.....: '.                            
019700       05 WS-TOTAL-REG            PIC Z.ZZZ.ZZ9.                  
018600    03 WS-IMPRIME-REG-ORG.                                        
018700       05 FILLER                  PIC X(028) VALUE                
018800       'TOTAL DE CONTAS DA ORG....: '.                            
018900       05 WS-REG-ORG              PIC Z.ZZZ.ZZ9.                  
000000    03 WS-IMPRIME-TOTAL-SALDO-ORG.                                
000000       05 FILLER                  PIC X(028) VALUE                
019600       'TOTAL DO SALDO DA ORG.....: '.                            
000000       05 WS-IMPRIME-SALDO-ORG    PIC ZZZZZZZZZZ9.99.             
019000    03 WS-IMPRIME-REG-LOGO.                                       
019100       05 FILLER                  PIC X(028) VALUE                
019200       'TOTAL DE CONTAS NA LOGO...: '.                            
019300       05 WS-REG-LOGO             PIC Z.ZZZ.ZZ9.                  
000000    03 WS-IMPRIME-TOTAL-SALDO-LOGO.                               
000000       05 FILLER                  PIC X(028) VALUE                
019600       'TOTAL DO SALDO DA LOGO....: '.                            
000000       05 WS-IMPRIME-SALDO-LOGO   PIC ZZZZZZZZZZ9.99.             
020600*----------------------------------------------------------------*
020700* VARIAVEIS PARA MONTAGEM DA DATA E HORA DE CRIACAO DO ARQUIVOS   
020800*----------------------------------------------------------------*
020900 01 MONTADORES-DATA.                                              
021000    03 WS-DATA-SYS.                                               
021100       05 WS-DT-SYS-AAAA         PIC 9(004) VALUE ZEROS.          
021200       05 WS-DT-SYS-MM           PIC 9(002) VALUE ZEROS.          
021300       05 WS-DT-SYS-DD           PIC 9(002) VALUE ZEROS.          
021400    03 HORA-SISTEMA.                                              
021500       05 WS-HH-SYS              PIC 9(002) VALUE ZEROS.          
021600       05 WS-MM-SYS              PIC 9(002) VALUE ZEROS.          
021700       05 WS-SS-SYS              PIC 9(002) VALUE ZEROS.          
021800    03 DATE-OPEN-AUX             PIC 9(007) VALUE ZEROS.          
021900    03 WS-DATA-AUX.                                               
022000       05 WS-DIA-AUX             PIC 9(002) VALUE ZEROS.          
022100       05 WS-MES-AUX             PIC 9(002) VALUE ZEROS.          
022200       05 WS-ANO-AUX             PIC 9(004) VALUE ZEROS.          
022300    03 DATE-OPEN-PROC            PIC 9(007) VALUE ZEROS.          
022400    03 WS-DATA-PROC.                                              
022500       05 WS-DIA-PROC-DATA       PIC 9(002) VALUE ZEROS.          
022600       05 WS-MES-PROC-DATA       PIC 9(002) VALUE ZEROS.          
022700       05 WS-ANO-PROC-DATA       PIC 9(004) VALUE ZEROS.          
022800*----------------------------------------------------------------*
022900*            BOOKS  PARA  ACESSO  AOS ARQUIVOS                   *
023000*----------------------------------------------------------------*
023100*                                                                 
023200     COPY AR00WS.                                                 
023600*                                                                 
023700     COPY AMCRIO.                                                 
023800     COPY AMCRRL.                                                 
023900     COPY AMCRRB.                                                 
024000*                                                                 
024500     COPY CCS302.                                                 
024600     COPY CCS301.                                                 
024700*================================================================*
024800 PROCEDURE DIVISION.                                              
024900*================================================================*
025000 ROTINA-PRINCIPAL.                                                
025100     MOVE 'UUGP'                     TO WS-ABEND-PROG-ID.         
025200*                                                                 
025300     PERFORM ABRIR-ARQUIVOS          THRU ABRIR-ARQUIVOS-FIM.     
025400*                                                                 
025300     PERFORM PROCESSA                THRU PROCESSA-FIM            
000000                         UNTIL WS-FIM-UUGBF122 = 'S'.             
025700     MOVE WS-CONT-REG           TO WS-TOTAL-REG.                  
025700     MOVE WS-CONT-REG-ORG       TO WS-REG-ORG.                    
025700     MOVE WS-CONT-REG-LOGO      TO WS-REG-LOGO.                   
025700     MOVE WS-DEV-LOGO           TO WS-IMPRIME-SALDO-LOGO.         
025700     MOVE WS-DEV-ORG            TO WS-IMPRIME-SALDO-ORG.          
000000*                                                                 
038300     WRITE SAIDA-RELATORIO      FROM WS-IMPRIME-REG-LOGO.         
038300     WRITE SAIDA-RELATORIO      FROM WS-IMPRIME-TOTAL-SALDO-LOGO. 
000000*                                                                 
038400     WRITE SAIDA-RELATORIO      FROM WS-IMPRIME-REG-ORG.          
038400     WRITE SAIDA-RELATORIO      FROM WS-IMPRIME-TOTAL-SALDO-ORG.  
000000*                                                                 
025800     WRITE SAIDA-RELATORIO      FROM WS-IMPRIME-TOTAL-REG.        
025900*                                                                 
026000     PERFORM FECHAR-ARQUIVOS    THRU FECHAR-ARQUIVOS-FIM.         
026100*                                                                 
026400     DISPLAY WS-IMPRIME-TOTAL-REG.                                
026700*                                                                 
026800     STOP RUN.                                                    
026900*                                                                 
027000 ROTINA-PRINCIPAL-FIM.                                            
027100*                                                                 
027200*------------------ABERTURA DE ARQUIVOS                           
027300 ABRIR-ARQUIVOS.                                                  
027400*                                                                 
027900     MOVE 'OPCR'                     TO WS-ABEND-LOCATION.        
028000     SET AMCRRB-OPEN-INPUT-RDM       TO TRUE.                     
028100     PERFORM AMCRPD-ACCESS  THRU AMCRPD-ACCESS-EXIT.              
000000*                                                                 
017400     MOVE 'OPE1'              TO WS-ABEND-LOCATION.               
017500     OPEN INPUT UUGBF122.                                         
017600     IF FS-UUGBF122 NOT EQUAL '00'                                
017700     ,  DISPLAY '**************************'                      
017800     ,  DISPLAY '**************************'                      
017900     ,  DISPLAY '** OPEN ERROR UUGBF122O   **'                    
018000     ,  DISPLAY '** STATUS = ' FS-UUGBF122                        
018100     ,  DISPLAY '**************************'                      
018200     ,  DISPLAY '**************************'                      
018300     ,  PERFORM CCSI-ABEND         THRU CCSI-ABEND-EXIT           
018400     END-IF.                                                      
020000*                                                                 
026500     MOVE 'OPSD'                     TO WS-ABEND-LOCATION.        
026600     OPEN OUTPUT SAIDA.                                           
026700     IF FS-SAIDA NOT EQUAL '00'                                   
026800     ,  DISPLAY '*************************'                       
026900     ,  DISPLAY '*************************'                       
027000     ,  DISPLAY '** OPEN ERROR SAIDA   **'                        
027100     ,  DISPLAY '** STATUS = ' FS-SAIDA                           
027200     ,  DISPLAY '*************************'                       
027300     ,  DISPLAY '*************************'                       
027400     ,  MOVE 'ERRO OPEN SAIDA '   TO WS-ABENDMSG8                 
027500     ,  PERFORM CCSI-ABEND         THRU CCSI-ABEND-EXIT           
027600     END-IF.                                                      
027700*                                                                 
020100 ABRIR-ARQUIVOS-FIM. EXIT.                                        
029500*------------------FIM DA ABERTURA DOS ARQUIVOS                   
041400*----------------------------------------------------------------*
041500 PROCESSA.                                                        
041600*                                                                 
042800     PERFORM LER-UUGBF122  THRU LER-UUGBF122-FIM.                 
043000*                                                                 
043100     IF  SAIDA-ORG     NOT EQUAL WS-ORG-ANT                       
043200     AND WS-ORG-ANT    NOT EQUAL ZEROS                            
043300     ,   MOVE WS-CONT-REG-ORG   TO WS-REG-ORG                     
043400     ,   MOVE WS-CONT-REG-LOGO  TO WS-REG-LOGO                    
025700     ,   MOVE WS-DEV-LOGO       TO WS-IMPRIME-SALDO-LOGO          
025700     ,   MOVE WS-DEV-ORG        TO WS-IMPRIME-SALDO-ORG           
043500     ,   WRITE SAIDA-RELATORIO  FROM WS-IMPRIME-REG-LOGO          
038300     ,   WRITE SAIDA-RELATORIO  FROM WS-IMPRIME-TOTAL-SALDO-LOGO  
043700     ,   WRITE SAIDA-RELATORIO  FROM WS-IMPRIME-REG-ORG           
038300     ,   WRITE SAIDA-RELATORIO  FROM WS-IMPRIME-TOTAL-SALDO-ORG   
043900     ,   MOVE 0 TO WS-CONT-REG-ORG                                
044000     ,   MOVE 0 TO WS-CONT-REG-LOGO                               
044100     ,   MOVE 0 TO WS-CONT-LINHAS                                 
044000     ,   MOVE 0 TO WS-DEV-LOGO                                    
044100     ,   MOVE 0 TO WS-DEV-ORG                                     
044500     ,   PERFORM MONTAR-CABECALHO                                 
044600     ,           THRU MONTAR-CABECALHO-EXIT                       
044700     END-IF.                                                      
044800     IF  SAIDA-LOGO    NOT EQUAL WS-LOGO-ANT                      
044900     AND WS-LOGO-ANT   NOT EQUAL ZEROS                            
045000     ,   MOVE WS-CONT-REG-LOGO TO WS-REG-LOGO                     
025700     ,   MOVE WS-DEV-LOGO      TO WS-IMPRIME-SALDO-LOGO           
045100     ,   WRITE SAIDA-RELATORIO FROM WS-IMPRIME-REG-LOGO           
045300     ,   WRITE SAIDA-RELATORIO FROM WS-IMPRIME-TOTAL-SALDO-LOGO   
045500     ,   MOVE 0 TO WS-CONT-REG-LOGO                               
045600     ,   MOVE 0 TO WS-CONT-LINHAS                                 
044000     ,   MOVE 0 TO WS-DEV-LOGO                                    
045900     ,   PERFORM MONTAR-CABECALHO                                 
046000     ,           THRU MONTAR-CABECALHO-EXIT                       
046100     END-IF.                                                      
046200     PERFORM INICIO-REL THRU FIM-REL.                             
046300*                                                                 
046400  PROCESSA-FIM. EXIT.                                             
025600*----------------------LEITURA DO UUGBF122-----------------------*
025700 LER-UUGBF122.                                                    
025800     MOVE 'RDUU'                    TO WS-ABEND-LOCATION.         
025900     READ UUGBF122.                                               
026800*                                                                 
026900     IF FS-UUGBF122 NOT = ZEROS                                   
027000     ,  IF FS-UUGBF122 NOT = 10                                   
027100     ,  ,  DISPLAY '*************************'                    
027200     ,  ,  DISPLAY '*************************'                    
027300     ,  ,  DISPLAY '**    READ UUGBF122    **'                    
027400     ,  ,  DISPLAY '** STATUS = ' FS-UUGBF122                     
027500     ,  ,  DISPLAY '*************************'                    
027600     ,  ,  DISPLAY '*************************'                    
027700     ,  PERFORM CCSI-ABEND        THRU CCSI-ABEND-EXIT            
027800     END-IF.                                                      
000000*                                                                 
030000    IF FS-UUGBF122 = 10                                           
026200    ,  MOVE 'S'              TO WS-FIM-UUGBF122                   
030400    ,  GO                    TO LER-UUGBF122-FIM                  
030500    END-IF.                                                       
030600*                                                                 
031300     IF FS-UUGBF122 = '00' AND UUGBF122-SALDO-CONTA <= 100        
041700     ,  MOVE SAIDA-ORG                TO WS-ORG-ANT               
041800     ,  MOVE SAIDA-LOGO               TO WS-LOGO-ANT              
041900     ,  MOVE UUGBF122-ORG             TO SAIDA-ORG                
042000     ,  MOVE UUGBF122-LOGO            TO SAIDA-LOGO               
042100     ,  MOVE UUGBF122-ACCT            TO SAIDA-ACCT               
042200     ,  MOVE UUGBF122-STS             TO SAIDA-STS                
042300     ,  MOVE UUGBF122-BC1             TO SAIDA-BC1                
042400     ,  MOVE UUGBF122-BC2             TO SAIDA-BC2                
042500     ,  MOVE UUGBF122-DATA-OPEN       TO DATE-OPEN-AUX            
042700     ,  MOVE UUGBF122-SALDO           TO SAIDA-SALDO-CONTA        
034500     END-IF.                                                      
000000     COMPUTE WS-DEV-LOGO = SAIDA-SALDO-CONTA + WS-DEV-LOGO.       
000000     COMPUTE WS-DEV-ORG  = SAIDA-SALDO-CONTA + WS-DEV-ORG.        
027900*                                                                 
028200 LER-UUGBF122-FIM. EXIT.                                          
037800*----------------------FIM LEITURA ENTRADA-----------------------*
046500*----------------------MONTAGEM DO RELATORIO---------------------*
046600 INICIO-REL.                                                      
046800*                                                                 
046900     IF WS-CONT-LINHAS = 00                                       
047000     , ADD 1 TO WS-CONT-PAG                                       
047100     , MOVE WS-CONT-PAG TO WS-PAG                                 
047200     , PERFORM MONTAR-CABECALHO   THRU MONTAR-CABECALHO-EXIT      
047300     END-IF.                                                      
047400*                                                                 
047500     PERFORM MONTAR-CORPO THRU MONTAR-CORPO-EXIT.                 
047600*                                                                 
047700 FIM-REL. EXIT.                                                   
047800*----------------------LEITURA DA ORG DO AMCR--------------------*
047900 LER-ORG-AMCR.                                                    
048000     MOVE 'RCRO'                     TO WS-ABEND-LOCATION.        
048100*                                                                 
048200     MOVE 'LCRO'                     TO WS-ABEND-LOCATION.        
048300     MOVE UUGBF122-ORG               TO AMCRIO-ORG.               
048400     MOVE ZEROS                      TO AMCRIO-LOGO.              
048500     MOVE 01                         TO AMCRIO-REC-NBR.           
048600     DISPLAY '##ORG-AMCR..: ' AMCRIO-ORG.                         
048700     DISPLAY '##LOGO-AMCR.: ' AMCRIO-LOGO.                        
048800     SET AMCRRB-READ-RANDOM          TO TRUE.                     
048900     PERFORM AMCRPD-ACCESS         THRU AMCRPD-ACCESS-EXIT.       
049000     MOVE AMCRIO-RECORD              TO AMCR-RECORD-ORGANIZATION. 
049100     MOVE AMCR-O-NAME-ADDR(1)        TO SAIDA-DESC-ORG.           
049200 LER-ORG-AMCR-FIM. EXIT.                                          
049300*----------------------FIM LEITURA ORG DO AMCR-------------------*
049400*----------------------LEITURA DA LOGO DO AMCR-------------------*
049500 LER-LOGO-AMCR.                                                   
049600     MOVE 'RCRO'                     TO WS-ABEND-LOCATION.        
049700*                                                                 
049800     MOVE 'LCRL'                     TO WS-ABEND-LOCATION.        
049900     MOVE UUGBF122-ORG               TO AMCRIO-ORG.               
050000     MOVE UUGBF122-LOGO              TO AMCRIO-LOGO.              
050100     MOVE 02                         TO AMCRIO-REC-NBR.           
050200     SET AMCRRB-READ-RANDOM          TO TRUE.                     
050300     PERFORM AMCRPD-ACCESS         THRU AMCRPD-ACCESS-EXIT.       
050400     MOVE AMCRIO-RECORD              TO AMCR-RECORD-LOGO-BASE.    
050500     MOVE AMCR-LB-DESCRIPTION        TO SAIDA-DESC-LOGO.          
050600 LER-LOGO-AMCR-FIM. EXIT.                                         
050700*----------------------FIM LEITURA LOGO DO AMCR------------------*
050800*----------------------LEITURA DA DATA PRCO DO AMCR--------------*
050900 LER-DATA-PROC-AMCR.                                              
051000     MOVE 'RCRO'                     TO WS-ABEND-LOCATION.        
051100*                                                                 
051200     MOVE 'LCRP'                     TO WS-ABEND-LOCATION.        
051300     MOVE ZEROS                      TO AMCRIO-ORG.               
051400     MOVE ZEROS                      TO AMCRIO-LOGO.              
051500     MOVE ZEROS                      TO AMCRIO-REC-NBR.           
051600     SET AMCRRB-READ-RANDOM          TO TRUE.                     
051700     PERFORM AMCRPD-ACCESS         THRU AMCRPD-ACCESS-EXIT.       
051800     MOVE AMCRIO-RECORD              TO AMCR-RECORD-SYSTEM.       
051900     MOVE AMCR-S-C-TODAYS-JULIAN     TO DATE-OPEN-PROC.           
052000 LER-DATA-PROC-AMCR-FIM. EXIT.                                    
052100*----------------------FIM LEITURA DA DATA PROC DO AMCR----------*
053700*----------------------MONTAGEM DO CABECALHO---------------------*
053800 MONTAR-CABECALHO.                                                
053900*----------------------------------------------------------------*
054000*-------                                                          
054100*MONTAGEM DA DATA DO CABECALHO                                    
054200*-------                                                          
054300     ACCEPT WS-DATA-SYS   FROM DATE YYYYMMDD.                     
054400     MOVE WS-DT-SYS-AAAA                TO WS-ANO-SISTEMA.        
054500     MOVE WS-DT-SYS-MM                  TO WS-MES-SISTEMA.        
054600     MOVE WS-DT-SYS-DD                  TO WS-DIA-SISTEMA.        
054700*-------                                                          
054800*MONTAGEM DA HORA                                                 
054900*-------                                                          
055000     ACCEPT  HORA-SISTEMA FROM TIME.                              
055100     MOVE WS-HH-SYS                     TO WS-HH-SISTEMA.         
055200     MOVE WS-MM-SYS                     TO WS-MM-SISTEMA.         
055300     MOVE WS-SS-SYS                     TO WS-SS-SISTEMA.         
055400*-------                                                          
055500*MONTAGEM DA DATA PROC DO AMCR (JULIAN-TO-GREG)                   
055600*-------                                                          
055700     MOVE  DATE-OPEN-PROC               TO  WS-DTE-JULIAN.        
055800     MOVE  1                            TO  WS-DATE-FORMAT.       
055900     PERFORM CCSI-JUL-TO-GREG           THRU  CCSI-JTG-EXIT.      
056000     MOVE  WS-DTE-DATE                  TO  WS-DATA-PROC.         
056100     MOVE  WS-DIA-PROC-DATA             TO  SAIDA-PROC-DIA.       
056200     MOVE  WS-MES-PROC-DATA             TO  SAIDA-PROC-MES.       
056300     MOVE  WS-ANO-PROC-DATA             TO  SAIDA-PROC-ANO.       
000000*                                                                 
044200     PERFORM LER-ORG-AMCR       THRU LER-ORG-AMCR-FIM.            
044300     PERFORM LER-LOGO-AMCR      THRU LER-LOGO-AMCR-FIM.           
044400     PERFORM LER-DATA-PROC-AMCR THRU LER-DATA-PROC-AMCR-FIM.      
038500*                                                                 
038600     WRITE SAIDA-RELATORIO FROM PRIMEIRA-LINHA.                   
038700     ADD 1               TO WS-NUM-LINHAS.                        
038900*                                                                 
039000     WRITE SAIDA-RELATORIO FROM SEGUNDA-LINHA.                    
039100     ADD 1               TO WS-NUM-LINHAS.                        
039200*                                                                 
039300     WRITE SAIDA-RELATORIO FROM TERCEIRA-LINHA.                   
039400     ADD 1               TO WS-NUM-LINHAS.                        
039200*                                                                 
039300     WRITE SAIDA-RELATORIO FROM QUARTA-LINHA.                     
039400     ADD 1               TO WS-NUM-LINHAS.                        
039200*                                                                 
039300     WRITE SAIDA-RELATORIO FROM QUINTA-LINHA.                     
039400     ADD 1               TO WS-NUM-LINHAS.                        
039200*                                                                 
039300     WRITE SAIDA-RELATORIO FROM SEXTA-LINHA.                      
039400     ADD 1               TO WS-NUM-LINHAS.                        
039200*                                                                 
039300     WRITE SAIDA-RELATORIO FROM SETIMA-LINHA.                     
039400     ADD 1               TO WS-NUM-LINHAS.                        
039600*                                                                 
039800 MONTAR-CABECALHO-EXIT. EXIT.                                     
058100*----------------------MONTAGEM DO CORPO-------------------------*
058200 MONTAR-CORPO.                                                    
058300*-------                                                          
059400     WRITE SAIDA-RELATORIO FROM RELATORIO-CORPO.                  
059500     ADD 1                 TO WS-CONT-LINHAS                      
059600                              WS-CONT-REG-ORG                     
000000                              WS-CONT-REG                         
059700                              WS-CONT-REG-LOGO.                   
059800*                                                                 
059900     IF WS-CONT-LINHAS > 59                                       
060000     , MOVE 0 TO WS-CONT-LINHAS                                   
060100     END-IF.                                                      
060200 MONTAR-CORPO-EXIT. EXIT.                                         
060300*------------------FECHAMENTO DE ARQUIVOS                         
060400 FECHAR-ARQUIVOS.                                                 
060900     MOVE 'CLCR'                    TO WS-ABEND-LOCATION          
061000     SET AMCRRB-CLOSE               TO TRUE.                      
061100     PERFORM AMCRPD-ACCESS  THRU AMCRPD-ACCESS-EXIT.              
061200*                                                                 
061700     CLOSE SAIDA.                                                 
061800     IF FS-SAIDA   NOT EQUAL '00'                                 
061900        DISPLAY '##ERRO FECHAR ARQUIVO SAIDA: ' FS-SAIDA          
062000        MOVE 'ERRO CLOSE SAIDA'   TO WS-ABENDMSG8                 
062100        PERFORM CCSI-ABEND         THRU CCSI-ABEND-EXIT           
062200     END-IF.                                                      
061200*                                                                 
061700     CLOSE SAIDA.                                                 
061800     IF FS-SAIDA   NOT EQUAL '00'                                 
061900        DISPLAY '##ERRO FECHAR ARQUIVO SAIDA: ' FS-SAIDA          
062000        MOVE 'ERRO CLOSE SAIDA'   TO WS-ABENDMSG8                 
062100        PERFORM CCSI-ABEND         THRU CCSI-ABEND-EXIT           
062200     END-IF.                                                      
062300 FECHAR-ARQUIVOS-FIM. EXIT.                                       
062400*------------------FIM DO FECHAMENTO DE ARQUIVOS                  
062500*================================================================*
062600*    BOOKS DE PROCEDURE UTILIZADOS                               *
062700*================================================================*
062800     COPY CCS502.                                                 
062900     COPY CCS508.                                                 
063100     COPY AMCRPD.                                                 