000100*================================================================*
000200*            CONTAS SELECIONADAS PARA CANCELAMENTO               *
000300*                                                                *
000400* UUGBF122 - ESTE BOOK CONTEM AS CONTAS DO BRADESCO  SELECIO-    *
000500*            NADAS PARA CANCELAMENTO.                            *
000600*                                                                *
000700* TAMANHO  - 150                                                 *
000800*                                                                *
000900* PROJETO  - 144107 - AJUSTES PLATAFORMA VISION PLUS             *
001000*                                                                *
001100* CRI      - 2288499                                             *
001200*                                                                *
001300* CUSTOM.  - Z0144107                                            *
001400*                                                                *
001500* DLMSYS   - DANIEL EDUARDO MACHALA                              *
001600*                                                                *
001700* DATA     - 28/11/2014                                          *
001800*================================================================*
001900 01  UUGBF122-RECORD.
002000*
002100     03 UUGBF122-KEY.
002200        05 UUGBF122-ORG-X.
002300           07 UUGBF122-ORG       PIC  9(003).
002400              88 UUGBF122-HEADER-REC         VALUE 000.
002500              88 UUGBF122-TRAILER-REC        VALUE 999.
002600        05 UUGBF122-REQUISITO-FUNCIONAL
002700                                 PIC  X(004).
002800        05 UUGBF122-REQUISITO-NEGOCIAL
002900                                 PIC  X(004).
003000        05 UUGBF122-CONTA        PIC  X(019).
003100*
003200     03 UUGBF122-DADOS.
003300        05 UUGBF122-LOGO         PIC  9(003).
003400        05 UUGBF122-CPF-CLIENTE  PIC  X(011).
003500        05 UUGBF122-NOME-CLIENTE PIC  X(030).
003600        05 UUGBF122-NUMERO-BENEFICIO
003700                                 PIC  X(019).
003800        05 UUGBF122-CODIGO-BLOQUEIO-1
003900                                 PIC  X(001).
004000        05 UUGBF122-CODIGO-BLOQUEIO-2
004100                                 PIC  X(001).
004200        05 UUGBF122-DIAS-ATRASO  PIC  9(005).
004300        05 UUGBF122-SALDO-CONTA  PIC  9(011)V99.
004400        05 UUGBF122-SINAL-SALDO-CONTA
004500                                 PIC  X(001).
004600        05 UUGBF122-PRIORIDADE-BLOQUEIO-1
004700                                 PIC  9(002).
004800        05 UUGBF122-PRIORIDADE-BLOQUEIO-2
004900                                 PIC  9(002).
005000        05 UUGBF122-COD-BLOQ-CANCELAMENTO
005100                                 PIC  X(001).
005200        05 UUGBF122-PRI-BLOQ-CANCELAMENTO
005300                                 PIC  9(002).
005400        05 UUGBF122-DATA-VENCIMENTO
005500                                 PIC  9(007).
005600        05 UUGBF122-EVITA-EXPURGO
005700                                 PIC  X(001).
005800        05 UUGBF122-INT-STATUS   PIC  X(001).
005810        05 UUGBF122-DATA-ULT-COMPRA   PIC  9(007) COMP-3.
005820        05 UUGBF122-SALDO-ATUAL       PIC  9(009)V99 COMP-3.
005830        05 UUGBF122-DESCONSIGNADO     PIC  X(001).
005900        05 FILLER                PIC  X(009).
