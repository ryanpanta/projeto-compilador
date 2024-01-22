/*+=============================================================
 |          UNIFAL = Universidade Federal de Alfenas.
 |            BACHARELADO EM CIENCIA DA COMPUTACAO.
 | Trabalho..: Funcao com retorno
 | Disciplina: Teoria de Linguagens e Compiladores
 | Professor.: Luiz Eduardo da Silva
 | Aluno.....: Ryan Rodrigues - RA: 2021.1.08.032
 | Data......: 17/02/2023
 +=============================================================*/

%{
#include "lexico.c"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "utils.c"

int contaVar;
int rotulo = 0;
int tipo;
char escopo = 'g';
int contaPar = 0;
int varLocal;
int endFuncao;
int contaVarLocal = 0;
int posFunc;
int contaArgs = 0;
int arg[50];
int i = 0;


%}

%token T_PROGRAMA
%token T_INICIO
%token T_FIM
%token T_LEIA
%token T_ESCREVA
%token T_SE
%token T_ENTAO
%token T_SENAO
%token T_FIMSE
%token T_FACA
%token T_ENQTO
%token T_FIMENQTO
%token T_INTEIRO
%token T_LOGICO
%token T_MAIS
%token T_MENOS
%token T_VEZES
%token T_DIV
%token T_ATRIBUI
%token T_MAIOR
%token T_MENOR
%token T_IGUAL
%token T_E
%token T_OU
%token T_NAO
%token T_ABRE
%token T_FECHA
%token T_V 
%token T_F 
%token T_IDENTIF
%token T_NUMERO

%token T_RETORNE 
%token T_FUNC
%token T_FIMFUNC


%start programa 
%expect 1

%left T_E T_OU 
%left T_IGUAL 
%left T_MAIOR T_MENOR 
%left T_MAIS T_MENOS 
%left T_VEZES T_DIV 


%%


programa 
    : cabecalho 
        {   contaVar = 0; 
            escopo = 'G';
        }
    variaveis 
        { 
            empilha(contaVar, 'n');
            if (contaVar) 
                fprintf(yyout,"\tAMEM\t%d\n", contaVar); 
        }
       rotinas
       T_INICIO lista_comandos T_FIM
        { 
            int conta = desempilha('n');
            if (conta)
                fprintf(yyout,"\tDMEM\t%d\n", conta); 
            fprintf(yyout,"\tFIMP\n");
            printf("\t\t\t\tTabela de Simbolos sem as variáveis locais.\n");
            mostraTabela();
            mostraPilha();
        }
    ;

cabecalho
    : T_PROGRAMA T_IDENTIF
        { fprintf(yyout,"\tINPP\n"); }
    ;

variaveis
    :   /* vazio */
    | declaracao_variaveis
    ;

declaracao_variaveis
    : tipo lista_variaveis declaracao_variaveis
    | tipo lista_variaveis
    ;

tipo 
    : T_LOGICO
        { tipo = LOG; }
    | T_INTEIRO
        { tipo = INT; }
    ;

lista_variaveis
    : lista_variaveis T_IDENTIF 
        { 
          strcpy(elemTab.id, atoma);
          elemTab.end = contaVar;
          elemTab.tip = tipo;
          elemTab.cat = 'V';
          elemTab.rot = 0;
          elemTab.esc = escopo;
          insereSimbolo(elemTab);
          contaVar++; 
        }
    | T_IDENTIF
        { 
          strcpy(elemTab.id, atoma);
          elemTab.end = contaVar;
          elemTab.tip = tipo;
          elemTab.cat = 'V';
          elemTab.rot = 0;
          elemTab.esc = escopo;
          insereSimbolo(elemTab);
          contaVar++; 
        }
    ;

rotinas 
    :  // quando nao tem funcao
    |
        { fprintf(yyout, "\tDSVS\tL0\n"); }
    funcoes 
        { fprintf(yyout, "L0\tNADA\n"); }
    ;

funcoes 
    : funcao
    | funcao funcoes 
    ;
    
funcao 
    : T_FUNC tipo T_IDENTIF 
        {
            strcpy(elemTab.id, atoma);
            elemTab.tip = tipo;
            elemTab.cat = 'F';
            elemTab.esc = escopo;
            escopo = 'L';
            elemTab.rot = ++rotulo;
            insereSimbolo(elemTab); 
            fprintf(yyout,"L%d\tENSP\n", rotulo);
            //empilha(rotulo);
        }

    T_ABRE parametros T_FECHA 
        {
            ajusta_parametros(contaPar); //funcao para definir o endereco da funcao e dos parametros
            endFuncao = ajusta_parametros(contaPar); //endFuncao eh uma variavel que guarda o end da funcao
            arrumaVetorPar(contaPar); //funcao para arrumar o vetor de parametros, onde preenche de 0 ou 1, se o tipo for int ou log respectivamente
            //int posFunc = buscaSimbolo(atoma); // variavel posFunc onde armazena o indice da funcao serve para tratar possiveis erros.  
        } 

    variaveis 
        {
            elemTab.cat = 'V';
            //insereSimbolo(elemTab);
            ajusta_varLocal();
            varLocal = ajusta_varLocal();
            if (varLocal != 0) //se existe alguma variavel local, printa amem
                fprintf(yyout, "\tAMEM\t%d\n", varLocal);
            contaVarLocal++;
        }
    
    T_INICIO lista_comandos T_FIMFUNC 
    // 
    {
        printf("\t\t\t\tTabela de Simbolos com as variáveis locais.\n");
        mostraTabela();
        remover_variaveis_locais();
        //mostraTabela();
        escopo = 'G';
        contaPar = 0;
        endFuncao = -1;
    }
    ;

parametros 
    : /* vazio */
    | parametro parametros  
    ;

parametro 
    : tipo T_IDENTIF  
        {
            strcpy(elemTab.id, atoma);
            elemTab.tip = tipo;
            elemTab.esc = escopo;
            elemTab.cat = 'P';
            elemTab.rot = 0;
            insereSimbolo(elemTab);
            contaPar++;
        }  
    ;
    
lista_comandos
    :
    | comando lista_comandos
    ;

comando 
    : entrada_saida
    | repeticao 
    | selecao
    | atribuicao 
    | retorno 
    ;

retorno 
    : T_RETORNE expressao  
        {   
            if(endFuncao == -1)
                yyerror("Erro: retorne localizado fora da funcao!");
            varLocal = ajusta_varLocal();
            endFuncao = ajusta_parametros(contaPar);
            fprintf(yyout, "\tARZL\t%d\n", endFuncao); //ARZL com o end da funcao
            if(varLocal != 0) //se existe pelo menos uma variavel local, printa o dmem
                fprintf(yyout, "\tDMEM\t%d\n", varLocal);
            fprintf(yyout, "\tRTSP\t%d\n", contaPar); //RTSP com a quantidade de parametros
            desempilha('t');
        }
    ;

entrada_saida
    : leitura
    | escrita
    ;


leitura 
    : T_LEIA T_IDENTIF
        
        { 
            int pos = buscaSimbolo(atoma);
            fprintf(yyout,"\tLEIA\n\tARZG\t%d\n", tabSimb[pos].end); 
        }
    ;

escrita 
    : T_ESCREVA expressao 
        {
            desempilha('t');
            fprintf(yyout,"\tESCR\n"); 
        }
    ;

repeticao 
    : T_ENQTO
        { 
            fprintf(yyout,"L%d\tNADA\n", ++rotulo); 
            empilha(rotulo, 'r');
        } 
    expressao T_FACA  
        {   
            int tip = desempilha('t');
            if (tip != LOG)
                yyerror("Incompatibilidade de tipo");
            fprintf(yyout,"\tDSVF\tL%d\n", ++rotulo); 
            empilha(rotulo, 'r');
        }
    lista_comandos
    T_FIMENQTO
        {
            
            int rot1 = desempilha('r');
            int rot2 = desempilha('r');
            fprintf(yyout,"\tDSVS\tL%d\nL%d\tNADA\n", rot2, rot1); 

        }
    ;

selecao 
    : T_SE expressao T_ENTAO 
        { 
            int tip = desempilha('t');
            if (tip != LOG)
                yyerror("Incompatibilidade de tipo");
            fprintf(yyout,"\tDSVF\tL%d\n", ++rotulo);
            empilha(rotulo, 'r'); 
        }
    lista_comandos T_SENAO 
        {
            int rot = desempilha('r'); 
            fprintf(yyout,"\tDSVS\tL%d\nL%d\tNADA\n", ++rotulo, rot); 
            empilha(rotulo, 'r');
        }
    lista_comandos T_FIMSE
        {
            int rot = desempilha('r'); 
            fprintf(yyout,"L%d\tNADA\n", rot); 
        }
    ;

atribuicao 
    : T_IDENTIF
        {
            int pos = buscaSimbolo(atoma);
            empilha(pos, 'p');
        } 
      T_ATRIBUI expressao 
        { 
            //mostraPilha();
            int tip = desempilha('t');
            int pos = desempilha('p');
            if (tabSimb[pos].tip != tip)
                yyerror("Incompatibilidade de tipo!");
            if (tabSimb[pos].esc == 'G')
                fprintf(yyout,"\tARZG\t%d\n", tabSimb[pos].end); 
            else 
                fprintf(yyout,"\tARZL\t%d\n", tabSimb[pos].end);
        }

expressao 
    : expressao T_VEZES expressao 
        { 
            testaTipo(INT, INT, INT);
            fprintf(yyout,"\tMULT\n"); 
        
        }
    | expressao T_DIV expressao 
        { 
            testaTipo(INT, INT, INT);
            fprintf(yyout,"\tDIVI\n"); 
        }
    | expressao T_MAIS expressao
        { 
            testaTipo(INT, INT, INT);
            fprintf(yyout,"\tSOMA\n"); 
        } 
    | expressao T_MENOS expressao
        { 
            testaTipo(INT, INT, INT);
            fprintf(yyout,"\tSUBT\n"); 
        } 
    | expressao T_MAIOR expressao
        { 
            testaTipo(INT, INT, LOG);
            fprintf(yyout,"\tCMMA\n"); 
        } 
    | expressao T_MENOR expressao 
        { 
            testaTipo(INT, INT, LOG);
            fprintf(yyout,"\tCMME\n"); 
        }
    | expressao T_IGUAL expressao
        { 
            testaTipo(INT, INT, LOG);
            fprintf(yyout,"\tCMIG\n"); 
        } 
    | expressao T_E expressao 
        { 
            testaTipo(LOG, LOG, LOG);
            fprintf(yyout,"\tCONJ\n"); 
        }
    | expressao T_OU expressao
        { 
            testaTipo(LOG, LOG, LOG);
            fprintf(yyout,"\tDISJ\n"); 
        } 
    | termo 
    ;

identificador 
    : T_IDENTIF 
        { int pos = buscaSimbolo(atoma);
            empilha(pos, 'p');
        }
    ;


chamada 
    : 
      { int pos = desempilha('p');
        if (tabSimb[pos].esc == 'G')
            fprintf(yyout,"\tCRVG\t%d\n", tabSimb[pos].end);
        else
            fprintf(yyout,"\tCRVL\t%d\n", tabSimb[pos].end);
        empilha(tabSimb[pos].tip, 't');      }                 
    
    | T_ABRE 
         {  
            fprintf(yyout,"\tAMEM\t1\n");
        }
     
    lista_argumentos 
    
    T_FECHA 
        {
        int pos = desempilha('p');
        for(int j = 0; j < tabSimb[pos].npa; j++){ //j percorre ate a quantidade de parametros da funcao
            if (tabSimb[pos].par[j] != arg[j]) //compara o vetor de tipo de parametros com o vetor de tipos dos argumentos
                yyerror("Erro: tipo de argumento diferente do tipo do parametro.");
        }
         if (contaArgs != tabSimb[pos].npa)
            yyerror("Erro: quantidade de parametros diferente de argumentos");  
        fprintf(yyout, "\tSVCP\n");
        fprintf(yyout, "\tDSVS\tL%d\n", tabSimb[pos].rot);
        empilha(tabSimb[pos].tip, 't');
        contaArgs = 0; //reiniciar a contagem dos argumentos, quando fecha a chamada
        }
    ;

lista_argumentos 
    :   /* vazio */
    | expressao 
        {   
            arg[i] = desempilha('t');
            i++;
        }
    
    lista_argumentos
        {
            contaArgs++;
        }
    ;

termo 
    : identificador chamada
    /* : T_IDENTIF
        { 
            int pos = buscaSimbolo(atoma);
            fprintf(yyout,"\tCRVG\t%d\n", tabSimb[pos].end);
            empilha(tabSimb[pos].tip);
            empilha(tabSimb[pos].tip, 't');
        } */
    | T_NUMERO
        {   fprintf(yyout,"\tCRCT\t%s\n", atoma); 
            empilha(INT, 't');
        }
    | T_V 
        {   fprintf(yyout,"\tCRCT\t1\n"); 
            empilha(LOG, 't');
        }
    | T_F 
        {   fprintf(yyout,"\tCRCT\t0\n"); 
            empilha(LOG, 't');
        }
    | T_NAO termo
        { 
            int t = desempilha('t');
            if (t != LOG) yyerror ("Incompatibilidade de tipo!");
            fprintf(yyout,"\tNEGA\n"); 
            empilha(LOG, 't');
        }
    | T_ABRE expressao T_FECHA
    ;

%%


int main (int argc, char *argv[]) {
    char *p, nameIn[100], nameOut[100];
    argv++;
    if (argc < 2) {
        puts("\nCompilador Simples");
        puts("\n\tUso: ./simples <NOME>[.simples]\n\n");
        exit(10);
    }
    p = strstr(argv[0], ".simples");
    if (p) *p = 0;
    strcpy(nameIn, argv[0]);
    strcat(nameIn, ".simples");
    strcpy(nameOut, argv[0]);
    strcat(nameOut, ".mvs");
    yyin = fopen (nameIn, "rt");
    if (!yyin) {
        puts("Programa fonte não encontrado!");
        exit(20);
    }
    yyout = fopen(nameOut, "wt");
    yyparse();
    puts("Programa ok!");
}