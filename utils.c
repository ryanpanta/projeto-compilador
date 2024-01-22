/*+=============================================================
  |          UNIFAL - Universidade Federal de Alfenas.
  |            BACHARELADO EM CIENCIA DA COMPUTACAO.
  | Trabalho..: Funcao com retorno
  | Disciplina: Teoria de Linguagens e Compiladores - 2022/2
  | Professor.: Luiz Eduardo da Silva
  | Aluno.....: Ryan Rodrigues - RA: 2021.1.08.032
  | Data......: 17/02/2023
  +=============================================================*/

#include <ctype.h>
#include <string.h>

#define TAM_TAB 100
#define MAX_PAR 20
enum {
    INT, 
    LOG
};


struct elemTabSimbolos {
    char id[100]; //identificador
    int end;      //endereco (global) ou deslocamento (local) ---- mudar para dsl
    int tip;      // tipo
    char cat;     // categoria: 'f' = FUN, 'p' = PAR, 'v' = VAR       <---- elemento adicionado em aula do dia 31-01
    char esc;     // escopo: 'g' = GLOBAL, 'l' = LOCAL         <---- elemento adicionado em aula do dia 31-01
    int rot; 
    int conta[1];     // rotulo: (especifico para funcao) - > L0, L1, L2, laços      <---- elemento adicionado em aula do dia 31-01
    int npa;      // numero de parametros (para funcao)
    int par[MAX_PAR]; // tipos dos parametros (para funcao)
} tabSimb[TAM_TAB], elemTab;

int posTab = 0; 

void empilha (int valor, char tipo); 
int desempilha(char tipo); 

void maiscula (char *s) {
    for(int i = 0; s[i]; i++)
        s[i] = toupper(s[i]);
}

int buscaSimbolo(char *id)
{
    int i;
    //maiscula(id);
    for (i = posTab - 1; strcmp(tabSimb[i].id, id) && i >= 0; i--)
        ;
    if (i == -1) {
        char msg[200];
        sprintf(msg, "Identificador [%s] não encontrado!", id);
        yyerror(msg);
    }
    return i;
}
void insereSimbolo (struct elemTabSimbolos elem) {
    int i; 
    //maiscula(elem.id);
    if (posTab == TAM_TAB)
        yyerror("Tabela de Simbolos Cheia!");
    if((elem.cat == 'F' || elem.cat == 'V') && elem.esc == 'G' ) { //selecao feita para tratar variaveis "repetidas"
        for (i = posTab - 1; strcmp(tabSimb[i].id, elem.id) && i >= 0; i--)
            ;
    }
    if (i != -1) {
        char msg[200];
        sprintf(msg, "Identificador [%s] duplicado!", elem.id);
        yyerror(msg);
    }
    tabSimb[posTab++] = elem; 
}

int ajusta_parametros(int nPar) { //funcao feita para ajustar o endereco da funcao e dos parametros
    int i, aux, aux2;
    aux = 0;
    //int conta2[1];
    for(i = TAM_TAB; i >= 0; i--){
        if(nPar == 0 && tabSimb[i].cat == 'F') { //se a funcao nao tem parametros, end = -3
            tabSimb[i].end = -3;
            aux2 = tabSimb[i].end = -3;
            return aux2; 
        }
        else {
            //if (tabSimb[i].cat == 'F' && conta2[0] != 1) {se a funcao tem parametros, end = -3 - nPar
            if (tabSimb[i].cat == 'F') {
                tabSimb[i].npa = nPar;
                tabSimb[i].end = -3 - nPar;
                aux2 = tabSimb[i].end = -3 - nPar;
                //conta2[0] = 1;
                return aux2;
            }
                else {
                    //if (tabSimb[i].cat == 'P' && tabSimb[i].conta[0] != 1) { end de parametro eh -3 + aux
                    if (tabSimb[i].cat == 'P') {                    //aux comeca em 0, e vai decrementando 
                        tabSimb[i].npa = 0;
                        tabSimb[i].end = -3 - aux;
                        aux++;
                        //tabSimb[i].conta[0] = 1;
                    }
                }
                
        }     
        
    }
    //return aux2;
}

int ajusta_varLocal() { //funcao feita para ajustar os enderecos de variaveis locais da funcao, aux comeca em 0 e vai decrementando, essa funcao
    int i;                                  //tambem retorna a quantidade de variaveis locais dentro da funcao da posicao i
    int aux = 0;
    for(i = 0; i < posTab; i++)
        if (tabSimb[i].esc == 'L' && tabSimb[i].cat == 'V') {
            tabSimb[i].end = aux;
            aux++;
        }
    return aux;
}

void arrumaVetorPar(int nPar) { //funcao feita para preencher o vetor par[] com 0 e 1, onde 0 eh int e 1 log, obs: apenas parametros

    for(int i = 0; i <= TAM_TAB; i++){
        if(tabSimb[i].cat == 'F'){
            for(int j = 1; j <= nPar; j++)
                tabSimb[i].par[j-1] = tabSimb[i+j].tip;
        }
    }
}

char * renomeiaPar(struct elemTabSimbolos tab) { //funcao feita para guardar o tipo dos parametro, INT ou LOG em uma string (renomeia)
    char *renomeia = malloc(150);
    int i;
    for(i = 0; i < tab.npa; i++) {
        if(tab.par[i] == 0)
            strcat(renomeia, " INT ");
        else {
            if(tab.par[i] == 1)
                strcat(renomeia, " LOG ");
        } 
    }
    return renomeia; 
}

void remover_variaveis_locais() { //funcao que remove as variaveis onde o escopo eh = 'L', foi utilizado um algoritmo padrao de remocao
    int i, j;
    for(i = 0; i < posTab; i++) {
            if (tabSimb[i].esc == 'L') {
                for(j = i; j < posTab - 1; j++)
                    tabSimb[j] = tabSimb[j + 1]; //remocao
                posTab--;
                i--;
            }
    }
}

void mostraTabela () {
    int linha = 0;
    //puts("Tabela de Simbolos");
    puts("\t\t\t\t-------------------------------------------");

    printf("\n\t\t %5s %15s | %3s | %3s | %3s | %3s | %3s | %3s | %3s\n", "#", "ID", "ESC", "END", "ROT", "CAT", "TIP" ,"NPAR", "PAR");
    for(int i = 0; i < 100; i++) 
        printf(".");
    for(int i = 0; i < posTab; i++) {
        printf("\n\t\t%6d %15s | %3c | %3d | %3d | %3c | %3s | %4d | %3s", linha, tabSimb[i].id, tabSimb[i].esc, tabSimb[i].end, tabSimb[i].rot, tabSimb[i].cat, tabSimb[i].tip == INT? "INT" : "LOG", tabSimb[i].npa, renomeiaPar(tabSimb[i]));
        linha++;
        }
    printf("\n");
    for(int i = 0; i < 100; i++) 
        printf(".");
    printf("\n\n\n");
}


#define TAM_PIL 100
int topo = -1;

struct 
{ 
    int valor;
    char tipo;   //'r'= rotulo, 'n' = nvars, 't' = tipo, 'p' = posicao
    
} pilha[TAM_PIL]; 

void empilha(int valor, char tipo) {
    if (topo == TAM_PIL)
        yyerror("Pilha semântica cheia");
    pilha[++topo].valor = valor;
    pilha[topo].tipo = tipo;
}

int desempilha(char tipo) {
    if (topo == -1)
        yyerror("Pilha semântica vazia");
    if (pilha[topo].tipo != tipo)
    {
        char msg[100];
        sprintf(msg, "Desempilha esperado[%c], encontrado[%c]", tipo, pilha[topo].tipo);
        yyerror(msg);
    }
    return pilha[topo--].valor;
}

void mostraPilha()
{
    int i = topo;
    printf("Pilha = [");
    while (i >= 0)
    {
        printf("(%d, %c)", pilha[i].valor, pilha[i].tipo);
        i--;
    }
    printf("]\n");
}

void testaTipo(int tipo1, int tipo2, int ret) 
{
    int t1 = desempilha('t');
    int t2 = desempilha('t');
    if (t1 != tipo1 || t2 != tipo2)
        yyerror("Incompatibilidade de tipo");
    empilha(ret, 't');
}
