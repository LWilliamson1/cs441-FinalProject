%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "calc3.h"
#include "symbol_table.h"

/* prototypes */
nodeType *opr(int oper, int nops, ...);
nodeType *id(char *name); /* changed JWJ */
nodeType *cid(char *name, vartypeEnum type); /* rgc: creating id with a type */
nodeType *con(int value);
nodeType *fpcon(double value);
void freeNode(nodeType *p);
dataType ex(nodeType *p);
int yylex(void);

void yyerror(char *s);
/* int sym[26]; JWJ:  removed to replace with real Symbol Table                  */
%}

%union {
    int iValue;                 /* integer value */
	double fValue;               /* floating point value */
    char *sName;                /* name of a variable - changed JWJ */
    nodeType *nPtr;             /* node pointer */
};

%token <iValue> INTEGER
%token <fValue> FLOAT
%token <sName> VARIABLE /* changed JWJ */
%token WHILE IF PRINT B_BEGIN B_END T_INT T_DOUBLE
%token DO FOR
%nonassoc IFX
%nonassoc ELSE

%left GE LE EQ NE '>' '<'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

%type <nPtr> stmt stmt_list expr typedecl_expr 

%%

program:
        function                { exit(0); }
        ;

function:
          function stmt         { ex($2); freeNode($2); }
        | /* NULL */
        ;

stmt:
          ';'                            { $$ = opr(';', 2, NULL, NULL); }
        | expr ';'                       { $$ = $1; }
        | typedecl_expr ';'              { $$ = $1; }
        | PRINT expr ';'                 { $$ = opr(PRINT, 1, $2); }
        | VARIABLE '=' expr ';'          { $$ = opr('=', 2, id($1), $3); } 
        | typedecl_expr '=' expr ';'     { $$ = opr('=', 2, $1, $3); } /* rgc: allow for inline declarations (i.e. int n=5) but only on LHS */
        | WHILE '(' expr ')' stmt        { $$ = opr(WHILE, 2, $3, $5); }
        | DO stmt WHILE '(' expr ')'     {  } /*JWJ: TODO implement*/
        | FOR '(' stmt stmt stmt ')'  stmt    { $$ = opr(FOR, 4, $3, $4, $5, $7); } /* JWJ: TODO: Implement for loops */
        | IF '(' expr ')' stmt %prec IFX { $$ = opr(IF, 2, $3, $5); }
        | IF '(' expr ')' stmt ELSE stmt { $$ = opr(IF, 3, $3, $5, $7); }
        | '{' stmt_list '}'              { $$ = $2; }
        | B_BEGIN stmt_list B_END        { $$ = $2; }
        ;

stmt_list:
          stmt                  { $$ = $1; }
        | stmt_list stmt        { $$ = opr(';', 2, $1, $2); }
        ;

expr:
          INTEGER               { $$ = con($1); }
        | FLOAT                 { $$ = fpcon($1); }
        | VARIABLE              { $$ = id($1); }
		| '(' T_INT ')' expr    { $$ = opr(T_INT, 1, $4); } /* rgc: typecasting */
		| '(' T_DOUBLE ')' expr { $$ = opr(T_DOUBLE, 1, $4); }
        | '-' expr %prec UMINUS { $$ = opr(UMINUS, 1, $2); }
        | expr '+' expr         { $$ = opr('+', 2, $1, $3); }
        | expr '-' expr         { $$ = opr('-', 2, $1, $3); }
        | expr '*' expr         { $$ = opr('*', 2, $1, $3); }
        | expr '/' expr         { $$ = opr('/', 2, $1, $3); }
        | expr '<' expr         { $$ = opr('<', 2, $1, $3); }
        | expr '>' expr         { $$ = opr('>', 2, $1, $3); }
        | expr GE expr          { $$ = opr(GE, 2, $1, $3); }
        | expr LE expr          { $$ = opr(LE, 2, $1, $3); }
        | expr NE expr          { $$ = opr(NE, 2, $1, $3); }
        | expr EQ expr          { $$ = opr(EQ, 2, $1, $3); }
        | '(' expr ')'          { $$ = $2; }
        ;
		
typedecl_expr:
		  T_INT VARIABLE        { $$ = cid($2, TYPE_INT); }
        | T_DOUBLE VARIABLE     { $$ = cid($2, TYPE_DOUBLE); }
        ;
		


%%

#define SIZEOF_NODETYPE ((char *)&p->con - (char *)p)

nodeType *con(int value) {
    nodeType *p;
    size_t nodeSize;

    /* allocate node */
    nodeSize = SIZEOF_NODETYPE + sizeof(conNodeType);
    if ((p = malloc(nodeSize)) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeCon;
	p->con.value.type = TYPE_INT;
    p->con.value._int = value;

    return p;
}

/* rgc: handle floating point constants */
nodeType *fpcon(double value) {
    nodeType *p;
    size_t nodeSize;

    /* allocate node */
    nodeSize = SIZEOF_NODETYPE + sizeof(conNodeType);
    if ((p = malloc(nodeSize)) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeCon;
	p->con.value.type = TYPE_DOUBLE;
    p->con.value._double = value;

    return p;
}

nodeType *id(char *name) {  /* changed JWJ */
    nodeType *p;
    size_t nodeSize;

    /* allocate node */
    nodeSize = SIZEOF_NODETYPE + sizeof(idNodeType);
    if ((p = malloc(nodeSize)) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeId;
	p->id.type = TYPE_UNDEF; /* rgc: we only give the node a type to help with initialization */
    p->id.s = name; /* changed JWJ */
	
    return p;
}

nodeType *cid(char *name, vartypeEnum type) {  /* changed JWJ */
    nodeType *p;
    size_t nodeSize;

    /* allocate node */
    nodeSize = SIZEOF_NODETYPE + sizeof(idNodeType);
    if ((p = malloc(nodeSize)) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeIdInit;
	p->id.type = type;
    p->id.s = name; /* changed JWJ */

    return p;
}

nodeType *opr(int oper, int nops, ...) {
    va_list ap;
    nodeType *p;
    size_t nodeSize;
    int i;

    /* allocate node */
    nodeSize = SIZEOF_NODETYPE + sizeof(oprNodeType) +
        (nops - 1) * sizeof(nodeType*);
    if ((p = malloc(nodeSize)) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeOpr;
    p->opr.oper = oper;
    p->opr.nops = nops;
    va_start(ap, nops);
    for (i = 0; i < nops; i++)
        p->opr.op[i] = va_arg(ap, nodeType*);
    va_end(ap);
    return p;
}

void freeNode(nodeType *p) {
    int i;

    if (!p) return;
    if (p->type == typeOpr) {
        for (i = 0; i < p->opr.nops; i++)
            freeNode(p->opr.op[i]);
    }
    free (p);
}

void yyerror(char *s) {
    fprintf(stdout, "%s\n", s);
}

int main(void) {
    pushSymbolTable();
    yyparse();
    return 0;
}
