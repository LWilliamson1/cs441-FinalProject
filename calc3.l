%{
#include <stdlib.h>
#include <string.h> /* changed JWJ */
#include "calc3.h"
#include "y.tab.h"
void yyerror(char *);
%}

I_LINE_COMMENT "//"[^\n]*|"\/\*"[^\n]*"\*\/"
N_LINE_COMMENT "\/\*"([^\n])*
END_COMMENT_LINE [^\n]*"\*\/"

%x LONG_COMMENT
%%

{I_LINE_COMMENT}	{;}
{N_LINE_COMMENT}	{ BEGIN(LONG_COMMENT); }
<LONG_COMMENT>{END_COMMENT_LINE}	{ BEGIN(INITIAL); }
<LONG_COMMENT>(^\*\/|^\n)* 	{;}

0           {
                yylval.iValue = atoi(yytext);
                return INTEGER;
            }

[1-9][0-9]* {
                yylval.iValue = atoi(yytext);
                return INTEGER;
            }
			
[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?	{
				yylval.fValue = atof(yytext);
				return FLOAT;
			}

[-()<>=+*/;{}.] {
                return *yytext;
             }

">="            return GE;
"<="            return LE;
"=="            return EQ;
"!="            return NE;
"begin"         return B_BEGIN;
"end"           return B_END;
"while"         return WHILE;
"do"            return DO;
"for"           return FOR; /* rgc: iterated loop support */
"if"            return IF;
"else"          return ELSE;
"print"         return PRINT;
"int"			return T_INT; /* rgc: reserved words for int and float/double types */
"double"		return T_DOUBLE;

[a-zA-Z_][a-zA-Z0-9_]* {  /* changed JWJ */
                yylval.sName = strdup(yytext); /* changed JWJ */
                return VARIABLE;
            }

[ \t]+        ;       /* ignore whitespace */
<INITIAL,LONG_COMMENT>\n			yylineno++; /* rgc: increment line number on newline (regardless of condition) */

.               yyerror("Unknown character");
%%
int yywrap(void) {
    return 1;
}
