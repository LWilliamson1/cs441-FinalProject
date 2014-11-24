#include <stdio.h>
#include <stdlib.h>
#include "symbol_table.h"
#include "calc3.h"
#include "y.tab.h"

extern int yylineno; /* added JWJ */

dataType ex(nodeType *p) {
	dataType rvTrue; //rgc: 'true' return variable
	rvTrue._int=1;
	rvTrue.type = TYPE_INT;
	
	dataType rvFalse; //rgc: 'false' return variable
	rvFalse._int=0;
	rvFalse.type = TYPE_INT;
	
    if (!p) return rvFalse;
	
    switch(p->type) {
		case typeCon:	
			return p->con.value;
		case typeId: {
			/* changed JWJ */
			struct symbol_entry *entry = getSymbolEntry(p->id.s);
			if (!entry) {
				fprintf(stderr, "Undeclared variable %s at line %d\n", p->id.s,yylineno);
				exit(1);
			}
			return entry->value;
		}
		case typeIdInit: {
			struct symbol_entry *entry = getSymbolEntry(p->id.s);
			if (!entry) {
				entry = calloc(1, sizeof(struct symbol_entry));
				entry->name = p->id.s;
				entry->value.type = p->id.type; // rgc: new LHS var inherits type from idnode type
				addSymbol(entry, yylineno);
				free(entry);

				/* need to look it up again since addSymbol makes a copy */
				entry = getSymbolEntry(p->id.s);
			}
			return rvTrue; //rgc: just initializing here, nothing to see, move along
		}
		case typeOpr: {
			switch(p->opr.oper) {
				case B_BEGIN: {
					pushSymbolTable();
					ex(p->opr.op[0]);
					popSymbolTable();
					return rvTrue;
				}
				case WHILE:	
					while(ex(p->opr.op[0])._int) ex(p->opr.op[1]); return rvTrue;
				case DO:
					do { ex(p->opr.op[0]); } while (ex(p->opr.op[1])._int); return rvTrue;
				case IF:	
					if (ex(p->opr.op[0])._int)
						ex(p->opr.op[1]);
					else if (p->opr.nops > 2)
						ex(p->opr.op[2]);
					return rvTrue;
				case FOR:
					for( ex(p->opr.op[0]); ex(p->opr.op[1])._int; ex(p->opr.op[2])) { ex(p->opr.op[3]); }
				case PRINT:	{
					dataType pv = ex(p->opr.op[0]);
					if (pv.type == TYPE_INT) {
						printf("%d\n", pv._int); 
						return rvTrue;
					} else if (pv.type == TYPE_DOUBLE) {
						printf("%f\n", pv._double);
						return rvTrue;
					} else {
						fprintf(stderr, "Error at line %d: printing unrecognized type\n", yylineno);
						exit(1);
					}
				}
				case ';':	
					ex(p->opr.op[0]); 
					return ex(p->opr.op[1]);
				case '=':
					/* changed JWJ */
					{
						/* rgc: check to make sure LHS is an identifier and not a constant */
						if ((p->opr.op[0]->type != typeId) && (p->opr.op[0]->type != typeIdInit)) {
							fprintf(stderr, "Syntax error at line %d: cannot perform assignment\n", yylineno);
							exit(1);
						}
						
						// rgc: create new var if inline declaration (int num=5)
						if (p->opr.op[0]->type == typeIdInit) ex(p->opr.op[0]); 
						
						const char *name = p->opr.op[0]->id.s;
						/* look up an existing variable */
						struct symbol_entry *entry = getSymbolEntry(name);
						
						/* doesn't exist: add a new variable to the symbol table */
						/* rgc: actually, we should throw an error. now we need to define vars before we use them */
						if (!entry) {
							fprintf(stderr, "Undeclared variable %s at line %d\n", name, yylineno);
							exit(1);
						}
						
						// calculate RHS
						dataType rhs = ex(p->opr.op[1]);
						
						// rgc: determine need for type casting
						if (entry->value.type == rhs.type) {
							// same type
							entry->value = rhs;
						} else if (entry->value.type == TYPE_INT) {
							// LHS int, RHS double
							entry->value._int = (int) rhs._double;
						} else if (entry->value.type == TYPE_DOUBLE) {
							// LHS double, RHS int
							entry->value._double = (double) rhs._int;
						}
						return entry->value;
					}
				case UMINUS: {
					dataType value = ex(p->opr.op[0]);
					if (value.type == TYPE_INT) {
						value._int *= -1;
					} else if (value.type == TYPE_DOUBLE) {
						value._double *= -1;
					}
					return value;
				}
				
				case T_INT: {
					dataType value = ex(p->opr.op[0]);
					if (value.type == TYPE_DOUBLE) {
						int tmp = (int) value._double;
						value.type = TYPE_INT;
						value._int = tmp;
					}
					return value;
				}
				
				case T_DOUBLE: {
					dataType value = ex(p->opr.op[0]);
					if (value.type == TYPE_INT) {
						double tmp = (double) value._int;
						value.type = TYPE_DOUBLE;
						value._double = tmp;
					}
					return value;
				}
				
				// rgc: in the case of all binary arithmetic operations, if both operands are int, result will be int. 
				// otherwise, intermediate calculations should be carried out with the full precision of a double.
				case '+':	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					dataType rv;
					if ((value0.type == TYPE_INT) && (value1.type == TYPE_INT)) {
						rv.type = TYPE_INT;
						rv._int = value0._int + value1._int;
					} else {
						rv.type = TYPE_DOUBLE;
						rv._double = (value0.type==TYPE_INT ? (double)value0._int : value0._double) + (value1.type==TYPE_INT ? (double)value1._int : value1._double);
					}
					return rv;
				}
				case '-':	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					dataType rv;
					if ((value0.type == TYPE_INT) && (value1.type == TYPE_INT)) {
						rv.type = TYPE_INT;
						rv._int = value0._int - value1._int;
					} else {
						rv.type = TYPE_DOUBLE;
						rv._double = (value0.type==TYPE_INT ? (double)value0._int : value0._double) - (value1.type==TYPE_INT ? (double)value1._int : value1._double);
					}
					return rv;
				}
				case '*':	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					dataType rv;
					if ((value0.type == TYPE_INT) && (value1.type == TYPE_INT)) {
						rv.type = TYPE_INT;
						rv._int = value0._int * value1._int;
					} else {
						rv.type = TYPE_DOUBLE;
						rv._double = (value0.type==TYPE_INT ? (double)value0._int : value0._double) * (value1.type==TYPE_INT ? (double)value1._int : value1._double);
					}
					return rv;
				}
				case '/':	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					dataType rv;
					if ((value0.type == TYPE_INT) && (value1.type == TYPE_INT)) {
						rv.type = TYPE_INT;
						rv._int = value0._int / value1._int;
					} else {
						rv.type = TYPE_DOUBLE;
						rv._double = (value0.type==TYPE_INT ? (double)value0._int : value0._double) / (value1.type==TYPE_INT ? (double)value1._int : value1._double);
					}
					return rv;
				}
				
				// rgc: for logic operations, we return an int 1 if true and int 0 if false. (rvTrue and rvFalse)
				case '<':	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					if ((value0.type==TYPE_INT ? value0._int : value0._double) < (value1.type==TYPE_INT ? value1._int : value1._double)) return rvTrue;
					// if we get here it's not true.
					return rvFalse;
				}
				case '>':	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					if ((value0.type==TYPE_INT ? value0._int : value0._double) > (value1.type==TYPE_INT ? value1._int : value1._double)) return rvTrue;
					// if we get here it's not true.
					return rvFalse;
				}
				case GE:	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					if ((value0.type==TYPE_INT ? value0._int : value0._double) >= (value1.type==TYPE_INT ? value1._int : value1._double)) return rvTrue;
					// if we get here it's not true.
					return rvFalse;
				}
				case LE:	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					if ((value0.type==TYPE_INT ? value0._int : value0._double) <= (value1.type==TYPE_INT ? value1._int : value1._double)) return rvTrue;
					// if we get here it's not true.
					return rvFalse;
				}
				case NE:	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					if ((value0.type==TYPE_INT ? value0._int : value0._double) != (value1.type==TYPE_INT ? value1._int : value1._double)) return rvTrue;
					// if we get here it's not true.
					return rvFalse;
				}
				case EQ:	{
					dataType value0 = ex(p->opr.op[0]);
					dataType value1 = ex(p->opr.op[1]);
					if ((value0.type==TYPE_INT ? value0._int : value0._double) == (value1.type==TYPE_INT ? value1._int : value1._double)) return rvTrue;
					// if we get here it's not true.
					return rvFalse;
				}
			}
		}
	}
    return rvFalse;
}
