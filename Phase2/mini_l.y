%{
	#include <stdio.h>
	#include <stdlib.h>
	#define YY_NO_UNPUT
	void yyerror(const char *msg);
	extern int currLine;
	extern int currPos;
	extern char *yytext;
%}

%union{
	char* idval;
	int nval;
}

%error-verbose
%start prog_start
%token FUNCTION
%token BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY
%token INTEGER ARRAY
%token OF IF THEN ENDIF ELSE 
%token WHILE DO BEGINLOOP ENDLOOP
%token CONTINUE
%token READ WRITE  
%token AND OR NOT 
%token TRUE FALSE
%token RETURN

%token SUB ADD MULT DIV MOD
%token EQ NEQ LT GT LTE GTE

%token SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN

%token <idval> IDENT
%token <nval> NUMBER

%%

prog_start:                						{printf("prog_start -> epsilon\n");}
                        						| function prog_start {printf("prog_start -> functions\n");}
                        						;

function:               						FUNCTION IDENT SEMICOLON BEGIN_PARAMS multi_declaration END_PARAMS BEGIN_LOCALS multi_declaration END_LOCALS BEGIN_BODY multi_statement END_BODY
										{printf("function -> FUNCTION IDENT SEMICOLON BEGIN_PARAMS multi_declaration END_PARAMS BEGIN_LOCALS multi_declaration END_LOCALS BEGIN_BODY multi_statement END_BODY\n");}
									FUNCTION IDENT error BEGIN_PARAMS multi_declaration END_PARAMS BEGIN_LOCALS multi_declaration END_LOCALS BEGIN_BODY multi_statement END_BODY
									;

declaration:            						identifiers COLON INTEGER {printf("declaration -> identifiers COLON INTEGER\n");}
									| identifiers error INTEGER
                        						| identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
                        					        | identifiers error ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER	
									;

statement:								var ASSIGN exp {printf("statement -> identifiers ASSIGN exp\n");}
									| var error exp
									| IF bool_exp THEN multi_statement multi_statement_else ENDIF {printf("statement -> IF bool_exp THEN multi_statement multi_statement_else ENDIF\n");}
									| WHILE bool_exp BEGINLOOP multi_statement ENDLOOP {printf("statement -> WHILE bool_exp BEGINLOOP multi_statement ENDLOOP\n");}
									| DO BEGINLOOP multi_statement ENDLOOP WHILE bool_exp {printf("statement -> DO BEGINLOOP multi_statement ENDLOOP WHILE bool_exp\n");}
									| READ multi_var {printf("statement -> READ multi_var\n");}
									| WRITE multi_var {printf("statement -> WRITE multi_var\n");}
									| CONTINUE {printf("statement -> CONTINUE\n");}
									| RETURN exp {printf("statement -> RETURN exp\n");}
									;

bool_exp: 								relation_and_exp {printf("bool_exp -> relation_and_exp\n");}
									| bool_exp OR relation_and_exp {printf("bool_exp -> bool_exp OR relation_and_exp\n");}
									;

relation_and_exp:							relation_exp {printf("relation_and_exp -> relation_exp\n");}
									| relation_and_exp AND relation_exp {printf("relation_and_exp -> relation_and_exp AND relation_exp\n");}
									;

relation_exp:                       					exp comp exp {printf("relation_exp -> exp comp exp\n");}
									| TRUE {printf("relation_exp -> TRUE\n");}
									| FALSE {printf("relation_exp -> FALSE\n");}
									| L_PAREN bool_exp R_PAREN {printf("relation_exp -> L_PAREN bool_exp R_PAREN\n");}
									| NOT exp comp exp {printf("relation_exp -> NOT exp comp exp\n");}
									| NOT TRUE {printf("relation_exp -> NOT TRUE\n");}
									| NOT FALSE {printf("relation_exp -> NOT FALSE\n");}
									| NOT L_PAREN bool_exp R_PAREN {printf("relation_exp -> NOT L_PAREN bool_exp R_PAREN\n");}
									;

comp:									EQ {printf("comp -> EQ\n");}
									| NEQ {printf("comp -> NEQ\n");}
									| LT {printf("comp -> LT\n");}
									| GT {printf("comp -> GT\n");}
									| LTE {printf("comp -> LTE\n");}
									| GTE {printf("comp -> GTE\n");}
									;

exp:									multiplicative_exp {printf("exp -> multiplicative_exp\n");}
									| exp ADD multiplicative_exp {printf("exp -> exp ADD multiplicative_exp\n");}
									| exp SUB multiplicative_exp {printf("exp -> exp SUB multiplicative_exp\n");}
									;

multiplicative_exp:							term {printf("multiplicative_exp -> term\n");}
									| multiplicative_exp MULT term {printf("multiplicative_exp -> multiplicative_exp MULT term\n");}
									| multiplicative_exp DIV term {printf("multiplicative_exp -> multiplicative_exp DIV term\n");}
									| multiplicative_exp MOD term {printf("multiplicative_exp -> multiplicative_exp MOD term\n");}
									;

term: 									var {printf("term -> var\n");}
									| NUMBER {printf("term -> NUMBER\n");}
									| L_PAREN exp R_PAREN {printf("term -> L_PAREN exp R_PAREN\n");}
									| SUB var {printf("term -> SUB var\n");}
									| SUB NUMBER {printf("term -> SUB NUMBER\n");}
									| SUB L_PAREN exp R_PAREN {printf("term -> SUB L_PAREN exp R_PAREN\n");}
									| IDENT L_PAREN multi_exp R_PAREN {printf("term -> IDENT L_PAREN multiplicative_exp R_PAREN\n");}
									;

multi_exp:								exp {printf("multi_exp -> exp\n");}
									| exp COMMA multi_exp {printf("multi_exp -> exp COMMA multi_exp\n");}
									;

multi_declaration:							{printf("multi_declaration -> epsilon \n");}
									| declaration SEMICOLON multi_declaration {printf("multi_declaration -> declaration SEMICOLON multi_declaration\n");}
									;

multi_statement: 							statement SEMICOLON {printf("multi_statement -> statement SEMICOLON\n");}
									| statement SEMICOLON multi_statement {printf("multi_statement -> statement SEMICOLON multi_statement\n");}
									;

multi_statement_else:							{printf("multi_statement_else -> epsilon\n");}
									| ELSE multi_statement {printf("multi_statement_else -> ELSE multi_statement_else\n");}
									;

multi_var:								var {printf("multi_var -> var \n");}
									|multi_var COMMA var {printf("multi_var -> multi_var COMMA var \n");}
									;


identifiers:             						IDENT {printf("identifiers -> IDENT %s\n", yytext);}
                        						| identifiers COMMA IDENT {printf("identifiers -> identifiers COMMA IDENT \n");}
                        						;

var:									IDENT {printf("var -> IDENT %s\n", yytext);}
									| IDENT L_SQUARE_BRACKET exp R_SQUARE_BRACKET {printf("var -> IDENT L_SQUARE_BRACKET exp R_SQUARE_BRACKET");}

%%

int main(int argc, char **argv) {
   	yyparse(); 
}

void yyerror(const char *msg){
        printf("Error: %s at symbol \"%s\" on line %d\n", msg, yytext, currLine);
}


