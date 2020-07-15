%{
	#include <stdio.h>
	#include <stdlib.h>
	#define YY_NO_UNPUT
	extern int currLine;
	extern int currPos;
	extern char* yytext;

	void yyerror(const char* msg);
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
%token WHILE DO FOR IN BEGINLOOP ENDLOOP
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

prog_start:         {printf("prog_start -> epsilon\n");}
                    | function prog_start {printf("prog_start -> function prog_start\n");}
                    ;

function:           FUNCTION identifier SEMICOLON BEGIN_PARAMS multi_decl END_PARAMS BEGIN_LOCALS multi_decl END_LOCALS BEGIN_BODY multi_stat END_BODY
                            {printf("function -> FUNCTION identifier SEMICOLON BEGIN_PARAMS multi_decl END_PARAMS BEGIN_LOCALS multi_decl END_LOCALS BEGIN_BODY multi_stat END_BODY\n");}
                    | FUNCTION identifier error BEGIN_PARAMS multi_decl END_PARAMS BEGIN_LOCALS multi_decl END_LOCALS BEGIN_BODY multi_stat END_BODY
                    ;

declaration:        identifiers COLON INTEGER {printf("declaration -> identifiers COLON INTEGER\n");}
                    | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
		              {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER;\n", $5);}
                    ;

multi_decl:         {printf("multi_decl -> epsilon\n");}
                    | declaration SEMICOLON multi_decl {printf("multi_decl -> declaration SEMICOLON multi_decl\n");}
                    ;

statement:          identifiers ASSIGN exp {printf("statement -> identifiers ASSIGN exp\n");}
                    | identifiers error exp
                    | IF bool_exp THEN multi_stat else ENDIF {printf("statement -> IF bool_exp THEN multi_stat else ENDIF\n");}
                    | WHILE bool_exp BEGINLOOP multi_stat ENDLOOP {printf("statement -> WHILE bool_exp BEGINLOOP multi_stat ENDLOOP\n");}
                    | DO BEGINLOOP multi_stat ENDLOOP WHILE bool_exp{printf("statement -> DO BEGINLOOP multi_stat ENDLOOP WHILE bool_exp\n");}
                    | FOR identifier IN identifier BEGINLOOP multi_stat ENDLOOP{printf("statement -> FOR identifier IN identifier BEGINLOOP multi_stat ENDLOOP\n");}
                    | READ identifiers {printf("statement -> READ identifiers\n");}
                    | WRITE identifiers {printf("statement -> WRITE identifiers\n");}
                    | CONTINUE {printf("statement -> CONTINUE\n");}
                    | RETURN exp {printf("statement -> RETURN exp\n");}
                    ;

multi_stat:         statement SEMICOLON multi_stat {printf("multi_stat -> statement SEMICOLON multi_stat\n");}
                    | statement SEMICOLON {printf("multi_stat -> statement SEMICOLON\n");}
                    ;

else:               {printf("else -> epsilon\n");}
                    | ELSE multi_stat {printf("else -> ELSE multi_stat\n");}
                    ;

exp:                multiplic_exp {printf("exp -> multiplic_exp\n");}
                    | multiplic_exp ADD exp{printf("exp -> multiplic_exp ADD exp\n");}
                    | multiplic_exp SUB exp {printf("exp -> multiplic_exp SUB exp\n");}
                    ;

multi_exp:          {printf("multi_exp -> epsilon\n");}
                    | exp COMMA multi_exp {printf("multi_exp -> exp COMMA multi_exp\n");}
                    | exp {printf("multi_exp -> exp\n");}
                    ;

multiplic_exp:      term{printf("multiplic_exp -> term\n");}
                    | term MULT multiplic_exp {printf("multiplic_exp -> term MULT multiplic_exp\n");}
                    | term DIV multiplic_exp {printf("multiplic_exp -> term DIV multiplic_exp\n");}
                    | term MOD multiplic_exp {printf("multiplic_exp -> term MOD multiplic_exp\n");}
                    ;

term:          	    identifiers {printf("term -> identifier\n");}
                    | SUB identifiers {printf("term -> SUB identifier\n");}
                    | NUMBER {printf("term -> NUMBER %d\n", $1);}
                    | SUB NUMBER {printf("term -> SUB NUMBER %d\n", $2);}
                    | SUB L_PAREN exp R_PAREN {printf("term -> SUB L_PAREN exp R_PAREN\n");}
                    | exp_loop {printf("term -> exp_loop\n");}
                    ;

exp_loop:   	    L_PAREN exp R_PAREN {printf("term -> L_PAREN exp R_PAREN\n");}|
                    | identifiers L_PAREN multi_exp R_PAREN {printf("term -> Ident L_PAREN multi_exp R_PAREN\n");}
                    ;

bool_exp:  	    rel_and_exp {printf("bool_exp -> rel_exp\n");}
                    | rel_and_exp OR bool_exp {printf("bool_exp -> rel_and_exp OR bool_exp\n");}
                    ;

rel_and_exp:        rel_exp {printf("rel_and_exp -> rel_exp\n");}
                    | rel_exp AND rel_and_exp {printf("rel_and_exp -> rel_exp AND rel_and_exp\n");}
                    ;

rel_exp:            NOT rel_exp_paths {printf("rel_exp -> NOT BoolValue\n");}
                    | rel_exp_paths {printf("rel_exp -> BoolValue\n");}
                    ;

rel_exp_paths:      exp comp exp {printf("rel_exp -> exp comp exp\n");}
                    | TRUE {printf("rel_exp -> TRUE\n");}
                    | FALSE {printf("rel_exp -> FALSE\n");}
                    | L_PAREN bool_exp R_PAREN {printf("rel_exp -> L_PAREN bool_exp R_PAREN\n");}
                    ;

comp:       	    EQ {printf("comp -> EQ\n");}
                    | NEQ {printf("comp -> NEQ\n");}
                    | LT {printf("comp -> LT\n");}
                    | GT {printf("comp -> GT\n");}
                    | LTE {printf("comp -> LTE\n");}
                    | GTE {printf("comp -> GTE\n");}
                    ;

identifiers:        identifier COMMA identifiers{printf("identifiers -> identifier COMMA identifiers\n");}
                    |identifier L_SQUARE_BRACKET exp R_SQUARE_BRACKET {printf("identifiers -> identifier  L_SQUARE_BRACKET exp R_SQUARE_BRACKET\n");}
                    |identifier error
                    |identifier {printf("identifiers -> identifier \n");}


identifier:        IDENT{printf("identifier -> IDENT %s \n", yytext);}

%%
int main(int argc, char **argv) {
    yyparse(); 
}

void yyerror(const char* msg) {
  printf("Error: %s at symbol \"%s\" on line %d\n", msg, yytext, currLine);
}
