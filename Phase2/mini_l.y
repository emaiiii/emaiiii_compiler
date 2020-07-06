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
%start Program
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

Program: 		{printf("Program -> epsilon\n");}
			| Function Program {printf("Program -> function Program\n");}
			;

Function:		;

Declaration:            Identifier COLON INTEGER
			| Identifier COLON ARRAY L_SQUARE_BRACKET R_SQUARE_BRACKET OF INTEGER
			;

Identifier: 		IDENT {printf("Identifier -> IDENT %s\n", yytext);}
			| IDENT COMMA Identifier {printf("Identifier -> IDENT COMMA Identifier")}
			;

%%

void yyerror(const char *msg){
        printf("Error: %s at symbol \"%s\" on line %d\n", msg, yytext, currLine);
}


