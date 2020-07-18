/*
 *
 *
 */

//definitions

%{
#include<stdio.h>
#include<stdlib.h>
#define YY_NO_UNPUT
void yyerror(const char * msg);
extern int currLine;
extern int currPos;
extern char* yytext;
%}

%union{
  char* idval;
  int nval;
}

%error-verbose

%start program

%token FUNCTION
%token BEGIN_PARAMS END_PARAMS
%token BEGIN_LOCALS END_LOCALS
%token BEGIN_BODY END_BODY
%token INTEGER
%token ARRAY
%token OF
%token IF THEN ENDIF ELSE
%token WHILE DO BEGINLOOP ENDLOOP CONTINUE
%token READ WRITE
%token AND OR NOT
%token TRUE FALSE
%token RETURN
%token SUB ADD MULT DIV MOD EQ NEQ LT GT LTE GTE
%token <idval> IDENT
%token <nval>  NUMBER
%token SEMICOLON COLON COMMA
%token L_PAREN R_PAREN
%token L_SQUARE_BRACKET R_SQUARE_BRACKET
%token ASSIGN

%right ASSIGN
%left  OR
%left  AND 
%right NOT
%left  MOD
%left  DIV
%left  MULT
%right SUB
%left  L_SQUARE_BRACKET R_SQUARE_BRACKET
%left  L_PAREN R_PAREN

%%

program: functions     {printf("program -> functions\n");}
       | /* epsilon */ {printf("program -> epsilon");}
       ;

functions: function function {printf("functions -> function function\n");}
         | function          {printf("functions -> function\n");}

function: FUNCTION identifier SEMICOLON BEGIN_PARAMS declarations END_PARAMS
                                        BEGIN_LOCALS declarations END_LOCALS
                                        BEGIN_BODY   statements   END_BODY
          {printf("function -> FUNCTION identifier SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n");}
        | FUNCTION identifier error BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
        ;

declarations: declaration SEMICOLON declarations {printf("declarations -> declaration SEMICOLON declaration\n");}
            | /* epsilon */                      {printf("declarations -> epsilon\n");}
            ;

declaration: identifiers COLON INTEGER                                                   {printf("declaration -> identifiers COLON INTEGER\n");}
           | identifiers COLON ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET OF INTEGER\n");}
           ;

identifiers: identifier                                              {printf("identifiers -> identifier\n");}
           | identifier COMMA identifiers                            {printf("identifiers -> identifier COMMA identifiers\n");}
           | identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("identifiers -> identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
           | identifier error
           ;

number: NUMBER {printf("number -> NUMBER %s\n", yytext);}
      ;

statement: variable ASSIGN expression                              {printf("statement -> variable ASSIGN expression\n");}
         | IF bool_expression THEN statements else_statement ENDIF {printf("statement -> IF bool_expression THEN multi_statement else_statement ENDIF\n");}
         | WHILE bool_expression BEGINLOOP statements ENDLOOP      {printf("statement -> WHILE bool_expression BEGINLOOP multi_statement ENDLOOP\n");}
         | DO BEGINLOOP statements ENDLOOP WHILE bool_expression   {printf("statement -> DO BEGINLOOP multi_statement ENDLOOP WHILE bool_expression\n");}
         | READ variables                                          {printf("statement -> READ variables\n");}
         | WRITE variables                                         {printf("statement -> WRITE variables\n");}
         | CONTINUE                                                {printf("statement -> CONTINUE\n");}
         | RETURN expression                                       {printf("statement -> RETURN expression\n");}
         ;

statements: statement SEMICOLON statements {printf("statements -> statements SEMICOLON statements\n");}
          | /* epsilon */                  {printf("statements -> epsilon\n");}
          ;

else_statement: ELSE statements               {printf("else_statement -> ELSE statements\n");}
              | /* epsilon */                 {printf("else_statement -> epsilon\n");}
              ;

variables: variable COMMA variables {printf("variables -> variables COMMA variable\n");}
         | variable                 {printf("variables -> variable\n");}
         ;

bool_expression: relation_and_expression OR bool_expression {printf("bool_expression -> relation_and_expression OR bool_expression\n");}
               | relation_and_expression                    {printf("bool_expression -> relation_and_expression\n");}
               ;

relation_and_expression: not_relation_expression AND relation_and_expression {printf("relation_and_expression -> relation_expression AND relation_and_expression\n");}
                       | not_relation_expression                             {printf("relation_and_expression -> relation_expression\n");}
                       ;

not_relation_expression: not relation_expression {printf("not_relation_expression -> not relation_expression\n");}
                       ;

relation_expression: L_PAREN bool_expression R_PAREN {printf("relation_expression -> L_PAREN bool_expression R_PAREN\n");}
                   | TRUE                            {printf("relation_expression -> TRUE\n");}
                   | FALSE                           {printf("relation_expression -> FALSE\n");} 
                   | expression comp expression      {printf("relation_expression -> expression comp expression\n");}
                   ;

not: NOT           {printf("not -> NOT\n");} 
   | /* epsilon */ {printf("not -> epsilon\n");}
   ;

comp: EQ  {printf("comp -> EQ\n");}
    | NEQ {printf("comp -> NEQ\n");}
    | LT  {printf("comp -> LT\n");}
    | GT  {printf("comp -> GT\n");}
    | LTE {printf("comp -> LTE\n");}
    | GTE {printf("comp -> GTE\n");}
    ;

expression: expression ADD multiplicative_expression {printf("expression -> expression ADD multiplicative_expression\n");}
          | expression SUB multiplicative_expression {printf("expression -> expression SUB multiplicative_expression\n");}
          | multiplicative_expression                {printf("expression -> multiplicative_expression\n");}
          ;

multiplicative_expression: signed_term MULT multiplicative_expression {printf("multiplicative_expression -> signed_term MULT multiplicative_expression\n");}
                         | signed_term DIV  multiplicative_expression {printf("multiplicative_expression -> signed_term DIV multiplicative_expression\n");}
                         | signed_term MOD  multiplicative_expression {printf("multiplicative_expression -> signed_term MOD multiplicative_expression\n");}
                         | signed_term                                {printf("multiplicative_expression -> signed_term\n");}
                         ;

signed_term: sign term {printf("signed_term -> sign term\n");}
           ;

term: variable                               {printf("term -> variable\n");}
    | number                                 {printf("term -> number\n");}
    | L_PAREN expression R_PAREN             {printf("term -> L_PAREN expression R_PAREN\n");}
    | identifier L_PAREN expressions R_PAREN {printf("term -> identifier L_PAREN nested_expressions R_PAREN\n");}
    ;

expressions: expressions COMMA expression {printf("expressions -> expressions COMMA expression\n");}
           | expression                   {printf("expressions -> expression\n");} 
           ;

sign: SUB           {printf("sign -> SUB\n");}
    | /* epsilon */ {printf("sign -> epsilon\n");} 
    ;

variable: identifier                                              {printf("variable -> identifier\n");}
        | identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("variable -> identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
        ;

identifier: IDENT {printf("indentifier -> IDENT %s\n", yytext);}
          ;

%%

//code

void yyerror(const char* msg) {
  printf("Error: %s at symbol \"%s\" on line %d\n", msg, yytext, currLine);
}

int main(int argc, char **argv) {
  printf("\n\n");
  yyparse();
  printf("\n\n");
}
