/*  
 *  MINI-L scanner
 *  Description: Lexical analyzer for MINI-L language
 *  Usage: (1) $ flex mini_l.lex
 *         (2) $ gcc -o mini_l.lex lex.yy.c -lfl
 *         (3) $ testing: cat test.min | ./mini_l
 *  Rick Lee, Email: rlee037@ucr.edu, SID: 861281330
 *  CS152 Compiler Design A01 Summer 2020
 *  Professor Rajiv Gupta
 */

%{
  #include <iostream>
  #include "y.tab.h"
  #define YY_DECL yy::parser::symbol_type yylex()
  //int currLine = 1;
  //int currPos  = 1;
  
  static yy::location loc;
%}

%option noyywrap

%{
#define YY_USER_ACTION loc.columns(yyleng);
%}

DIGIT [0-9]
LETTER [a-zA-Z]
ALPHANUMERIC [0-9a-zA-Z]
IDENTIFIER {LETTER}((({ALPHANUMERIC}|_)*{ALPHANUMERIC}+)|{ALPHANUMERIC}*)

%%

%{
loc.step();
%}

"function"    {return yy::parser::make_FUNCTION(loc);}
"beginparams" {return yy::parser::make_BEGIN_PARAMS(loc);}
"endparams"   {return yy::parser::make_END_PARAMS(loc);}
"beginlocals" {return yy::parser::make_BEGIN_LOCALS(loc);}
"endlocals"   {return yy::parser::make_END_LOCALS(loc);}
"beginbody"   {return yy::parser::make_BEGIN_BODY(loc);}
"endbody"     {return yy::parser::make_END_BODY(loc);}
"integer"     {return yy::parser::make_INTEGER(loc);}
"array"       {return yy::parser::make_ARRAY(loc);}
"of"          {return yy::parser::make_OF(loc);}
"if"          {return yy::parser::make_IF(loc);}
"then"        {return yy::parser::make_THEN(loc);}
"endif"       {return yy::parser::make_ENDIF(loc);}
"else"        {return yy::parser::make_ELSE(loc);}
"while"       {return yy::parser::make_WHILE(loc);}
"do"          {return yy::parser::make_DO(loc);}
"beginloop"   {return yy::parser::make_BEGINLOOP(loc);}
"endloop"     {return yy::parser::make_ENDLOOP(loc);}
"continue"    {return yy::parser::make_CONTINUE(loc);}
"read"        {return yy::parser::make_READ(loc);}
"write"       {return yy::parser::make_WRITE(loc);}
"and"         {return yy::parser::make_AND(loc);}
"or"          {return yy::parser::make_OR(loc);}
"not"         {return yy::parser::make_NOT(loc);}
"true"        {return yy::parser::make_TRUE(loc);}
"false"       {return yy::parser::make_FALSE(loc);}
"return"      {return yy::parser::make_RETURN(loc);}
"-"           {return yy::parser::make_SUB(loc);}
"+"           {return yy::parser::make_ADD(loc);}
"*"           {return yy::parser::make_MULT(loc);}
"/"           {return yy::parser::make_DIV(loc);}
"%"           {return yy::parser::make_MOD(loc);}
"=="          {return yy::parser::make_EQ(loc);}
"<>"          {return yy::parser::make_NEQ(loc);}
"<"           {return yy::parser::make_LT(loc);}
">"           {return yy::parser::make_GT(loc);}
"<="          {return yy::parser::make_LTE(loc);}
">="          {return yy::parser::make_GTE(loc);}
{IDENTIFIER}  {return yy::parser::make_IDENT(yytext, loc);}
{DIGIT}+      {return yy::parser::make_NUMBER(yytext, loc);}

";"           {return yy::parser::make_SEMICOLON(loc);}
":"           {return yy::parser::make_COLON(loc);}
","           {return yy::parser::make_COMMA(loc);}
"("           {return yy::parser::make_L_PAREN(loc);}
")"           {return yy::parser::make_R_PAREN(loc);}
"["           {return yy::parser::make_L_SQUARE_BRACKET(loc);}
"]"           {return yy::parser::make_R_SQUARE_BRACKET(loc);}
":="          {return yy::parser::make_ASSIGN(loc);}

[ \t]+        {loc.step();}
"\n"          {loc.step(); loc.lines();}
"##".*        {loc.step();}

({DIGIT}|_)+{IDENTIFIER} {/*printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", currLine, currPos, yytext); exit(0);*/}
{IDENTIFIER}_+           {/*printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", currLine, currPos, yytext); exit(0);*/}
.                        {/*printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", currLine, currPos, yytext); exit(0);*/}

<<EOF>> {return yy::parser::make_END(loc);}

%%
/*
int main(int argc, char ** argv)
{
  if (argc >= 2) {
    yyin = fopen(argv[1], "r");
    if (yyin == NULL) {
      yyin = stdin;
    } else {
      yyin = stdin;
    }
  }
  yylex();
}
*/
