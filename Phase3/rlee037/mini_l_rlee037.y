/*
 *
 *
 */

%{
//#include<stdio.h>
//#include<stdlib.h>
//#define YY_NO_UNPUT
//void yyerror(const char * msg);
//extern int currLine;
//extern int currPos;
//extern char* yytext;
%}

%skeleton "lalr1.cc"
%require "3.0.4"
%defines
%define api.token.constructor
%define api.value.type variant
%define parse.error verbose
%locations

%code requires
{
#include <list>
#include <string>
#include <functional>
using namespace std;
  /* define structures used as types for non-terminals */
  void appendIDs(list<string>* appendTo, const list<string> copyFrom);
  struct code_struct {
    string code;
    list<string> ids;
  };
  struct exp_struct {
    string code;
    list<string> ids;
    string dst;
    string src1;
    string src2;
    string op;
    exp_struct() {code = ""; dst = "", src1 = "", src2 = "", op = "";}
    exp_struct(string c, const list<string> idList, string d, string s1, string s2, string o) {
      code = c;
      appendIDs(&ids, idList);
      dst  = d;
      src1 = s1;
      src2 = s2;
      op   = o;
    }
    void setexp(string d, string s1, string s2, string o) {
      dst  = d;
      src1 = s1;
      src2 = s2;
      op   = o;
      ids.push_back(s1);
      ids.push_back(s2);
      ids.push_back(d);
    }
  };
  struct var {
    string id;
    bool   isArray;
    string index;
    string code;
    var() {id = ""; isArray = false; index = "", code = "";}
    var(string varID, bool isArr, string i, string exp) {id = varID, isArray = isArr, index = i; code = exp;}
  };
  struct var_struct {
    list<var> vars;
  };
  struct ids_struct {
    list<string> ids;
    list<bool>   isArray;
  };
  /* end structures for non-terminal types */
}

%code
{
#include "y.tab.h"
#include <sstream>
#include <regex>
#include <set>
yy::parser::symbol_type yylex();
  /* define symbol table, global variables, list of keywords or functions needed */
  bool no_error = true;
  int numTemps = 0;
  string newTemp() {string temp = "__temp__" + to_string(numTemps); ++numTemps; return temp;}
  int numLabels = 0;
  string newLabel() {string label = "__label__" + to_string(numLabels); ++numLabels; return label;} 
  list<string> symbol_table;
  void appendIDs(list<string>* appendTo, const list<string> copyFrom) {
    appendTo->insert(appendTo->end(), copyFrom.begin(), copyFrom.end());
  }
  string getLastLine(const string code) {
    string temp;
    if (code.size() < 3 && code.size() > 0) {
      temp = code.at(0);
    } else {
      for (int i = code.size() - 2; i == 0 | code.at(i) == '\n'; --i) {
       temp.insert(0, 1, code.at(i)); 
      }
    }
    return temp;
  }
  string setIndex(const var v) {
    string exp;
    //
    return exp;
  }
  /* end code */
}

//%union{
//  char* idval;
//  int nval;
//}

//%error-verbose

%token END 0 "end of file";

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
%token IDENT
%token NUMBER
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

%type<string>      IDENT NUMBER
%type<string>      program function comp sign number identifier not
%type<code_struct> declarations declaration statements statement else_statement signed_term term
%type<exp_struct>  bool_expression relation_and_expression not_relation_expression relation_expression expressions expression multiplicative_expression
%type<ids_struct>  identifiers
%type<var_struct>  variables variable

%start prog_start;

%%

prog_start: program {if (no_error) cout << $1 << endl;}
          ;

program: function program {$$ = $1 + "\n" + $2;}
       | /* epsilon */    {$$ = "";}
       ;

function: FUNCTION identifier SEMICOLON BEGIN_PARAMS declarations END_PARAMS
                                        BEGIN_LOCALS declarations END_LOCALS
                                        BEGIN_BODY   statements   END_BODY
          {
            $$ =  "func " + $2 + "\n";
            //begin params
            $$ += $5.code;
            int i = 0;
            for (list<string>::iterator it = $5.ids.begin(); it != $5.ids.end(); ++it) {
              $$ += "= " + *it + ", $" + to_string(i) + "\n";
              ++i;
            }
            //end params
            //begin locals
            $$ += $8.code;
            //end locals
            //begin body
            $$ += $11.code;
            //end body
            $$ += "endfunc\n";
          }
        ;

declarations: declaration SEMICOLON declarations
              {
                $$.code = $1.code + $3.code;
                $$.ids  = $1.ids;
                appendIDs(&$$.ids, $3.ids);
              }
            | /* epsilon */ {$$.code = ""; $$.ids = list<string>();}
            ;

declaration: identifiers COLON INTEGER
             {
               for (list<string>::iterator it = $1.ids.begin(); it != $1.ids.end(); ++it) {
                 $$.code += ". " + *it + "\n";
                 $$.ids.push_back(*it);
               }
             }
           | identifiers COLON ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET OF INTEGER
             {
               for (list<string>::iterator it = $1.ids.begin(); it != $1.ids.end(); ++it) {
                 $$.code += ".[] " + *it + ", " + $5 + "\n";
                 $$.ids.push_back(*it);
               }
             }
           ;

identifiers: identifier
             {$$.ids.push_back($1);}
           | identifier COMMA identifiers
             {
               $$.ids.push_back($1);
               appendIDs(&$$.ids, $3.ids);
             }
           | identifier error {no_error = false;}
           ;

number: NUMBER {$$ = $1;}
      ;

statement: variable ASSIGN expression
           {
             $$.code = $3.code;
//cout << $$.code << endl;
             string src = $3.dst;
             if ($1.vars.begin()->isArray) {
               string index = $1.vars.begin()->index;
               $$.code += "[]= " + $1.vars.begin()->id + ", " + src + ", " + index;
             } else {
               $$.code += "= " + $1.vars.begin()->id + ", " + src;
             }
             appendIDs(&$$.ids, $3.ids);
             $$.code += "\n";
           }
         | IF bool_expression THEN statements else_statement ENDIF {printf("statement -> IF bool_expression THEN multi_statement else_statement ENDIF\n");}
         | WHILE bool_expression BEGINLOOP statements ENDLOOP      {printf("statement -> WHILE bool_expression BEGINLOOP multi_statement ENDLOOP\n");}
         | DO BEGINLOOP statements ENDLOOP WHILE bool_expression   {printf("statement -> DO BEGINLOOP multi_statement ENDLOOP WHILE bool_expression\n");}
         | READ variables
           {
             for (list<var>::iterator it = $2.vars.begin(); it != $2.vars.end(); ++it) {
               if (it->isArray) {
                 $$.code += ".[]< " + it->id + ", " + it->index + "\n";
               } else {
                 $$.code += ".< " + it->id + "\n";
               }
               $$.ids.push_back(it->id);
             }
           }
         | WRITE variables
           {
             for (list<var>::iterator it = $2.vars.begin(); it != $2.vars.end(); ++it) {
               if (it->isArray) {
                 $$.code += ".[]> " + it->id + ", " + it->index + "\n";
               } else {
                 $$.code += ".> " + it->id + "\n";
               }
               $$.ids.push_back(it->id);
             }
           }
         | CONTINUE                                                {printf("statement -> CONTINUE\n");}
         | RETURN expression                                       {printf("statement -> RETURN expression\n");}
         ;

statements: statement SEMICOLON statements
            {
              $$.code = $1.code + $3.code;
              appendIDs(&$$.ids, $1.ids);
              appendIDs(&$$.ids, $3.ids);
            }
          | /* epsilon */ {$$.code = ""; $$.ids = list<string>();}
          ;

else_statement: ELSE statements               {printf("else_statement -> ELSE statements\n");}
              | /* epsilon */                 {$$.code = ""; $$.ids = list<string>();}
              ;

variables: variable COMMA variables
           {
             $$.vars.insert($$.vars.end(), $1.vars.begin(), $1.vars.end());
             $$.vars.insert($$.vars.end(), $3.vars.begin(), $3.vars.end());
           }
         | variable {$$.vars.insert($$.vars.end(), $1.vars.begin(), $1.vars.end());}
         ;

bool_expression: relation_and_expression OR bool_expression {printf("bool_expression -> relation_and_expression OR bool_expression\n");}
               | relation_and_expression                    {printf("bool_expression -> relation_and_expression\n");}
               ;

relation_and_expression: not_relation_expression AND relation_and_expression {printf("relation_and_expression -> relation_expression AND relation_and_expression\n");}
                       | not_relation_expression                             {printf("relation_and_expression -> relation_expression\n");}
                       ;

not_relation_expression: not relation_expression{printf("not_relation_expression -> not relation_expression\n");}
                       ;

relation_expression: L_PAREN bool_expression R_PAREN {printf("relation_expression -> L_PAREN bool_expression R_PAREN\n");}
                   | TRUE                            {printf("relation_expression -> TRUE\n");}
                   | FALSE                           {printf("relation_expression -> FALSE\n");} 
                   | expression comp expression      {printf("relation_expression -> expression comp expression\n");}
                   ;

not: NOT           {$$ = "!";} 
   | /* epsilon */ {$$ = "";}
   ;

comp: EQ  {$$ = "==";}
    | NEQ {$$ = "!=";}
    | LT  {$$ = "<";}
    | GT  {$$ = ">";}
    | LTE {$$ = "<=";}
    | GTE {$$ = ">=";}
    ;

expression: expression ADD multiplicative_expression
            {
              string src1 = $1.dst;
              string src2 = $3.dst;
              string dst  = newTemp();
              $$.setexp(dst, src1, src2, "+");
              $$.code =  $1.code;
              $$.code += $3.code;
              $$.code += ". " + dst + "\n";
              string exp = $$.op + " " + $$.dst + ", " + $$.src1 + ", " + $$.src2;
              $$.code += exp;//cout << exp << endl;
            }
          | expression SUB multiplicative_expression {printf("expression -> expression SUB multiplicative_expression\n");}
          | multiplicative_expression {$$.code = $1.code; appendIDs(&$$.ids, $1.ids);}
          ;

multiplicative_expression: signed_term MULT multiplicative_expression
                           {
                             string term1  = newTemp();
                             string term2  = newTemp();
                             string result = newTemp();
                             $$.code += "* " + result + ", " + term1 + ", " + term2;
                             appendIDs(&$$.ids, $3.ids);
                           }
                         | signed_term DIV  multiplicative_expression
                           {
                             //
                           }
                         | signed_term MOD  multiplicative_expression {printf("multiplicative_expression -> signed_term MOD multiplicative_expression\n");}
                         | signed_term                                {$$.code = $1.code; appendIDs(&$$.ids, $1.ids);}
                         ;

signed_term: sign term {$$.code = $1 + $2.code; appendIDs(&$$.ids, $2.ids);}
           ;

term: variable
      {
        list<var>::iterator it = $1.vars.begin(); 
        $$.code += it->id;
        $$.ids.push_back(it->id);
      }
    | number {$$.code = $1 + "\n";}
    | L_PAREN expression R_PAREN             {$$.code = $2.code; appendIDs(&$$.ids, $2.ids);}
    | identifier L_PAREN expressions R_PAREN {printf("term -> identifier L_PAREN nested_expressions R_PAREN\n");}
    ;

expressions: expressions COMMA expression {$$.code = $1.code + $3.code; appendIDs(&$$.ids, $1.ids); appendIDs(&$$.ids, $3.ids);}
           | expression                   {$$.code = $1.code; appendIDs(&$$.ids, $1.ids);} 
           ;

sign: SUB           {$$ = "-";}
    | /* epsilon */ {$$ = "";} 
    ;

variable: identifier
          {
            var temp = var($1, false, "", "");
            $$.vars.push_back(temp);
          }
        | identifier L_SQUARE_BRACKET expression R_SQUARE_BRACKET
          {
            //$3.code
            string index = $3.dst;
            var temp = var($1, true, index, $3.code);
            $$.vars.push_back(temp);
          }
        ;

identifier: IDENT {$$ = $1;}
          ;

%%

//code

void yy::parser::error(const yy::location& l, const std::string& m) {
  std::cerr << l  << ": "  << m << std::endl;
}

int main(int argc, char **argv) {
  yy::parser p;
  return p.parse();
}
