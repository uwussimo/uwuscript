%code requires {

# define YYLTYPE_IS_DECLARED 1 /* alert the parser that we have our own definition */

}

%{
    #include "AstNode.h"
    #include "FunctionDeclaration.h"
    #include "ClassDeclaration.h"
    #include "Return.h"
    #include "FunctionDeclaration.h"
    #include "ClassDeclaration.h"
    #include "Conditional.h"
    #include "UnaryOperator.h"
    #include "BinaryOperator.h"
    #include "CompareOperator.h"
    #include "Assignment.h"
    #include "MethodCall.h"
    #include "Declaration.h"
    #include "WhileLoop.h"
    #include "Array.h"
    #include "Range.h"

    #include <stdio.h>
    #include <stack>
    uwuscript::Block *programBlock; /* the top level root node of our final AST */

    extern int yylex();
    int yyerror(char const * s );
    #define YYERROR_VERBOSE
    #define YYDEBUG 1

    extern std::stack<std::string> fileNames;

    # define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
          (Current).file_name = fileNames.top();            \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
          (Current).file_name = fileNames.top();            \
        }                                                               \
    while (0)

%}

/* Represents the many different ways we can access our data */
%union {
    uwuscript::Node *node;
    uwuscript::Block *block;
    uwuscript::Expression *expr;
    uwuscript::Statement *stmt;
    uwuscript::Identifier *ident;
    uwuscript::VariableDeclaration *var_decl;
    std::vector<uwuscript::VariableDeclaration*> *varvec;
    std::vector<uwuscript::Expression*> *exprvec;
    std::string *string;
    long long integer;
    double number;
    int boolean;
    int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TSTR
%token <integer> TINTEGER
%token <number> TDOUBLE
%token <boolean> TBOOL
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE
%token <token> TLTLT "<<"
%token <token> TRANGE
%token <token> TPLUS TMINUS TMUL TDIV
%token <token> TNOT TAND TOR
%token <token> TIF TELSE TWHILE
%token <token> TDEF TRETURN TVAR
%token <token> INDENT UNINDENT 

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (Identifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> literals expr boolean_expr binop_expr unaryop_expr array_expr array_access  range_expr
%type <varvec> func_decl_args
%type <exprvec> call_args array_elemets_expr 
%type <block> program stmts block
%type <stmt> stmt var_decl func_decl conditional return while class_decl array_add_element
%type <token> comparison 

/* Operator precedence for mathematical operators */
%left TPLUS TMINUS
%left TMUL TDIV
%left TAND TNOT

%start program
%debug 
%verbose 
%locations /* track locations: @n of component N; @$ of entire range */
/*
%define parse.lac full
%define lr.type ielr
*/

%%

program : %empty { programBlock = new uwuscript::Block(); }
        | stmts { programBlock = $1; }
        ;

stmts : stmt { $$ = new uwuscript::Block(); $$->statements.push_back($<stmt>1); }
      | stmts stmt { $1->statements.push_back($<stmt>2); }
      ;

stmt : var_decl
     | func_decl
     | class_decl
     | conditional 
     | return
     | while
     | array_add_element
     | expr { $$ = new uwuscript::ExpressionStatement($1); }
     ;

block : INDENT stmts UNINDENT { $$ = $2; }
      | INDENT UNINDENT { $$ = new uwuscript::Block(); }
      ;

conditional : TIF expr block TELSE block {$$ = new uwuscript::Conditional($2,$3,$5);}
            | TIF expr block {$$ = new uwuscript::Conditional($2,$3);}
            ; 

while : TWHILE expr block TELSE block {$$ = new uwuscript::WhileLoop($2,$3,$5);}
      | TWHILE expr block {$$ = new uwuscript::WhileLoop($2,$3);}
      ; 

var_decl : ident ident { $$ = new uwuscript::VariableDeclaration($1, $2, @$); }
         | ident ident '=' expr { $$ = new uwuscript::VariableDeclaration($1, $2, $4, @$); }
         | TVAR ident { $$ = new uwuscript::VariableDeclaration($2, @$); }
         | TVAR ident '=' expr { $$ = new uwuscript::VariableDeclaration($2, $4, @$); }
         ;

func_decl : TDEF ident '(' func_decl_args ')' ':' ident block { $$ = new uwuscript::FunctionDeclaration($7, $2, $4, $8, @$); }
          | TDEF ident '(' func_decl_args ')' block { $$ = new uwuscript::FunctionDeclaration($2, $4, $6, @$); }
          ;

func_decl_args : %empty  { $$ = new uwuscript::VariableList(); }
          | var_decl { $$ = new uwuscript::VariableList(); $$->push_back($<var_decl>1); }
          | func_decl_args ',' var_decl { $1->push_back($<var_decl>3); }
          ;

class_decl: TDEF ident block {$$ = new uwuscript::ClassDeclaration($2, $3); }
          ;

return : TRETURN { $$ = new uwuscript::Return(@$); }
       | TRETURN expr { $$ = new uwuscript::Return(@$, $2); }
       ;

expr : ident '=' expr { $$ = new uwuscript::Assignment($<ident>1, $3, @$); }
     | ident '(' call_args ')' { $$ = new uwuscript::MethodCall($1, $3, @$);  }
     | ident { $<ident>$ = $1; }
     | literals
     | boolean_expr 
     | binop_expr
     | unaryop_expr
     | '(' expr ')' { $$ = $2; }
     | range_expr
     | array_expr
     | array_access
     ;

ident : TIDENTIFIER { $$ = new uwuscript::Identifier(*$1, @1); delete $1; }
      | TIDENTIFIER '.' TIDENTIFIER { $$ = new uwuscript::Identifier(*$1,*$3, @$); delete $1; delete $3;}
      ;

literals : TINTEGER { $$ = new uwuscript::Integer($1); }
         | TDOUBLE { $$ = new uwuscript::Double($1); }
         | TSTR { $$ = new uwuscript::String(*$1); delete $1; }
         | TBOOL { $$ = new uwuscript::Boolean($1); }
         ;

/* have to write it explicit to have the right operator precedence */
binop_expr : expr TAND expr { $$ = new uwuscript::BinaryOp($1, $2, $3, @$); }
           | expr TOR expr { $$ = new uwuscript::BinaryOp($1, $2, $3, @$); }
           | expr TPLUS expr { $$ = new uwuscript::BinaryOp($1, $2, $3, @$); }
           | expr TMINUS expr { $$ = new uwuscript::BinaryOp($1, $2, $3, @$); }
           | expr TMUL expr { $$ = new uwuscript::BinaryOp($1, $2, $3, @$); }
           | expr TDIV expr { $$ = new uwuscript::BinaryOp($1, $2, $3, @$); }
           ;

unaryop_expr : TNOT expr { $$ = new uwuscript::UnaryOperator($1, $2); }
             ;

boolean_expr : expr comparison expr { $$ = new uwuscript::CompOperator($1, $2, $3); }
             ;

call_args : %empty  { $$ = new uwuscript::ExpressionList(); }
          | expr { $$ = new uwuscript::ExpressionList(); $$->push_back($1); }
          | call_args ',' expr  { $1->push_back($3); }
          ;
 
comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE
           ;
          
array_elemets_expr: %empty {$$ = new uwuscript::ExpressionList(); }
                 | expr {$$ = new uwuscript::ExpressionList(); $$->push_back($1);}
                 | array_elemets_expr ',' expr {$$->push_back($3); }
                 ; 
                 
array_expr : '[' array_elemets_expr ']' {$$ = new uwuscript::Array($2, @$);}
          ;
          
array_add_element: ident "<<" expr { $$ = new uwuscript::ArrayAddElement($1, $3, @$); }
                ;
                
array_access: ident '[' TINTEGER ']' { $$ = new uwuscript::ArrayAccess($1, $3, @$); }
           | array_access '[' TINTEGER ']' { $$ = new uwuscript::ArrayAccess($1, $3, @$); }
           ;

range_expr : '[' expr TRANGE expr ']' {$$ = new uwuscript::Range($2, $4, @$);}
           ;

%%
