/* Ocamlyacc parser for C-net */ 

%{
    open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI SQUOTE DQUOTE
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token AND OR NOT
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE
%token INT FLOAT CHAR STRING VOID STRUCT 
%token NEW DELETE 
%token <int> INTLIT 
%token <string> ID FLOATLIT 
%token <string> STRLIT 
%token EOF

%start program

%type <Ast.program> program

%%

program: 
    decls { () }
    | EOF { () }

decls : 
      { () } (* empty *)
    | decls vdecl | decls fdecl | decls sdecl { () }

fdecl :
    typ ID LPAREN opt_params RPAREN LBRACE opt_stmts RBRACE { () }

opt_params : 
    { () }
    | params { () }

params: 
    typ ID { () }
    | params COMMA typ ID { () }

typ : CHAR | INT | FLOAT | STRING | SOCKET | STRUCT ID | typ LBRACKET RBRACKET {()}

opt_stmts: 
    {()}
    | stmts_gen { () }

stmts_gen:
    stmt { () }
    | stmts_gen vdecl { () }
    | stmts_gen vdecl_assign { () }
    | stmts_gen stmt { () }



expr: 
    INTLIT              { () }
  | FLOATLIT            { () }
  | STRLIT              { () }
  | ID                  { () }
  | LPAREN expr LPAREN  { () }
  | expr EQ expr        { () }
  | expr NEQ expr       { () }
  | expr LT expr        { () }
  | expr LEQ expr       { () }
  | expr GT expr        { () }
  | expr GEQ expr       { () }
  | expr PLUS expr      { () }
  | expr MINUS expr     { () }
  | expr TIMES expr     { () }
  | expr DIVIDE expr    { () }    // Revise 
  | ID ASSIGN expr      { () } 
    

typ:
     CHAR        { () }
  |  INT         { () }
  |  FLOAT       { () }
  |  STRING      { () } 
