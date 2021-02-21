/* Ocamlyacc parser for C-net */ 

%{
    open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI SQUOTE DQUOTE
%left PLUS MINUS 
%left TIMES DIVIDE 
%token MOD ASSIGN
%token PLUSEQ MINUSEQ 
%token EQ NEQ LT LEQ GT GEQ
%token AND OR NOT
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE
%token NOELSE
%token INT FLOAT CHAR STRING VOID STRUCT SOCKET
%token TCP UDP
%token NEW DELETE 
%token STRUCTMEM
%token <int> INTLIT CHARLIT
%token <string> ID FLOATLIT 
%token <string> STRLIT 
%token <string> ID  
%token EOF

%start program

%type <Ast.program> program

%%

program: 
    decls { () }
    | EOF { () }

decls : 
      { () } /* empty */
    | decls vdecl { () }
    | decls fdecl { () }
    | decls sdecl { () }

fdecl :
    typ ID LPAREN opt_params RPAREN LBRACE opt_stmts RBRACE { () }

opt_params : 
    { () }
    | params { () }

params: 
    typ ID { () }
    | params COMMA typ ID { () }

typ : 
    CHAR  { () }
    | INT  { () }
    | FLOAT  { () }
    | STRING  { () }
    | SOCKET  { () }
    | STRUCT ID  { () }
    | typ LBRACKET RBRACKET { () }

opt_stmts: 
    {()}
    | stmts_gen { () }

stmt_gen :
    stmt { () }
    | LBRACE stmts_gen RBRACE { () }

stmts_gen:
    stmt { () }
    | stmts_gen vdecl { () } /* TODO this might have to become vdecl stmts_gen OR make vdecl and vdecl_assign terminals */
    | stmts_gen vdecl_assign { () }
    | stmts_gen stmt { () }

vdecls:
    vdecls vdecl { () }
    | vdecl { () }

vdecl:
    typ ID SEMI { () }

vdecl_assign:
    typ ID ASSIGN expr SEMI { () }
    | typ ID ASSIGN NEW typ LBRACKET INTLIT RBRACKET LBRACE args RBRACE SEMI { () }

sdecl:
    STRUCT ID LBRACE vdecls RBRACE SEMI { () }

stmts:
    stmts stmt { () }
    | stmt { () }

stmt: 
    opt_expr SEMI { () }
    | RETURN opt_expr SEMI { () }
    | LBRACE stmts RBRACE { () }
    | IF LPAREN expr RPAREN stmt %prec NOELSE { () }
    | IF LPAREN expr RPAREN stmt ELSE stmt    { () }
    | FOR LPAREN opt_expr SEMI expr SEMI opt_expr RPAREN stmt_gen { () }
    | WHILE RPAREN expr LPAREN stmt_gen  { () }

opt_expr: 
    { () }
    | expr { () }

arraylit: 
    LBRACE args RBRACE { () }

expr: 
    INTLIT                { () }
    | CHARLIT             { () }
    | FLOATLIT            { () }
    | STRLIT              { () }
    | arraylit            { () }
    | ID                  { () }
    | LPAREN expr RPAREN  { () }
    | expr EQ expr        { () }
    | expr NEQ expr       { () }
    | expr LT expr        { () }
    | expr LEQ expr       { () }
    | expr GT expr        { () }
    | expr GEQ expr       { () }
    | expr PLUS expr      { () }
    | expr MINUS expr     { () }
    | expr TIMES expr     { () }
    | expr DIVIDE expr    { () }
    | expr MOD expr       { () }
    | ID PLUSEQ expr       { () }
    | ID MINUSEQ expr       { () }
    | MINUS expr %prec NOT { () }
    | NOT expr { () }
    | expr LBRACKET expr RBRACKET { () }
    | NEW typ { () }
    | NEW typ LBRACKET expr RBRACKET { () }
    | DELETE ID { () }
    | ID ASSIGN expr { () }
    | STRUCTMEM ASSIGN expr { () }
    | ID LPAREN opt_args RPAREN { () }
    | STRUCTMEM LPAREN opt_args RPAREN { () }

opt_args : 
    { () }
    | args { () }

args : 
    expr { () }
    | args COMMA expr { () }
