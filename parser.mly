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

stmt_gen :
    stmt { () }
    | LBRACE stmts_gen RBRACE { () }

stmts_gen:
    stmt { () }
    | stmts_gen vdecl { () } (* TODO this might have to become vdecl stmts_gen OR make vdecl and vdecl_assign terminals *)
    | stmts_gen vdecl_assign { () }
    | stmts_gen stmt { () }

vdecls:
    vdecls vdecl { () }
    | vdecl { () }

vdecl:
    typ ID SEMI { () }

vdecl_assign:
    typ ID ASSIGN expr SEMI

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

expr: 
    INTLIT                { () }
    | CHARLIT             { () }
    | FLOATLIT            { () }
    | STRLIT              { () }
    | ARRAYLIT            { () }
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
    | expr DIVIDE expr    { () }    (* revise *)
    | expr PEQ expr       { () }
    | expr MEQ expr       { () }
    | MINUS expr %prec NOT
    | NOT expr
    | NEW typ
    | NEW typ LBRACKET INTLIT RBRACKET 
    | DELETE ID
    | ID ASSIGN expr
    | ID DOT ID ASSIGN expr
    | ID LPAREN opt_args RPAREN
    | ID DOT ID LPAREN opt_args RPAREN
    | PLUSPLUS expr
    | MINUSMINUS expr

opt_args : 
    { () }
    | args { () }

args : 
    expr | args COMMA expr
