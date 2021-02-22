/* Ocamlyacc parser for C-net */ 

%{
    open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI SQUOTE DQUOTE
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token PLUSEQ MINUSEQ 
%token DOT
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

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc PLUSEQ MINUSEQ
%nonassoc LPAREN RPAREN
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left MOD
%right NOT
%left DOT



%%

program: 
    decls { () }
    | EOF { () }

decls : 
    decls decl { () }
    | decl { () }

decl:
   | vdecl { () }
   | sdecl { () }
   | fdecl { () }

typ : 
    CHAR  { () }
    | INT  { () }
    | FLOAT  { () }
    | STRING  { () }
    | SOCKET  { () }
    | STRUCT ID  { () }
    | typ LBRACKET RBRACKET { () }

vdecls:
    vdecls vdecl { () }
    | vdecl { () }

vdecl:
    typ ID SEMI { () }

/* sdecls: */
/*     sdecls sdecl { () } */
/*     | sdecl { () } */

sdecl:
    STRUCT ID LBRACE vdecls RBRACE SEMI { () }

fdecl :
    typ ID LPAREN opt_params RPAREN LBRACE opt_stmts RBRACE { () }

opt_params : 
    { () }
    | params { () }

params: 
    typ ID { () }
    | params COMMA typ ID { () }


opt_stmts: 
    {()}
    | stmts_gen { () }

stmts_gen:
    | stmts_gen stmt_gen { () }
    | stmt_gen { () }

stmt_gen :
    vdecl { () }
    | vdecl_assign { () }
    | stmt { () }
    | LBRACE stmts_gen RBRACE { () }

vdecl_assign:
    typ ID ASSIGN INTLIT SEMI { () }
    | typ ID ASSIGN NEW typ LBRACKET INTLIT RBRACKET LBRACE INTLIT RBRACE SEMI { () }


stmt: 
    opt_expr SEMI { () }
    | RETURN opt_expr SEMI { () }
    | IF LPAREN expr RPAREN stmt_gen ELSE stmt_gen    { () }
    | IF LPAREN expr RPAREN stmt_gen %prec NOELSE { () }
    | FOR LPAREN opt_expr SEMI opt_expr SEMI opt_expr RPAREN stmt_gen { () }
    | WHILE LPAREN expr RPAREN stmt_gen { () }

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
    | structmem           { () }
    /* | ID                  { () } */
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
    | LBRACKET expr RBRACKET { () }
    | NEW typ { () }
    | NEW typ LBRACKET expr RBRACKET { () }
    | DELETE ID { () }
    | structmem ASSIGN expr { () }
    | structmem LPAREN args RPAREN { () }


    /* | STRUCTMEM LPAREN opt_args RPAREN { () } */

/* a.name = "string"; */

structmem :
    ID { () }
    | ID DOT structmem { () }

/* opt_args : */ 
/*     { () } */
/*     | args { () } */

args : 
    expr { () }
    | args COMMA expr { () } 
/*



    */
