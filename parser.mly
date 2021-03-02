/* Ocamlyacc parser for C-net */ 

%{
    open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI SQUOTE DQUOTE
%token MOD ASSIGN
%token PLUS MINUS TIMES DIVIDE
%token PLUSEQ MINUSEQ 
%token DOT
%token EQ NEQ LT LEQ GT GEQ
%token AND OR NOT
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE
%token NOELSE
%token INT FLOAT CHAR STRING VOID STRUCT SOCKET
%token TCP UDP
%token NEW DELETE 
%token <int> INTLIT CHARLIT
%token <float> FLOATLIT 
%token <string> STRLIT 
%token <string> ID  
%token EOF

%start program

%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc PLUSEQ MINUSEQ
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
    decls EOF { $1 }

decls : 
    decls decl { $2::$1 }
    | decl { [$1] }

decl:
   | vdecl { () }
   | sdecl { () }
   | fdecl { () }

typ : 
    VOID   { Void }
    | CHAR  { Char }
    | INT  { Int }
    | FLOAT  { Float }
    | STRING  { String }
    | SOCKET  { () }
    | STRUCT ID  { () }
    | typ LBRACKET RBRACKET { Array($1) }

vdecls:
    vdecls vdecl { $2::$1 }
    | vdecl { [$1] }

vdecl:
    typ ID SEMI { () }

sdecl:
    STRUCT ID LBRACE vdecls RBRACE SEMI { () }

fdecl :
    typ ID LPAREN opt_params RPAREN LBRACE opt_stmts RBRACE { () }

opt_params : 
    { [] }
    | params { List.rev $1 }

params: 
    typ ID { [($1, $2)] }
    | params COMMA typ ID { ($3, $4) :: $1 }


opt_stmts: 
    {()}
    | stmts { List.rev $1 }

stmts:
    | stmts stmt { $2::$1 }
    | stmt { [$1] }

vdecl_assign:
    typ ID ASSIGN expr SEMI { VDecl({vtyp: $1, vname: $2}) }
    /* | typ ID ASSIGN NEW typ LBRACKET INTLIT RBRACKET LBRACE INTLIT RBRACE SEMI { () } */
    /* became redundant because expr handles array literals */

stmt: 
    expr SEMI { () }
    | RETURN opt_expr SEMI { () }
    | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
    | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
    | FOR LPAREN opt_expr SEMI opt_expr SEMI opt_expr RPAREN stmt { For($3, $5, $7, $9) }
    | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
    | vdecl { () }
    | vdecl_assign { () }
    | LBRACE stmts RBRACE { () }

opt_expr: 
    { Noexpr }
    | expr { $1 }

opt_arraylit:
        { [] }
    | LBRACE args RBRACE { $2 }

expr: 
    INTLIT                { Intlit($1) }
    | CHARLIT             { Charlit($1) }
    | FLOATLIT            { Floatlit($1) }
    | STRLIT              { Strlit($1) }
    | id                  { Id($1) }
    | LPAREN expr RPAREN  { $2 }
    | expr EQ expr        { Binrelop($1, Eq, $3) }
    | expr NEQ expr       { Binrelop($1, Neq, $3) }
    | expr LT expr        { Binrelop($1, Lt, $3)}
    | expr LEQ expr       { Binrelop($1, Leq, $3)}
    | expr GT expr        { Binrelop($1, Gt, $3) }
    | expr GEQ expr       { Binrelop($1, Geq, $3) }
    | expr PLUS expr      { Binariop($1, Add, $3) } 
    | expr MINUS expr     { Binariop($1, Sub, $3) }
    | expr TIMES expr     { Binariop($1, Mul, $3) }
    | expr DIVIDE expr    { Binariop($1, Div, $3) }
    | expr MOD expr       { Binariop($1, Mod, $3) }
    | id ASSIGN expr      { Binassop($1, Assign, $3) }
    | id PLUSEQ expr      { Binassop($1, PlusEq, $3) }
    | id MINUSEQ expr     { Binassop($1, MinusEq, $3) }
    | MINUS expr %prec NOT { Unariop(Minus, $2) }
    | NOT expr { Unrelop(Not, $2) }
    | NEW typ { () }
    | NEW typ LBRACKET expr RBRACKET opt_arraylit { () }
    // | NEW newable { New($2) }
    | DELETE ID { () }
    | id LBRACKET expr RBRACKET { () }
    | id LPAREN opt_args RPAREN { () }

id :
    ID { Id($1) }
    | ID DOT id { () }

opt_args : 
    { [] }
    | args { List.rev $1 }

args : 
    expr { [$1] }
    | args COMMA expr { $3 :: $1 } 
