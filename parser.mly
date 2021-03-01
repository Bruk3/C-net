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
    decls decl { () }
    | decl { () }

decl:
   | vdecl { () }
   | sdecl { () }
   | fdecl { () }

typ : 
    VOID   { () }
    | CHAR  { () }
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
    { Noexpr }
    | expr { $1 }

opt_arraylit:
        { () }
    | LBRACE args RBRACE { () }

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
    // | LBRACKET expr RBRACKET { () } /* TODO why is this here? What does [3] do?  */
    | NEW typ { () }
    | NEW typ LBRACKET expr RBRACKET opt_arraylit { () }
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
