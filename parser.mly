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
%token INT FLOAT CHAR STRING VOID STRUCT SOCKET
/* %token TCP UDP */
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
%left ELIF
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
    decls EOF { Program(List.rev $1) }

decls :
    decls decl { $2::$1 }
    | decl { [$1] }

decl:
   | vdecl { GVdecl($1) }
   | sdecl { Sdecl($1) }
   | fdecl { Fdecl($1) }

typ :
    VOID   { Void }
    | CHAR  { Char }
    | INT  { Int }
    | FLOAT  { Float }
    | STRING  { String }
    | SOCKET  { Socket }
    | STRUCT ID  { Struct($2) }
    | typ LBRACKET RBRACKET { Array($1) } /* This is kinda awkward */

vdecls:
    vdecls vdecl { $2::$1 }
    | vdecl { [$1] }

vdecl:
    typ ID SEMI { {vtyp = $1; vname = $2} }

sdecl:
        STRUCT ID LBRACE vdecls RBRACE SEMI { {name = $2; members = List.rev $4} }

fdecl :
    typ ID LPAREN opt_params RPAREN LBRACE opt_stmts RBRACE
                    { {t = $1; name = $2; parameters = $4; body = $7} }

opt_params :
    { [] }
    | params { List.rev $1 }

params:
    typ ID { [ Id($1, $2) ] }
    | params COMMA typ ID { Id($3, $4) :: $1 }


opt_stmts:
    { [] }
    | stmts { List.rev $1 }

stmts:
    | stmts stmt { $2::$1 }
    | stmt { [$1] }

vdecl_assign:
    typ ID ASSIGN expr SEMI { Vdecl_assign({vtyp = $1; vname = $2}, $4) }
    /* | typ ID ASSIGN NEW typ LBRACKET INTLIT RBRACKET LBRACE INTLIT RBRACE SEMI { () } */
    /* became redundant because expr handles array literals */

stmt:
    opt_expr SEMI { Expr($1) }
    | RETURN opt_expr SEMI { Return($2) }
    | ifstmt ELSE stmt    { If($1, $3)}
    | ifstmt %prec NOELSE { If($1,Expr(Noexpr))}
    | FOR LPAREN opt_expr SEMI opt_expr SEMI opt_expr RPAREN stmt { For($3, $5, $7, $9) }
    | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
    | vdecl { Vdecl($1) }
    | vdecl_assign { $1 }
    | LBRACE stmts RBRACE { Block(List.rev $2) }

ifstmt:
    IF LPAREN expr RPAREN stmt %prec ELIF { [ ($3,$5) ] }
    | IF LPAREN expr RPAREN stmt ELSE ifstmt { ($3,$5)::$7 }


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
    | id                  { Rid($1) }
    | LPAREN expr RPAREN  { $2 }
    | expr EQ expr        { Binop($1, Eq, $3) }
    | expr NEQ expr       { Binop($1, Neq, $3) }
    | expr LT expr        { Binop($1, Lt, $3)}
    | expr LEQ expr       { Binop($1, Leq, $3)}
    | expr GT expr        { Binop($1, Gt, $3) }
    | expr GEQ expr       { Binop($1, Geq, $3) }
    | expr PLUS expr      { Binop($1, Add, $3) }
    | expr MINUS expr     { Binop($1, Sub, $3) }
    | expr TIMES expr     { Binop($1, Mul, $3) }
    | expr DIVIDE expr    { Binop($1, Div, $3) }
    | expr MOD expr       { Binop($1, Mod, $3) }
    | id ASSIGN expr      { Binassop($1, Assign, $3) }
    | id PLUSEQ expr      { Binassop($1, PlusEq, $3) }
    | id MINUSEQ expr     { Binassop($1, MinusEq, $3) }
    | MINUS expr %prec NOT { Unop(Minus, $2) }
    | NOT expr { Unop(Not, $2) }
    | NEW STRUCT ID { New(NStruct($3)) }
    | NEW typ LBRACKET expr RBRACKET opt_arraylit { ArrayLit($2, $4, $6) }
    | DELETE id { Delete($2) }
    | id LBRACKET expr RBRACKET { Index($1, $3) }
    | id LPAREN opt_args RPAREN { Call($1, $3) }

id :
    ID { FinalID(Nid($1)) }
    | id DOT ID { RID($1, $3) }

opt_args :
    { [] }
    | args { List.rev $1 }

args :
    expr { [$1] }
    | args COMMA expr { $3 :: $1 }
