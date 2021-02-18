/* Ocamlyacc parser for C-net */ 

%{
    open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI SQUOTE DQUOTE
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token AND OR NOT
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE
%token INT FLOAT 
%token NEW DELETE 
%token <int> INTLIT 
%token <string> ID FLOATLIT 
%token <string> STRLIT 
%token EOF

%start program

%type <Ast.program> program

%%

program: 
    EOF { () }
    
