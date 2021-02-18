{
    open Parser;;
    open Float
}

let alpha = ['a'-'Z']
let digit = ['0'-'9']
let squote = '\''
let bslash = '\\'

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { RBRACKET }
| ']' { LBRACKET }
| ',' { COMMA }
| ';' { SEMI }
| '\'' {SQUOTE}
| '"' {DQUOTE}
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LEQ }
| ">" { GT }
| ">=" { GEQ }
| "&&" { AND }
| "||" { OR }
| "if" { IF }
| "else" { ELSE }
| "for" { FOR }
| "while" { WHILE }
| "return" { RETURN }
| "break" { BREAK }
| "continue" { CONTINUE }
| "int" { INT }
| "float" { FLOAT }
| "char" { CHAR }
| "string" { STRING }
| "socket" { SOCKET }
| "struct" { STRUCT }
| "new" { NEW }
| "delete" { DELETE }
(* Now the tokens that have to be matched with regex *)
| "//" { scomment lexbuf }
| "/*" { mcomment lexbuf }
| (alpha | '_')+(alpha | digit | '_')* {ID}
| digit* as intlit { INTLIT(int_of_string intlit) } 
| '"'*'"' { STRLIT(str) } (* TODO parse only the string part; discard the quotes *)
| squote bslash digit digit digit squote { CHARLIT(0) } (* TODO extract char *)
| squote bslash alpha squote { CHARLIT(0) } (* TODO get special escape sequence *)
| digit+ '.' digit* as flt { FLOATLIT(flt) }
| eof { EOF }

and scomment = parse
'\n' { tokenize lexbuf }
| _ { scomment lexbuf }

and mcomment = parse
"*/" { tokenize lexbuf }
| _ { mcomment lexbuf }
