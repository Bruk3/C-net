{
    open Parser;;
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let squote = '\''
let bslash = '\\'
let octal_dig = ['0'-'7']
let octal_triplet = (octal_dig)(octal_dig)(octal_dig)
let normal_id = (alpha | '_')(alpha | digit | '_')* 


let print_char = [' '-'~']

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
| '%' { MOD }
| "+=" { PLUSEQ }
| "-=" { MINUSEQ }
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LEQ }
| ">" { GT }
| ">=" { GEQ }
| "&&" { AND }
| "||" { OR }
| "!" { NOT }
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
| "void" { VOID }
| "struct" { STRUCT }
| "socket" { SOCKET }
| "TCP" {TCP}
| "UDP" {UDP}
| '.' { DOT }
| "new" { NEW }
| "delete" { DELETE }
(* Now the tokens that have to be matched with regex *)
| "//" { scomment lexbuf }
| "/*" { mcomment lexbuf }
| normal_id {ID}
| ((normal_id)('.'))+normal_id { STRUCTMEM }
| digit* as intlit { INTLIT(int_of_string intlit) } (* TODO possibly negative*)
| '"' (print_char*) as str '"' { STRLIT(str) } 
| squote bslash ((octal_triplet) as oct_num)  squote { CHARLIT(int_of_string ("0o" ^ oct_num)) }
| squote bslash ('n' | 't' | '\\' | '0') squote { CHARLIT(0) } (* TODO replace special char with number *)
| digit+ '.' digit* as flt { FLOATLIT(flt) } (* TODO Optional negative sign *)
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
| eof { EOF }

and scomment = parse
'\n' { tokenize lexbuf }
| eof { tokenize lexbuf }
| _ { scomment lexbuf }

and mcomment = parse
"*/" { tokenize lexbuf }
| _ { mcomment lexbuf }
