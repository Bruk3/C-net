{
    open Parser;;
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let squote = '\''
let bslash = '\\'
let octal_dig = ['0'-'7']
let octal_triplet = (octal_dig)(octal_dig)(octal_dig)
let integer = digit+
let normal_id = (alpha | '_')(alpha | digit | '_')* 

let print_char = [' '-'~']

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '('  { LPAREN }
| ')'  { RPAREN }
| '{'  { LBRACE }
| '}'  { RBRACE }
| '['  { LBRACKET }
| ']'  { RBRACKET }
| ','  { COMMA }
| ';'  { SEMI }
| '\'' {SQUOTE}
| '"'  {DQUOTE}
(* Operators *)
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
| '.' { DOT }
(*Control flow*)
| "if" { IF }
| "else" { ELSE }
| "for" { FOR }
| "while" { WHILE }
| "break" { BREAK }
| "continue" { CONTINUE }
(*Types*)
| "int" { INT }
| "float" { FLOAT }
| "char" { CHAR }
| "string" { STRING }
| "void" { VOID }
| "struct" { STRUCT }
| "socket" { SOCKET }
| "TCP" {TCP}
| "UDP" {UDP}
(*Functions*)
| "return" { RETURN }
(*Memory*)
| "new" { NEW }
| "delete" { DELETE }

(* Now the tokens that have to be matched with regex *)
| "//" { scomment lexbuf }
| "/*" { mcomment lexbuf }
| normal_id as lxm {ID(lxm)}

| integer as lxm { INTLIT(int_of_string lxm) }
| '"' ((print_char)* as str) '"' { STRLIT(str) } 
| squote bslash ((octal_triplet) as oct_num)  squote { CHARLIT(int_of_string ("0o" ^ oct_num)) }
| squote bslash ('n' | 't' | '\\' | '0') squote { CHARLIT(0) } (* TODO replace special char with number *) (*Kingsley: what is this one for?*)
| squote print_char squote as lxm               {CHARLIT(Char.code(lxm.[1]))} (*For chars like 'a'*)
| digit+ '.' digit* as flt { FLOATLIT(float_of_string flt) } (* TODO Optional negative sign *)
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

| eof { EOF }


and scomment = parse
'\n' { tokenize lexbuf }
| eof { tokenize lexbuf }
| _ { scomment lexbuf }

and mcomment = parse
"*/" { tokenize lexbuf }
| _ { mcomment lexbuf }

{
  let pretty_print = function
  | LPAREN                -> Printf.sprintf "LPAREN"
  | RPAREN                -> Printf.sprintf "RPAREN"
  | LBRACE                -> Printf.sprintf "LBRACE"
  | RBRACE                -> Printf.sprintf "RBRACE"
  | RBRACKET              -> Printf.sprintf "RBRACKET"
  | LBRACKET              -> Printf.sprintf "LBRACKET"
  | EOF                   -> Printf.sprintf "EOF"
  | COMMA                 -> Printf.sprintf "COMMA"
  | SEMI                  -> Printf.sprintf "SEMI"
  | SQUOTE                -> Printf.sprintf "SQUOTE"
  | DQUOTE                -> Printf.sprintf "DQUOTE"
  | PLUS                  -> Printf.sprintf "PLUS"
  | MINUS                 -> Printf.sprintf "MINUS"
  | TIMES                 -> Printf.sprintf "TIMES"
  | DIVIDE                -> Printf.sprintf "DIVIDE"
  | ASSIGN                -> Printf.sprintf "ASSIGN"
  | PLUSEQ                -> Printf.sprintf "PLUSEQ"
  | MINUSEQ               -> Printf.sprintf "MINUSEQ"
  | EQ                    -> Printf.sprintf "EQ"
  | NEQ                   -> Printf.sprintf "NEQ"
  | NOT                   -> Printf.sprintf "NOT"
  | LT                    -> Printf.sprintf "LT"
  | LEQ                   -> Printf.sprintf "LEQ"
  | GT                    -> Printf.sprintf "GT"
  | GEQ                   -> Printf.sprintf "GEQ"
  | AND                   -> Printf.sprintf "AND"
  | OR                    -> Printf.sprintf "OR"
  | DOT                   -> Printf.sprintf "DOT"
  | MOD                   -> Printf.sprintf "MOD"
  | IF                    -> Printf.sprintf "IF"
  | ELSE                  -> Printf.sprintf "ELSE"
  | FOR                   -> Printf.sprintf "FOR"
  | WHILE                 -> Printf.sprintf "WHILE"
  | BREAK                 -> Printf.sprintf "BREAK"
  | CONTINUE              -> Printf.sprintf "CONTINUE"
  | INT                   -> Printf.sprintf "INT"
  | FLOAT                 -> Printf.sprintf "FLOAT"
  | CHAR                  -> Printf.sprintf "CHAR"
  | STRING                -> Printf.sprintf "STRING"
  | SOCKET                -> Printf.sprintf "SOCKET"
  | STRUCT                -> Printf.sprintf "STRUCT"
  | VOID                  -> Printf.sprintf "VOID"
  | TCP                   -> Printf.sprintf "TCP"
  | UDP                   -> Printf.sprintf "UDP"
  | RETURN                -> Printf.sprintf "RETURN"
  | NEW                   -> Printf.sprintf "NEW"
  | DELETE                -> Printf.sprintf "DELETE"
  | ID(x)                 -> Printf.sprintf  "ID(%s)" (x)
  | INTLIT(x)             -> Printf.sprintf  "INTLIT(%d)" (x)
  | STRLIT(x)             -> Printf.sprintf  "STRLIT(%s)" (x)
  | CHARLIT(x)            -> Printf.sprintf  "CHARLIT(%d)" (x)
  | FLOATLIT(x)           -> Printf.sprintf  "FLOATLIT(%f)" (x)

  in 

  let lexbuf = Lexing.from_channel stdin in
  let token_string_list =
    let rec next accu = 
      match tokenize lexbuf with 
      | EOF -> List.rev (pretty_print EOF :: accu)
      | x   -> next (pretty_print x :: accu)
    in next []
  in List.iter (fun x -> print_endline x) token_string_list 
}
