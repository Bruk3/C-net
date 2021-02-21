{
    (*open Parser;;*)
    type token = 
     | LPAREN
     | RPAREN
     | LBRACE
     | RBRACE
     | RBRACKET
     | LBRACKET
     | EOF
     | COMMA
     | SEMI
     | SQUOTE
     | DQUOTE

}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let squote = '\''
let bslash = '\\'
let octal_dig = ['0'-'7']
let octal_triplet = (octal_dig)(octal_dig)(octal_dig)
let normal_id = (alpha | '_')(alpha | digit | '_')* 
let esc_char = bslash [''' '"' '\\' 'r' 't' 'n']


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
(*
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { ASSIGN }
| "+=" { PLUSEQ }
| "-=" { MINUSEQ }
| "==" { EQ }
| "!=" { NEQ }
| "!" { NOT }
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
| "void" { VOID }
| "int" { INT }
| "float" { FLOAT }
| "char" { CHAR }
| "string" { STRING }
| "socket" { SOCKET }
| "struct" { STRUCT }
| "TCP" {TCP}
| "UDP" {UDP}
| '.' { DOT }
| "new" { NEW }
| "delete" { DELETE }
(* Now the tokens that have to be matched with regex *)
| "//" { scomment lexbuf }
| "/*" { mcomment lexbuf }
| normal_id as lxm {ID(lxm)}
| ((normal_id)('.'))+normal_id { STRUCTMEM }

| digit+ as lxm { INTLIT(int_of_string lxm) } (* TODO possibly negative*)
| '"' ((print_char | esc_char)* as str) '"' { STRLIT(str) } 
| squote bslash ((octal_triplet) as oct_num)  squote { CHARLIT(int_of_string ("0o" ^ oct_num)) }
| squote bslash ('n' | 't' | '\\' | '0') squote { CHARLIT(0) } (* TODO replace special char with number *) (*Kingsley: what is this one for?*)
| squote print_char squote as lxm               {CHARLIT(lxm.[1])} (*For chars like 'a'*)
| digit+ '.' digit* as flt { FLOATLIT(flt) } (* TODO Optional negative sign *)
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
*)
| eof { EOF }

(*
and scomment = parse
'\n' { tokenize lexbuf }
| eof { tokenize lexbuf }
| _ { scomment lexbuf }

and mcomment = parse
"*/" { tokenize lexbuf }
| _ { mcomment lexbuf }
*)

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