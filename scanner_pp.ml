open Parser;;

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
  | RETURN                -> Printf.sprintf "RETURN"
  | NEW                   -> Printf.sprintf "NEW"
  | DELETE                -> Printf.sprintf "DELETE"
  | ID(x)                 -> Printf.sprintf  "ID(%s)" (x)
  | INTLIT(x)             -> Printf.sprintf  "INTLIT(%d)" (x)
  | STRLIT(x)             -> Printf.sprintf  "STRLIT(%s)" (x)
  | CHARLIT(x)            -> Printf.sprintf  "CHARLIT(%d)" (x)
  | FLOATLIT(x)           -> Printf.sprintf  "FLOATLIT(%f)" (x)

(* let lexbuf = Lexing.from_channel stdin in
   let token_string_list =
   let rec next accu =
    match Scanner.tokenize lexbuf with
    | EOF -> List.rev (pretty_print EOF :: accu)
    | x   -> next (pretty_print x :: accu)
   in next []
   in List.iter (fun x -> print_endline x) token_string_list  *)
