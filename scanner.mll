
{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse 
    [' ' '\t' '\r' '\n' ] { token lexbuf } (* Whitespace *)
  | "//" { slcomment lexbuf }               (* Single-line comment *) 
  | "/*" { mlcomment lexbuf }               (* Multi-line comment *)
  | "int"         { INT }
  | "bool"        { BOOL }
  | "float"       { FLOAT }
  | "string"      { STRING }
  | "void"        { VOID }
  | "if"          { IF }
  | "else"        { ELSE }
  | "for"         { FOR }
  | "while"       { WHILE }
  | "return"      { RETURN }
  | "!="          { NEQ }
  | "!"           { NOT }
  | '<'           { LT }
  | "<="          { LEQ }
  | ">"           { GT }
  | ">="          { GEQ }

and slcomment = parse 
    '\n'      { token lexbuf }
  | eof       { token lexbuf }
  | _         { slcomment lexbuf }

and mlcomment = parse 
    "*/"      { token lexbuf }
  | _         { mlcomment lexbuf }
