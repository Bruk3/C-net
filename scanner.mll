{
open Parser;;
open Utils;;
exception ScannerError of string;;

let scanner_err msg linenum =
  raise (ScannerError (Printf.sprintf msg linenum))
;;

}



let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let squote = '\''
let bslash = '\\'
let octal_dig = ['0'-'7']
let octal_triplet = (octal_dig)(octal_dig)(octal_dig)
let integer = digit+
let exp = 'e'['-''+']?['0'-'9']+
let sliterals = ([^ '\\' '"' '\n'] | ('\\'[^ '\n' ]))*
let cfloat = (
    ((digit)+'.'(digit)* (exp)?) |
    ((digit)* '.'(digit)+(exp)?) |
    ((digit)+exp)
)

let normal_id = (alpha | '_')(alpha | digit | '_')*

let whitespace = [' ' '\t' '\r' '\n']

let print_char = [' '-'~']

rule tokenize = parse
[' ' '\t' '\r'] { tokenize lexbuf }
| '\n' { Lexing.new_line lexbuf; tokenize lexbuf }
| '('  { LPAREN }
| ')'  { RPAREN }
| '{'  { LBRACE }
| '}'  { RBRACE }
| '['  { LBRACKET }
| ']'  { RBRACKET }
| ','  { COMMA }
| ';'  { SEMI }
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
| "else" whitespace+ as ws "if" {count_new_lines ws lexbuf; ELIF }
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
| "file"   { FILE }
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
(* | '"' ((print_char)* as str) '"' { STRLIT(str) } *)
| '"' { STRLIT(strlit "" lexbuf) }
| squote bslash ((octal_triplet) as oct_num)  squote { CHARLIT(int_of_string ("0o" ^ oct_num)) }
| squote squote { scanner_err "empty char literal on line %d" (line_num lexbuf)}
| squote bslash (['f' 'n' 'r' 't' '\\' '0' '\''] as spec_char) squote {
    match spec_char with
      'f' -> CHARLIT(12)
    | 'n' -> CHARLIT(10)
    | 'r' -> CHARLIT(13)
    | 't' -> CHARLIT(9)
    | '\\' -> CHARLIT(92)
    | '0' -> CHARLIT(0)
    | '\'' -> CHARLIT(47)
    | _ -> scanner_err "[COMPILER BUG] unaccounted escape character line %d" (line_num lexbuf)
}

| squote print_char squote as lxm               {CHARLIT(Char.code(lxm.[1]))} (*For chars like 'a'*)
| cfloat as flt { FLOATLIT(float_of_string flt) } (* TODO Optional negative sign *)

(* Error cases *)
| '"' | squote { scanner_err "unmatched quote on line %d" (line_num lexbuf) }
| _ { scanner_err "illegal character on line %d" (line_num lexbuf) }

| eof { EOF }

and strlit str_so_far = parse
    '"' { str_so_far }
| "\\n" { strlit (str_so_far ^ "\n") lexbuf }
| "\\t" { strlit (str_so_far ^ "\t") lexbuf }
| "\\r" { strlit (str_so_far ^ "\r") lexbuf }
| "\\0" { strlit (str_so_far ^ (String.make 1 (Char.chr 0))) lexbuf }
| "\\\\" { strlit (str_so_far ^ "\\") lexbuf }
| "\""    { strlit (str_so_far ^ "\"") lexbuf }
| "\\" ((octal_triplet) as oct_num)
   { strlit (str_so_far ^ (String.make 1 (Char.chr (int_of_string ("0o" ^ oct_num))))) lexbuf }
| print_char as lxm { strlit (str_so_far ^ (String.make 1 lxm)) lexbuf }
| eof { scanner_err "unmatched quote on line %d" (line_num lexbuf) }
| _ { scanner_err "illegal character in string literal on line %d" (line_num lexbuf) }

and scomment = parse
'\n' { Lexing.new_line lexbuf; tokenize lexbuf }
| eof { tokenize lexbuf }
| _ { scomment lexbuf }

and mcomment = parse
"*/" { tokenize lexbuf }
| '\n' { Lexing.new_line lexbuf; mcomment lexbuf }
| eof { raise (ScannerError("reached end of file with an unclosed multiline comment"))}
| _ { mcomment lexbuf }
