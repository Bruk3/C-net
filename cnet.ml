type action = Ast | Scanner

let () =
  let action =  ref Ast in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-t", Arg.Unit (set_action Scanner), "Print the Scanner Tokens");
  ] in
  let usage_msg = "usage: ./cnet.native [-a|-s|-l|-c|-t] [file.cnet]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  match !action with
    Scanner ->
    let token_string_list =
      let rec next accu =
        match Scanner.tokenize lexbuf with
        | Parser.EOF -> List.rev (Scanner_pp.pretty_print Parser.EOF :: accu)
        | x   -> next (Scanner_pp.pretty_print x :: accu)
      in next []
    in List.iter (fun x -> print_endline x) token_string_list
  | Ast ->
    match
      let ast = Parser.program Scanner.tokenize lexbuf in
      print_string (Ast.string_of_program ast)

    with
    exception (Parsing.Parse_error)  ->
      let err_line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum + 2 in
      let spec_char = Lexing.lexeme lexbuf in
      let _  = Printf.printf "Syntax error on line %d near %s\n..." err_line spec_char;
      in exit 1;
    (* let x = Lexing.lexeme_char lexbuf first_tok in *)
    (* let curr = lexbuf.lex_curr_p in *)
    (* let line = curr.Lexing.pos_lnum in *)
    (* let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in *)
    (* let tok = Lexing.lexeme lexbuf in *)
    (* raise (Failure(tok ^ (string_of_int line) ^ " " ^ *)
    (*                Char.escaped x)) *)
    | _ -> ()

