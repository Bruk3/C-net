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
    Ast ->
    let ast = Parser.program Scanner.tokenize lexbuf in
    print_string (Ast.string_of_program ast)
  | Scanner ->
    let token_string_list =
      let rec next accu =
        match Scanner.tokenize lexbuf with
        | Parser.EOF -> List.rev (Scanner_pp.pretty_print Parser.EOF :: accu)
        | x   -> next (Scanner_pp.pretty_print x :: accu)
      in next []
    in List.iter (fun x -> print_endline x) token_string_list
