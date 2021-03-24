open Utils
type action =Scanner | Ast | LLVM_IR | Sast | Compile

let () =
  let action =  ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-t", Arg.Unit (set_action Scanner), "Print the Scanner Tokens");
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./cnet.native [-a|-s|-l|-c|-t] [file.cnet]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  try
    match !action with
    (*
     * Treat the scanner separately since it needs the lexbuf as it's being
     * parsed
     *)
      Scanner ->
      let token_string_list =
        let rec next accu =
          match Scanner.tokenize lexbuf with
          | Parser.EOF -> List.rev (Scanner_pp.pretty_print Parser.EOF :: accu)
          | x   -> next (Scanner_pp.pretty_print x :: accu)
        in next []
      in List.iter (fun x -> print_endline x) token_string_list; exit 0;


  (*
   * A bit inefficient to do all the steps no matter the flag, but it simplifies
   * the code a lot
   *)
    | _ ->
      let ast = Parser.program Scanner.tokenize lexbuf in
      (* let sast = Semant.check ast in *)
      (* let llvm_module = Codegen.translate sast in *)

      match !action with
        Ast -> print_string (Ast.string_of_program ast)
      | _ -> raise (Failure("This branch doesn't support that flag"))
      (* | Sast -> print_string(Sast.string_of_sprogram sast) *)
      (* | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate my_sast)) *)
      (* | Compile -> let m = Codegen.translate my_sast in *)
      (*   Llvm_analysis.assert_valid_module m; *)
      (*   print_string (Llvm.string_of_llmodule m) *)

  with
  Parsing.Parse_error ->
    let err_line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
    let spec_char = Lexing.lexeme lexbuf in
    let _  = Printf.printf "Syntax error on line %d near %s\n" err_line spec_char;
    in exit 1;
