module L = Llvm
module A = Ast
module U = Utils
open Sast


module StringMap = Map.Make(String)


(* translate : Sast.program -> Llvm.module *)
let translate (sdecl_list : sprogram) =
  (* replace with (vdecls, strct_decls, fdecls) *)
  let (_, _, fdecls) = U.decompose_program sdecl_list in
(* let translate ((vdecls : (A.vdecl * sexpr) list), (strct_decls : strct list), (fdecls : sfunc list)) = *)
  let context    = L.global_context () in

  let the_module = L.create_module context "CNet" in

(*******************************************************************************
 *                                   Types
 *******************************************************************************)

  (* Get types from the context *)
  let i8_t       = L.i8_type     context (* Char *)
  and i32_t      = L.i32_type    context (* Int *)
  and float_t    = L.double_type context (* Float *)
  and void_t     = L.void_type   context in
  let str_t      = L.pointer_type i8_t in

  (* Return the LLVM type for a cnet type *)
  let ltype_of_typ (t : A.typ) : (L.lltype) = match t with
      A.Char    -> i8_t
    | A.Int     -> i32_t
    | A.Float   -> float_t
    | A.Void    -> void_t
    | A.String  -> str_t
    | _         -> raise (Failure("Type not yet implemented"))
  in

  (* Kidus: we don't need this part yet (for the hello world) *)
  (* (1* Create a map of global variables after creating each *1) *)
  (* let global_decls : L.llvalue StringMap.t = *)
  (*   let global_vdecl m (typ, name) = *)
  (*     let init = match typ with *)
  (*         A.Float -> L.const_float (ltype_of_typ typ) 0.0 *)
  (*       | _ -> L.const_int (ltype_of_typ typ) 0 *)
  (*     in StringMap.add name (L.define_global name init the_module) m in *)
  (*   List.fold_left global_vdecl StringMap.empty vdecls in *)


  (*******************************************************************************
   *                             Built in functions
   *******************************************************************************)

  (* The function that writes to and reads from sockets/files, including stdin
   * and stdout
  *)
  let println_t : L.lltype =
    L.function_type i32_t [| i32_t; L.pointer_type i8_t; i32_t |] in
  let println_func : L.llvalue =
    L.declare_function "println" println_t the_module in
  (* TODO: read_line, read, print, send, atoi, ... *)

  (*******************************************************************************
   *                            Function signatures
   *******************************************************************************)

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc) StringMap.t =
    let function_decl m (fdecl : sfunc) =
      let ftyp = fdecl.styp
      and name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map
                         (function (t,_) -> ltype_of_typ t)
                         fdecl.sparameters) in
      let ftype = L.function_type (ltype_of_typ ftyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty fdecls in


  (*******************************************************************************
   *                              Function bodies
   *******************************************************************************)

  (* Fill in the body of the given function *)
  let build_function_body (fdecl : sfunc) =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)


    (* Construct code for an expression; return its value *)

    (* Kidus: I've deleted all but the ones we need for hello world *)
    let rec expr builder ((_, e) : sexpr) = match e with
        SIntlit i   -> L.const_int i32_t i
      | SStrlit s   -> L.build_global_stringptr (s ^ "\n") "tmp" builder
      | SCall("println", (A.String, SId(A.FinalID("stdout"))) :: (A.String, SStrlit(s)) :: []) ->
        L.build_call println_func
          [| L.const_int i32_t 1;
             (expr builder (A.String, SStrlit(s)));
             L.const_int i32_t (String.length s )
          |]
          "" builder
      | _ -> raise (Failure("Expression type not implemented"))
    in

    (* LLVM insists each basic block end with exactly one "terminator"
         instruction that transfers control.  This function runs "instr builder"
         if the current block does not already have a terminator.  Used,
         e.g., to handle the "fall off the end of the function" case. *)
    (* Don't need this for hello world either *)
    (* let add_terminal builder instr = *)
    (*   match L.block_terminator (L.insertion_block builder) with *)
    (*     Some _ -> () *)
    (*   | None -> ignore (instr builder) *)
    (* in *)

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let stmt builder = function
      | SExpr e -> ignore(expr builder e); builder
      | SReturn e -> ignore(match fdecl.styp with
          (* Special "return nothing" instr *)
            A.Void -> L.build_ret_void builder
          (* Build return statement *)
          | _ -> L.build_ret (expr builder e) builder ); builder

      | _ -> raise (Failure("Unimplemented statement type"))
    in
    List.iter (fun s -> ignore (stmt builder s); ()) fdecl.sbody in
  List.iter build_function_body fdecls; the_module;
