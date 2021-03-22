module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)


(* translate : Sast.program -> Llvm.module *)
let translate (vdecls, strct_decls, fdecls) =
  let context    = L.global_context () in
  
  let cnet_module = L.create_module context "CNet" in 

  (* let debug = fun s ->  
    print_endline ("`````````````````````````````````````"^s);
    dump_module cnet_module;
    print_endline ("`````````````````````````````````````"^s);
    () *)

  (* Get types from the context *)
  let i8_t       = L.i8_type     context (* Char *)
  and i32_t      = L.i32_type    context (* Int *) 
  and float_t    = L.double_type context (* Float *)
  and void_t     = L.void_type   context 
  in
  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Char    -> i8_t
    | A.Int     -> i32_t
    | A.Float   -> float_t
    | A.Void    -> void_t
  in

  (* Create a map of global variables after creating each *)
  let global_decls : L.llvalue StringMap.t =
    let global_vdecl m (typ, name) = 
      let init = match typ with
          A.Float -> L.const_float (ltype_of_typ typ) 0.0
        | _ -> L.const_int (ltype_of_typ typ) 0
      in StringMap.add name (L.define_global name init cnet_module) m in
    List.fold_left global_vdecl StringMap.empty vdecls in


  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t cnet_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t cnet_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc) StringMap.t =
      let function_decl m fdecl =
        let ftyp = fdecl.fid.typ
        and name = fdecl.fid.rid
        and formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.formals)
        in let ftype = L.function_type (ltype_of_typ ftyp) formal_types in
        StringMap.add name (L.define_function name ftype cnet_module, fdecl) m in
      List.fold_left function_decl StringMap.empty fdecls in
    
    (* Fill in the body of the given function *)
    let build_function_body fdecl =
      let (the_function, _) = StringMap.find fdecl.fid.rid function_decls in
      let builder = L.builder_at_end context (L.entry_block the_function) in
  
      (* Not quite sure what this is yet *)
      let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
      and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
  
      (* Construct the function's "locals": formal arguments and locally
         declared variables.  Allocate each on the stack, initialize their
         value, if appropriate, and remember their values in the "locals" map *)
      let local_vars =
        let add_formal m (t, n) p = 
          L.set_value_name n p;
    let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);
    StringMap.add n local m 
  
        (* Allocate space for any locally declared variables and add the
         * resulting registers to our map *)
        and add_local m (t, n) =
    let local_var = L.build_alloca (ltype_of_typ t) n builder
    in StringMap.add n local_var m 
        in
  
        let formals = List.fold_left2 add_formal StringMap.empty fdecl.formals
            (Array.to_list (L.params the_function)) in
        (* List.fold_left add_local formals fdecl.slocals  *)
      

      (* Return the value for a variable or formal argument.
       Check local names first, then global names.
       Will change this later. Doesn't fit what we are trying to do *)
      let lookup n = try StringMap.find n local_vars
                      with Not_found -> StringMap.find n global_decls
  in

  (* Construct code for an expression; return its value *)
  let rec expr builder ((_, e) : sexpr) = match e with
      SNoexpr     -> L.const_int i32_t 0    
    | SIntlit i  -> L.const_int i32_t i
    | SCharlit c  -> L.const_int i8_t c
    | SFloatlit l -> L.const_float_of_string float_t l
    | SStrlit s   -> L.const_string context s 
    | Sid s       -> L.build_load (lookup s.rid) s.rid builder
    | SBinassor (s,op,e) -> let e' =  ( match op with
                                      Assign -> expr builder e
                                    | PlusEq -> expr builder Sexpr(s.typ,SBinop(s,A.Add,e))
                                    | MinusEq -> expr builder Sexpr(s.typ,SBinop(s,A.Sub,e)) )
                                   in ignore(L.build_store e' (lookup s) builder); e'
    | SBinop ((A.Float,_ ) as e1, op, e2) ->
  let e1' = expr builder e1
  and e2' = expr builder e2 in
  (match op with 
    A.Add     -> L.build_fadd
    | A.Sub     -> L.build_fsub
    | A.Mul    -> L.build_fmul
    | A.Div     -> L.build_fdiv 
    | A.Eq      -> L.build_fcmp L.Fcmp.Oeq
    | A.Neq     -> L.build_fcmp L.Fcmp.One
    | A.Lt      -> L.build_fcmp L.Fcmp.Olt
    | A.Leq     -> L.build_fcmp L.Fcmp.Ole
    | A.Gt      -> L.build_fcmp L.Fcmp.Ogt
    | A.Geq     -> L.build_fcmp L.Fcmp.Oge
    | A.And | A.Or ->
        raise (Failure "internal error: semant should have rejected and/or on float")
    ) e1' e2' "tmp" builder
    | SBinop (e1, op, e2) ->
  let e1' = expr builder e1
  and e2' = expr builder e2 in
  (match op with
    A.Add     -> L.build_add
    | A.Sub     -> L.build_sub
    | A.Mul    -> L.build_mul
    | A.Div     -> L.build_sdiv
    | A.And     -> L.build_and
    | A.Or      -> L.build_or
    | A.Eq      -> L.build_icmp L.Icmp.Eq
    | A.Neq     -> L.build_icmp L.Icmp.Ne
    | A.Lt      -> L.build_icmp L.Icmp.Slt
    | A.Leq     -> L.build_icmp L.Icmp.Sle
    | A.Gt      -> L.build_icmp L.Icmp.Sgt
    | A.Geq     -> L.build_icmp L.Icmp.Sge
    ) e1' e2' "tmp" builder
    | SUnop(op, ((t, _) as e)) ->
        let e' = expr builder e in
        (match op with
        A.Minus when t = A.Float -> L.build_fneg 
            | A.Minus                  -> L.build_neg
            | A.Not                  -> L.build_not) e' "tmp" builder
    | SCall ("print", [e]) | SCall ("printb", [e]) ->
  L.build_call printf_func [| int_format_str ; (expr builder e) |]
    "printf" builder
    | SCall ("printbig", [e]) ->
  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
    | SCall ("printf", [e]) -> 
  L.build_call printf_func [| float_format_str ; (expr builder e) |]
    "printf" builder
    | SCall (f, args) ->
       let (fdef, fdecl) = StringMap.find f function_decls in
 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
 let result = (match fdecl.fid.typ with 
                      A.Void -> ""
                    | _ -> f ^ "_result") in
       L.build_call fdef (Array.of_list llargs) result builder
  in
  
  (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
       let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
    Some _ -> ()
        | None -> ignore (instr builder) in
    
      (* Build the code for the given statement; return the builder for
         the statement's successor (i.e., the next instruction will be built
         after the one generated by this call) *)
  
      let rec stmt builder = function
      SBlock sl -> List.fold_left stmt builder sl
    | SVdecl(svdcl,e) -> add_local formals svdcl; expr builder Sexpr(svdcl.typ, SBinassor(svdcl,A.Assign,e)) ; builder
    | SExpr e -> ignore(expr builder e); builder 
    | SReturn e -> ignore(match fdecl.fid.typ with
                                (* Special "return nothing" instr *)
                                A.Void -> L.build_ret_void builder 
                                (* Build return statement *)
                              | _ -> L.build_ret (expr builder e) builder ); builder
    (* | SIf (predicate, then_stmt, else_stmt) ->
           let bool_val = expr builder predicate in
            let merge_bb = L.append_block context "merge" the_function in
              let build_br_merge = L.build_br merge_bb in (* partial function *)
  
          let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
            build_br_merge;
        
          let else_bb = L.append_block context "else" the_function in
          add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
            build_br_merge;
        
          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
   *)
    | SWhile (predicate, body) ->
      let pred_bb = L.append_block context "while" the_function in
      ignore(L.build_br pred_bb builder);
  
      let body_bb = L.append_block context "while_body" the_function in
      add_terminal (stmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);
  
      let pred_builder = L.builder_at_end context pred_bb in
      let bool_val = expr pred_builder predicate in
  
      let merge_bb = L.append_block context "merge" the_function in
      ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
      L.builder_at_end context merge_bb
  
        (* Implement for loops as while loops *)
    | SFor (e1, e2, e3, body) -> stmt builder
        ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
      in
  
      (* Build the code for each statement in the function *)
      let builder = stmt builder (SBlock fdecl.fbody) in
  
      (* Add a return if the last block falls off the end *)
      add_terminal builder (match fdecl.fid.typ with
          A.Void -> L.build_ret_void
        | A.Float -> L.build_ret (L.const_float float_t 0.0)
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in
  
    List.iter build_function_body fdecls in
    cnet_module