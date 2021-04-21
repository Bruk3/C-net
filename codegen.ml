module L = Llvm
module A = Ast
module U = Utils
open Sast
open Ast


module StringMap = Map.Make(String)

(* this should see much fewere uses than SemanticError since codegen should work
 * relatively perfectly if the semantic checking has passed
 *)
exception CodegenError of string * int;;
(* The function for raising a codegen error *)
let codegen_err (msg : string) =
  raise (CodegenError(msg, -1));;

(* translate : Sast.program -> Llvm.module *)
let translate (sdecl_list : sprogram) =
  (* replace with (vdecls, strct_decls, fdecls) *)
  let (vdecls, sdecls, fdecls) = U.decompose_program sdecl_list in
(* let translate ((vdecls : (A.vdecl * sexpr) list), (strct_decls : strct list), (fdecls : sfunc list)) = *)
  let context    = L.global_context () in

  let the_module = L.create_module context "CNet" in

  let find_checked n m =
    if StringMap.mem n m then
      StringMap.find n m
    else
      codegen_err ("[COMPILER BUG] couldn't find id " ^ n)
  in

(*******************************************************************************
 *                                   Types
 *******************************************************************************)

  (* Get types from the context *)
  let i8_t       = L.i8_type     context (* Char *)
  and i32_t      = L.i32_type    context (* Int *)
  and float_t    = L.double_type context (* Float *)
  and void_t     = L.void_type   context in
  let ptr_t t    = L.pointer_type t in


  (* Return the LLVM type for a cnet type *)
  let rec ltype_of_typ (t : A.typ) (cstrcts : (A.strct * L.lltype) StringMap.t)
    : (L.lltype) = match t with
      A.Char            -> i8_t
    | A.Int             -> i32_t
    | A.Float           -> float_t
    | A.Void            -> void_t
    | A.String          -> ptr_t (snd (find_checked "string" cstrcts))
    | A.Struct(n)       -> ptr_t (snd (find_checked n cstrcts))
    | A.Array(typ)      -> ptr_t (snd (find_checked "array" cstrcts))
    | A.Socket          -> ptr_t (snd (find_checked "cnet_socket" cstrcts))
    | A.File            -> ptr_t (snd (find_checked "cnet_file" cstrcts))
  in

  let  size_of t = match t with
    A.Char            -> 1
  | A.Int             -> 4
  | _                 -> 8 (* Will Implement later*)
  in

(*******************************************************************************
   *                            Declare all the structs
 *******************************************************************************)
  let cstructs : (A.strct * L.lltype) StringMap.t =
    let declare_struct m (s : strct) =
      let cur_strct = L.named_struct_type context s.sname in
      let m = StringMap.add s.sname (s, cur_strct) m in
      let cmembers =
        Array.of_list (List.map (fun {vname=_; vtyp=t} -> ltype_of_typ t m)
                         s.members)
      in
      let _ = L.struct_set_body cur_strct cmembers false in
      m
    in
    (* TODO: instead of an empty stringmap, the list should be folded on the
     * default struct declarations (io/string/array)
     *)
    let cbuiltinstrcts =
      List.fold_left declare_struct StringMap.empty
        U.builtin_structs_l
    in
    List.fold_left declare_struct cbuiltinstrcts sdecls
  in

  let ltype_of_typ t = ltype_of_typ t cstructs in

  let cbuiltin_vars =
    let declare_struct_var {vtyp=vt; vname=vn} =
      let the_v  = (L.declare_global (ltype_of_typ vt) ("cnet_" ^ vn) the_module) in
      L.set_externally_initialized true the_v; ({vtyp=vt; vname=vn}, the_v)
    in
    StringMap.fold
      (fun k i m -> StringMap.add ("cnet_" ^ k) (declare_struct_var i) m)
      U.builtin_vars StringMap.empty
  in


  (* (1* Create a map of global variables after creating each 1 *)
  let global_decls : (A.vdecl * L.llvalue) StringMap.t =
    let global_vdecl m (vd, e) =
      let init = match e with
          A.Float, SFloatlit(f) -> L.const_float (ltype_of_typ A.Float) f
        | A.Int, SIntlit(i)     -> L.const_int (ltype_of_typ A.Int) i
        | A.Char, SCharlit(c)   -> L.const_int (ltype_of_typ A.Char) c
        | A.Struct(n) as t, _   -> L.const_pointer_null (ltype_of_typ t)
        (* | A.String, SStrlit(s)  -> L. *)
        (* | _ -> L.const_int (ltype_of_typ A.Void) 0 *)
      in StringMap.add vd.vname (vd, (L.define_global vd.vname init the_module)) m in
     List.fold_left global_vdecl cbuiltin_vars vdecls in


  (*******************************************************************************
   *                             Built in functions
   *******************************************************************************)

  (* The function that writes to and reads from sockets/files, including stdin
   * and stdout
  *)
  let builtin_func_decls : (L.llvalue * sfunc) StringMap.t =
    let function_type (fd : sfunc) =
      L.function_type
        (ltype_of_typ fd.styp)
        (Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fd.sparameters))
    in
    let declare_func m (fd : sfunc) =
      let the_func = L.declare_function fd.sfname (function_type fd) the_module in
      StringMap.add fd.sfname (the_func, fd) m
    in

    List.fold_left declare_func StringMap.empty U.sbuiltin_funcs_l
  in


  (* let println_t : L.lltype = *)
  (*   L.function_type i32_t [| i32_t; L.pointer_type i8_t; i32_t |] in *)
  (* let println_func : L.llvalue = *)
  (*   L.declare_function "println" println_t the_module in *)
  let var_arr_t t : L.lltype =
      L.var_arg_function_type (ltype_of_typ t) [| (ltype_of_typ t) |] in
  let arr_t t : L.lltype =
    L.function_type (ltype_of_typ t) [| i32_t;i32_t; (var_arr_t t)|] in
  let init_array_func t: L.llvalue =
      L.declare_function "cnet_init_array" (arr_t t) the_module in
  let arr_idx_t t: L.lltype =
    L.function_type (ltype_of_typ t) [| L.pointer_type (ltype_of_typ t) ; i32_t|] in
  let get_arr_index_func t: L.llvalue =
    L.declare_function "get_arr_index" (arr_idx_t t) the_module in

  let cnet_new_str_nolen_t: L.lltype =
    L.function_type (ltype_of_typ A.String) [| ptr_t i8_t |] in
  let cnet_new_str_func  =
    L.declare_function "cnet_new_str_nolen" cnet_new_str_nolen_t the_module in
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
    List.fold_left function_decl builtin_func_decls fdecls in

  let find_func_full fname =
    if StringMap.mem fname function_decls then
      StringMap.find fname function_decls
    else
      codegen_err ("[COMPILER BUG] couldn't find function" ^ fname)
  in
  let find_func fname = fst (find_func_full fname) in


  (*******************************************************************************
   *                              Function bodies
   *******************************************************************************)

  (* Fill in the body of the given function *)
  let build_function_body (fdecl : sfunc) =
    let (the_function, _) =
      if StringMap.mem fdecl.sfname function_decls then
        StringMap.find fdecl.sfname function_decls
      else
        codegen_err ("[COMPILER BUG] couldn't find function" ^ fdecl.sfname)

    in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
      let local_vars : (A.vdecl * L.llvalue) StringMap.t=
        let add_formal m (t, n) p =
          L.set_value_name n p;
          let local = L.build_alloca (ltype_of_typ t) n builder in
                ignore (L.build_store p local builder);
          StringMap.add n ({vtyp=t;vname=n},local) m

        and add_local m (t, n) =
          let local_var = L.build_alloca (ltype_of_typ t) n builder
          in StringMap.add n ({vtyp=t;vname=n}, local_var) m

      in
      List.fold_left2 add_formal StringMap.empty fdecl.sparameters
            (Array.to_list (L.params the_function))
        (* in *)
      (* List.fold_left add_local formals fdecl.sparameters *)
      in
      let func_scope = [global_decls]
      in
      (* Return the value for a variable or formal argument.
        Check local names first, then global names *)
      let rec lookup_helper (n : string) scope : (A.vdecl * L.llvalue) = match scope with
          [] -> codegen_err ("[COMPILER BUG] cannot find variable " ^ n)
        | hd :: tl ->
          if StringMap.mem n hd then
            StringMap.find n hd
          else
            lookup_helper n tl
      in

      let rec lookup n (t : A.typ) scope builder = match n with
          FinalID s -> lookup_helper s scope
        | RID(r, member) ->
          let vd, ll = lookup r t scope builder in
          let sname = match vd.vtyp with Struct(n) -> n in
          let sd,s = find_checked sname cstructs in
          let the_struct = L.build_load ll "tmp" builder in
          (vd, L.build_struct_gep the_struct (U.mem_to_idx sd member) "tmp" builder)

          (*   lookup_helper r curr_scope  (*This is wrong, need to fix*)*)
          (* | Index(r,e)     -> lookup_helper r curr_scope   (*This is wrong, need to fix*)*)
          (* | RID(r, member) ->
            let the_struct = lookup_helper r scope
            in (match the_struct with
              Struct(sname) ->
                 let the_struct = StringMap.find sname cstructs in (*This is wrong, need to fix*)
                 match List.filter (fun t -> t.vname = member) the_struct.members with
                   m :: [] -> m)

          | Index(r, e) ->
            let (t, _) = expr builder e scope in *)





      in
      (* Todo: Recursive lookup for complex data types*)
      (* let lookup n scopes = lookup_helper n (lookup_scope n scopes) *)
      (* in *)

    (* Construct code for an expression; return its value *)

    let rec expr builder ((t, e) : sexpr) scope  = match e with
        SNoexpr     -> L.const_int i32_t 0
      | SIntlit i   -> L.const_int i32_t i
      | SCharlit c  -> L.const_int i8_t c
      | SFloatlit f -> L.const_float float_t f
      | SId s       -> L.build_load (snd (lookup s t scope builder)) (U.final_id_of_rid s) builder
      | SBinassop (s, op, e) -> let e' =  expr builder e scope
                                  in ignore(L.build_store e' (snd (lookup s t scope builder)) builder); e'
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
        let e1' = expr builder e1 scope
        and e2' = expr builder e2 scope in
        (match op with
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mul     -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Mod     -> L.build_frem
          | A.Eq      -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Lt      -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Gt      -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | A.And | A.Or ->
              codegen_err "internal error: semant should have rejected and/or on float"
          ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
                let e1' = expr builder e1 scope
                and e2' = expr builder e2 scope in
                (match op with
                    A.Add       -> L.build_add
                    | A.Sub     -> L.build_sub
                    | A.Mul     -> L.build_mul
                    | A.Div     -> L.build_sdiv
                    | A.Mod     -> L.build_srem
                    | A.And     -> L.build_and
                    | A.Or      -> L.build_or
                    | A.Eq      -> L.build_icmp L.Icmp.Eq
                    | A.Neq     -> L.build_icmp L.Icmp.Ne
                    | A.Lt      -> L.build_icmp L.Icmp.Slt
                    | A.Leq     -> L.build_icmp L.Icmp.Sle
                    | A.Gt      -> L.build_icmp L.Icmp.Sgt
                    | A.Geq     -> L.build_icmp L.Icmp.Sge
                ) e1' e2' "tmp" builder
      | SUnop (op,  ((t, _) as e)) -> let e' = expr builder e scope in
                                            (match op with
                                                A.Minus when t = A.Float -> L.build_fneg
                                              | A.Minus                  -> L.build_neg
                                              | A.Not                  -> L.build_not) e' "tmp" builder
      | SStrlit s   ->
        L.build_call cnet_new_str_func [| L.build_global_stringptr s "tmp"
                                            builder |] "strlit" builder
      | SNew s      -> 
        let _, ll_strct = StringMap.find s cstructs in 
        L.build_malloc ll_strct "tmp" builder
      | SArrayLit (t, s, arr_lit) ->
        let size_t = expr builder (A.Int,SIntlit((size_of t))) scope in
        let arr_len = expr builder s scope in
        let ll_arr_lit = List.map (fun a -> expr builder a scope) arr_lit in
        let ll_va_args = arr_len :: size_t :: ll_arr_lit in
        L.build_call (init_array_func t) (Array.of_list ll_va_args) "cnet_init_array" builder
      (* | SIndex (r, s) -> *)
      (*   let s' = expr builder s scope in *)
      (*   let vd, ll = lookup r t scope builder in *)
      (*   let vd_ll = L.build_load ll (U.final_id_of_rid r) builder in *)
      (*   L.build_call (get_arr_index_func vd.vtyp) [|vd_ll; s'|] "cnet_arr_index" builder *)
      | SCall (n, args) ->
        let (fdef, fdecl) = find_checked n function_decls in
        let llargs = List.map (fun a -> expr builder a scope) args in
        let result = (match fdecl.styp with
                            A.Void -> ""
                          | _ -> n ^ "_result") in
              L.build_call fdef (Array.of_list llargs) result builder in
        let add_terminal builder instr =
          match L.block_terminator (L.insertion_block builder) with
            Some _ -> ()
          | None -> ignore (instr builder)

      


       (* LLVM insists each basic block end with exactly one "terminator"
          instruction that transfers control.  This function runs "instr builder"
          if the current block does not already have a terminator.  Used,
          e.g., to handle the "fall off the end of the function" case. *)

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

    (* LLVM insists each basic block end with exactly one "terminator"
         instruction that transfers control.  This function runs "instr builder"
         if the current block does not already have a terminator.  Used,
         e.g., to handle the "fall off the end of the function" case. *)
      let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder) in

    let add_var (vd, ll) scope = match scope with
        [] -> codegen_err "[COMPILER BUG] empty scope too far into pipeline"
      | hd :: tl -> (StringMap.add vd.vname (vd, ll) hd) :: tl
    in

    (* stmt : ((vdecl * L.llvalue StringMap.t), L.builder) -> sstmt ->
     *          vdecl * L.llvalue StringMap.t
     *)
    let rec stmt (scope,builder) the_stmt =
      let new_scope = StringMap.empty :: scope in

      match the_stmt with
        SExpr e -> ignore(expr builder e scope); (scope, builder)

      | SVdecl vd ->
        let new_var =
          L.build_alloca (ltype_of_typ vd.vtyp) vd.vname builder
        in
        (add_var (vd,new_var) scope), builder

      | SVdecl_ass (vd, (t, e)) ->
        let new_var = L.build_alloca (ltype_of_typ vd.vtyp) vd.vname builder
        in
        let new_scope = add_var (vd,new_var) scope in
        let the_assignment = vd.vtyp, SBinassop(A.FinalID(vd.vname), Assign, (t,e)) in
        ignore (expr builder the_assignment new_scope); (* do the assignment *)
        (new_scope, builder)

      | SDelete e -> let to_be_deleted = expr builder e scope in (match e with
            Struct(_), _ -> L.build_free to_be_deleted builder

          | _ -> L.build_call (find_func "cnet_free") [| to_be_deleted |] "tmp"
                   builder;
        ); scope, builder

      | SReturn e -> ignore (match fdecl.styp with
          (* Special "return nothing" instr *)
            A.Void -> L.build_ret_void builder
          (* Build return statement *)
          | _ -> L.build_ret (expr builder e scope) builder)
                   ; scope, builder

        (* do not attempt *)
      (* | SIf (psl, else_stmt) -> *)
      (*   let predicate_list = List.map (fun (p,_) -> expr builder p scope) psl in *)
      (*   let merge_bb = L.append_block context "merge" the_function in *)
      (*   let build_br_merge = L.build_br merge_bb in (1* partial function *1) *)

      (*   let add_ifthen (predicate, then_stmt) = *)
      (*     let pred = expr builder predicate scope in *)
      (*     let then_bb = L.append_block context "then" the_function in *)
      (*     (add_terminal (snd (stmt (scope, (L.builder_at_end context then_bb)) then_stmt)) build_br_merge)  :: l *)
      (*   in *)


      (*   let _ = List.fold_left add_ifthen [] psl in *)

      (*   let else_bb = L.append_block context "else" the_function in *)
      (*   add_terminal (stmt (L.builder_at_end context else_bb) else_stmt) *)
      (*     build_br_merge *)


      | SWhile (pred, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        let builder' = snd (stmt (scope, (L.builder_at_end context body_bb)) body) in
        add_terminal builder' (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let pred_val = expr pred_builder pred scope  in
        (* cast the value to a char (1 byte) *)
        let pred_val = L.build_icmp L.Icmp.Ne pred_val (L.const_int i32_t 0) "tmp" builder in


        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br pred_val body_bb merge_bb pred_builder);
        (new_scope, L.builder_at_end context merge_bb)

      | SBlock(sl) ->
        List.fold_left stmt (new_scope, builder) sl


      | _ -> codegen_err "unimplemented statement type"
    in
    List.fold_left stmt (func_scope, builder) fdecl.sbody; () in
  List.iter build_function_body fdecls; the_module;
