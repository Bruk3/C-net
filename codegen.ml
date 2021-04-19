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

(*******************************************************************************
 *                                   Types
 *******************************************************************************)

  (* Get types from the context *)
  let i8_t       = L.i8_type     context (* Char *)
  and i32_t      = L.i32_type    context (* Int *)
  and float_t    = L.double_type context (* Float *)
  and void_t     = L.void_type   context in
  let str_t      = L.pointer_type i8_t in (*Need to fix*)
  let ptr_t t    = L.pointer_type t in


  (* Return the LLVM type for a cnet type *)
  let rec ltype_of_typ (t : A.typ) (cstrcts : L.lltype StringMap.t)
    : (L.lltype) = match t with
      A.Char            -> i8_t
    | A.Int             -> i32_t
    | A.Float           -> float_t
    | A.Void            -> void_t
    | A.String          -> str_t
    | A.Struct(n)       -> ptr_t (StringMap.find n cstrcts)
    | A.Array(typ)      -> ptr_t (ltype_of_typ typ cstrcts)
    | A.Socket          -> ptr_t (StringMap.find "cnet_socket" cstrcts)
    | A.File            -> ptr_t (StringMap.find "cnet_file" cstrcts)
  in

(*******************************************************************************
   *                            Declare all the structs
 *******************************************************************************)
  let cstructs : L.lltype StringMap.t =
    let declare_struct m (s : strct) =
      let cur_strct = L.named_struct_type context s.sname in
      let m = StringMap.add s.sname cur_strct m in
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
      StringMap.fold (fun _ s m -> declare_struct m s) U.builtin_structs StringMap.empty
    in
    List.fold_left declare_struct cbuiltinstrcts sdecls
  in

  let ltype_of_typ t = ltype_of_typ t cstructs in

  let cbuiltin_vars =
    let declare_struct_var {vtyp=vt; vname=vn} =
      let the_v  = (L.declare_global (ltype_of_typ vt) vn the_module) in
      L.set_externally_initialized true the_v; the_v
    in
    StringMap.map declare_struct_var U.builtin_vars
  in


  (* (1* Create a map of global variables after creating each 1 *)
  let global_decls : L.llvalue StringMap.t =
    let global_vdecl m {vtyp=typ;vname=name} =
      let init = match typ with
          A.Float -> L.const_float (ltype_of_typ typ) 0.0
        | _ -> L.const_int (ltype_of_typ typ) 0
      in StringMap.add name (L.define_global name init the_module) m in
     List.fold_left global_vdecl cbuiltin_vars vdecls in


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
      let local_vars =
        let add_formal m (t, n) p = 
          L.set_value_name n p;
          let local = L.build_alloca (ltype_of_typ t) n builder in
                ignore (L.build_store p local builder);
          StringMap.add n local m 

        and add_local m (t, n) =
          let local_var = L.build_alloca (ltype_of_typ t) n builder
          in StringMap.add n local_var m 
      
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sparameters
            (Array.to_list (L.params the_function)) 
        (* in *)
      (* List.fold_left add_local formals fdecl.sparameters *)
      in 
      let func_scope = local_vars :: [global_decls]
      in
      (* Return the value for a variable or formal argument.
        Check local names first, then global names *)
      let rec lookup_scope n scope =  match n with
                    FinalID s ->  if ((try StringMap.find s (List.hd scope) with Not_found -> None) != None) 
                                  then List.hd scope
                                  else lookup_scope n (List.tl scope)
                    | RID(r, member) -> lookup_scope r scope 
                    | Index(r,e)     -> lookup_scope r scope                      
      in
      let rec lookup_helper n curr_scope = match n with
          FinalID s -> StringMap.find s curr_scope
          | RID(r, member) -> lookup_helper r curr_scope  (*This is wrong, need to fix*)
          | Index(r,e)     -> lookup_helper r curr_scope   (*This is wrong, need to fix*)
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
      let lookup n scopes = lookup_helper n (lookup_scope n scopes) 
      in

    (* Construct code for an expression; return its value *)

    let rec expr builder ((_, e) : sexpr) = match e with
        SNoexpr     -> L.const_int i32_t 0  
      | SIntlit i   -> L.const_int i32_t i
      | SCharlit c  -> L.const_int i8_t c
      | SId s       -> L.build_load (lookup s func_scope) s builder
      | SBinassop (s, op, e) -> let e' =  ( match op with
                                      Assign -> expr builder e
                                    | _      -> expr builder e (*Not yet implemented*))
                                  in ignore(L.build_store e' (lookup s func_scope) builder); e'
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
