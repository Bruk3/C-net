(* Semantic checking for the MicroC compiler *)

module U = Utils
open Ast
open Sast

module StringMap = Map.Make(String);;

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

(* Add other things that might be needed inside statements to this struct *)
(* to_free : list of names of string variables to get freed
 * in_loop : specifies whether the current context is a loop
 * *)
type stmt_params = {scp : vdecl StringMap.t list ; fl : string list list; il : bool};;


let check  = function
    Program(all_decls: decl list) ->
    let is_function = function
        Fdecl(_) -> true
      | _ -> false in
    let func_decl_list = List.filter is_function all_decls in

    let to_ast_func = function
        Fdecl(func) -> func
      | _ -> semant_err "illegal type passed to_ast_func " in

    let functions = List.map  to_ast_func func_decl_list in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if (lvaluet = rvaluet) then lvaluet
      else semant_err err
    in
    let check_assign_builtin lvaluet rvaluet err =
      if (lvaluet = rvaluet) then
        lvaluet
      else
        match (lvaluet,rvaluet) with
        | (Array(Void), Array(x)) | (Array(x), Array(Void)) -> Array(x)
        | _ -> semant_err err
    in

    let builtin_vars =
    let add_builtinvar m vd = StringMap.add vd.vname vd m in
      List.fold_left add_builtinvar StringMap.empty
        [
          {vname="stdout"; vtyp=File};
          {vname="stdin"; vtyp=File}
        ]
    in

    (* The generic checkbinds function that takes the structs as an argument
     * checks the current variable declaration with all the one's it already has
     * in the following steps
     * 1) Checks for duplicates within the current scope
     * 2) Checks the validity of the declaration
     *  i) not void
     *  ii) if its a struct, it should be a valid struct
     * If all is well, it returns the the scope updated with the new variable
     * *)
    let rec check_binds_general
        ((scope : vdecl StringMap.t), (structs : strct StringMap.t)) (v : vdecl)
      : vdecl StringMap.t * strct StringMap.t
      =
      let _ = match StringMap.mem v.vname builtin_vars with
          true -> semant_err (v.vname ^ " cannot be defined as a variable")
        | false -> ()
      in
      let valid_struct (sname : string) = match StringMap.mem sname structs with
          true -> ()
        | false -> semant_err (v.vname ^ " has unrecognized struct type [struct " ^ sname ^ "]")
      in

      let _ = match v.vtyp with (* validate non-void / valid struct *)
          Void -> semant_err (v.vname ^ " is a void type, which is illegal")
        | Struct(s) -> valid_struct s
        | Array(t) ->
          ignore (check_binds_general (scope, structs) {vtyp = t; vname = v.vname ^ "[0]"}); ()
        | _ -> ()
      in

      let _ = match StringMap.mem v.vname scope with (* check no duplicates in scope *)
          true -> semant_err ("duplicate " ^ v.vname)
        | false -> ()
      in

      (StringMap.add v.vname v scope) , structs

    in

    (* add the structs of a program to a StringMap and verify that they are
     * valid declarations *)
    let structs : strct StringMap.t =
      let add_struct m = function
          Sdecl(s) ->
          (match StringMap.mem s.sname m with
             true -> semant_err ("Duplicate declaration of struct " ^ s.sname)
           | false ->
             let structs_so_far =
               StringMap.add s.sname s m (* include the current one *)
             in
             ignore (List.fold_left check_binds_general
               (StringMap.empty,structs_so_far) s.members); structs_so_far)
        | _ -> m
      in
      List.fold_left add_struct StringMap.empty all_decls
    in

    (* The specific check binds that already has the structs and takes one scope
     * (the 'top' one)
     *)
    let check_binds scope v =
      fst (check_binds_general (scope, structs) v)
    in

    (* the check_binds that takes a full scope and checks for conflicts in the
     * top one
     *)
    let check_binds_scoped full_scope v
      : vdecl StringMap.t list
      =
      match full_scope with
        [] -> semant_err ("[COMPILER BUG] empty scope passed to check_binds_scoped for variable search " ^ v.vname)
      | scope :: tl -> (check_binds scope v) :: tl
    in


    (* Collect global variables and check their validity *)
    let globals =
      let add_global m = function
          GVdecl(vd) | GVdecl_ass(vd, _) -> check_binds m vd
        | Sdecl(_) | Fdecl(_) -> m
      in
      List.fold_left add_global builtin_vars all_decls
    in
        (* TODO: catch builtin decls *)


(******************************************************************************
                               Built-in functions
*******************************************************************************)
    (* @built_in_decls  : this is a string -> fdecl map. However, since there can
     *                    be multiple built-in functions with different names
     * @builtin_funcs_l : this is a list of all the builtin declarations. This
     *                    is required because the map cannot have two entries of
     *                    the same name, but some functions such as length are
     *                    defined for multiple objects.
     *)
    let built_in_decls = U.builtin_funcs in
    let builtin_funcs_l = U.builtin_funcs_l in

      (* Add function name to symbol table *)
      let add_func map (fd: func) =
        let built_in_err = "function " ^ fd.name ^ " may not be defined"
        and dup_err = "duplicate function " ^ fd.name
        and n = if fd.name = "main" then fd.name else "user_" ^ fd.name (* Name of the function prefixed with user_ *)
        in match fd with (* No duplicate functions or redefinitions of built-ins *)
          _ when StringMap.mem n built_in_decls -> semant_err built_in_err
        | _ when StringMap.mem n map -> semant_err dup_err
        | _ ->  StringMap.add n fd map
      in

      (* Collect all function names into one symbol table *)
      let function_decls = List.fold_left add_func built_in_decls functions
      in

      (* Return a function from our symbol table *)
      let find_func (s : string) =
        try StringMap.find s function_decls
        with Not_found -> semant_err ("unrecognized function " ^ remove_prefix s "user_")
      in

      (* return a builtin function matched on its name and first parameter *)
      let find_builtin_func name paramt =
        let match_fun = function
            {t=_; name=n; parameters=f :: _; body=_; locals=_} ->
            (if n = name && (fst f = paramt || fst f = Array(Void)) then true else false)
          | _ -> false
        in
        match List.filter match_fun builtin_funcs_l with
          hd :: [] -> hd
        | _ -> semant_err ("find_builtin_func asked for " ^ name ^ " that doesn't exist")
      in

      (* Ensure "main" is defined and has the correct prototype*)
      (* Allowed prototypes for main
        int main()
        int main(string[])
      *)
      let _ = (* check main *)
        let {t=t; name=_ ;body=_ ;locals=_ ; parameters=params} = try
            find_func "main"
          with _ -> semant_err "main function not found"
        in
        let _ = match t with (* check return value *)
            Int -> ()
          | _ -> semant_err  "return type of main must be int."
        in
        match params with
          [] -> ()
        | [(Array(String), _)] -> ()
        | _ -> semant_err "Invalid prototype of main function."
      in

      let check_function func =
        (* Make sure no formals or locals are void or duplicates *)
        (* check_binds "formal" func.formals;
           check_binds "local" func.locals; *)

        (* helper function for finding a variable in either the current scope or
         * all the scope's that include this one
         *)
        let rec find_var  (vname : string) (scope : vdecl StringMap.t list) : vdecl = match scope with
            [] -> semant_err ("undeclared identifier " ^ vname)
          | m :: tl ->
            if StringMap.mem vname m then
              let vd = StringMap.find vname m in vd
            else
              find_var vname tl
        in
        (* recursively verify an rid to be valid *)
        let rec type_of_identifier (scope : vdecl StringMap.t list) = function
            FinalID s -> let the_var = find_var s scope in (the_var.vtyp, SFinalID(s))

          | RID(r, member) ->
            let the_struct, sid = type_of_identifier scope r
            in (match the_struct with
              Struct(sname) ->
              (try
                 let the_struct = StringMap.find sname structs in
                 match List.filter (fun t -> t.vname = member) the_struct.members with
                   m :: [] -> m.vtyp, SRID(sid, member)
                 | [] -> semant_err ("struct " ^ sname ^ " has no member " ^ member)
                 | _ -> semant_err ("[COMPILER BUG] struct " ^ sname ^ " contains multiple members called " ^ member)
               with Not_found -> semant_err ("[COMPILER BUG] variable of type struct " ^ sname ^
                                             " allowed without the the struct begin declared"));
            | t -> semant_err ("dot operator not allowed on variable " ^
                               string_of_rid r ^ " of type " ^ string_of_typ t))

          | Index(r, e) -> (* TODO: index into a string should be char *)
            let (t, e') = expr scope e in
            match t with
              Int ->
              (let vt, iid = type_of_identifier scope r in
               match vt with
                 Array(at) -> at, SIndex(iid, (t, e'))
               | String -> Char, SIndex(iid, (t, e'))
               | _ -> semant_err ("cannot index non-array variable " ^
                                  (string_of_rid r)))
            | ot -> semant_err ("index into an array has to be of type int, " ^
                    "but the expression (" ^ (string_of_expr e) ^ ") has type " ^
                    (string_of_typ ot))




        (* in *)
(* Return a semantically-checked expression, i.e., with a type *)

        and expr (scope : vdecl StringMap.t list) = function
            Charlit l -> (Char, SCharlit l)
          | Intlit l -> (Int, SIntlit l)
          | Floatlit l -> (Float, SFloatlit l)
          | Strlit l -> (String, SStrlit l)
          | Noexpr     -> (Void, SNoexpr)
          | Rid rid      ->
            (match rid with
              FinalID(n) when n = "stdin" -> File, SId(SFinalID("cnet_stdin"))
            | FinalID(n) when n = "stdout" -> File, SId(SFinalID("cnet_stdout"))
            | _ -> let t, id = type_of_identifier scope rid in t, SId(id))
          | Binassop (var, op, e) as ex -> (match op with
                PlusEq -> expr scope (Binassop(var, Assign, Binop(Rid(var), Add, e)))
              | MinusEq -> expr scope (Binassop(var, Assign, Binop(Rid(var), Sub, e)))
              | Assign -> let lt, lid = type_of_identifier scope var
                and (rt, e') = expr scope e in
                let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                          string_of_typ rt ^ " in " ^ string_of_expr ex
                in check_assign lt rt err, SBinassop(lid, Assign, (rt, e')))
          | Unop(op, e) as ex ->
            let (t, e') = expr scope e in
            let ty = match op with
                (Minus | Not) when t = Int || t = Float -> t
              | _ -> semant_err ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex)
            in (ty, SUnop(op, (t, e')))
          | Binop(e1, op, e2) as e ->
            let (t1, e1') = expr scope e1
            and (t2, e2') = expr scope e2 in
            (* All binary operators require operands of the same type *)
            let same = t1 = t2 in
            (* Determine expression type based on operator and operand types *)
            let ty = match op with
                Add | Sub | Mul | Div when same && t1 = Int   -> Int
              | Add | Sub | Mul | Div when same && t1 = Float -> Float
              | Add when same && t1 = String -> String
              | Mul when (t1=Int && t2=String) || (t1=String && t2=Int) -> String
              (* | Add | Sub when t1 = Int && t2 = Char -> Int *)
              (* | Add | Sub when t1 = Char && t2 = Int -> Float *)
              | Eq | Neq            when same               -> Int
              | Lt | Leq | Gt | Geq when same && (t1 = Int || t1 = Float) -> Int
              | And | Or when same && t1 = Int -> Int
              | _ -> semant_err ("illegal binary operator " ^
                                 string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                 string_of_typ t2 ^ " in " ^ string_of_expr e)
            in (ty, SBinop((t1, e1'), op, (t2, e2')))
          | Call(fname, args) as call ->

            let fname = (match fname with
                FinalID(f_name) -> FinalID("user_" ^ f_name)
                | _ -> fname

            ) in

            let isbuiltin = StringMap.mem (U.final_id_of_rid fname) built_in_decls in
            let args = (match fname with
                  FinalID(_) -> args
                | RID(sm,_) when isbuiltin -> Rid(sm) :: args
                | RID(_,r) -> semant_err ("function call on member " ^ r ^ " is not a builtin function" )
                | indx -> semant_err ("cannot call a function on index " ^ (string_of_rid indx))
              )
            in

            let fd  = if isbuiltin then
                match args with
                  f :: _  -> find_builtin_func (U.final_id_of_rid fname) (fst (expr scope f))
                | _ -> semant_err "built-in function called with no arguments"
              else
                (find_func (U.final_id_of_rid fname))

            in

            let check_call fd args =
              let param_length = List.length fd.parameters in
              if List.length args != param_length then
                let expected = if isbuiltin then param_length - 1 else param_length in
                semant_err ("expecting " ^ string_of_int expected ^ " arguments in " ^ string_of_expr call)
              else begin
                let check_call_helper (ft, _) e =
                  let (et, e') = expr scope e in
                  let err = "illegal argument found in call to " ^ fd.name ^ " : " ^ "found " ^
                            string_of_typ et ^ " but expected " ^ string_of_typ ft ^ " in " ^
                            string_of_expr e
                  in ((if isbuiltin then check_assign_builtin else check_assign) ft et err, e')
                in List.map2 check_call_helper fd.parameters args
              end
            in
            let args' = check_call fd args in
            (fd.t, SCall(U.final_id_of_rid fname, args'))
          | New(NStruct(sn)) ->
              let ty =  try (ignore (StringMap.find sn structs)) ; Struct(sn) with
                Not_found -> semant_err("invalid new expression: type [struct " ^ sn ^ "] doesn't exist")
              in (ty, SNew(sn))

          | ArrayLit(t, e, e_l) ->
              let check_int =
                let err = "illegal expression found: new " ^ string_of_typ t ^ "[" ^ string_of_expr e ^
                 "] Expression " ^ string_of_expr e ^ " should be of type int"
                in
                let (t', se) = expr scope e in match t' with
                  Int -> (t', se)
                  | _ -> semant_err(err)
              in
              let check_expr_list e_l =
                let err t1 t2 = "illegal expression found in Array literal. Array of type " ^
                  string_of_typ t1 ^ " can not contain element of type " ^ string_of_typ t2
                  in
                let sx_list = List.map (fun e -> expr scope e) e_l in
                let rec invalid_exists = function
                  [] -> sx_list
                  | (ti, _) :: tail -> if ti = t then invalid_exists tail else semant_err(err t ti)
                  in invalid_exists sx_list
            in
              (Array(t), SArrayLit(t, check_int, (check_expr_list e_l)))
          (* | _ -> semant_err "Expression not yet implemented" *)
        in

        let check_bool_expr scope e =
          let (t', e') = expr scope e
          and err = "expected integer expression in " ^ (string_of_expr e)
          in (if t' != Int then semant_err err else (t', e'))

        in
        let string_flatten exp =
          let pres, e, fres = U.handle_strings exp in match pres with
            [] -> SExpr(e)
          | _ -> SBlock( pres @ [SExpr(e)] @ fres)
        in


          (* Take the current statement and the current scope.  Returns the new
           * statement and the new scope appropriately.
           * sp : contains the current scope, information about which variables
           * need to be freed and whether the current context is a loop or not
          *)
        let rec check_stmt (sp : stmt_params) (aexp : stmt)
          : (sstmt * stmt_params)
          =
          let {scp = scope; fl = tofree; il = inloop} = sp in
          let new_scope = {scp = StringMap.empty :: scope ; fl = [] :: tofree; il = inloop} in
          let new_loop_scope = {scp = StringMap.empty :: scope; fl = [] :: tofree; il = true} in
          let mkblock = function Block s -> Block s | s -> Block [s] in
          let add_free vd = match tofree with
              [] -> semant_err "[COMPILER BUG] empty list passed to free list"
            | hd :: tl -> match vd.vtyp with
                String -> { scp = check_binds_scoped scope vd;
                            fl = (vd.vname :: hd) :: tl; il = inloop}
              | _ -> {scp = check_binds_scoped scope vd; fl = tofree; il = inloop}
          in
          let insert_frees {scp=tscp; fl=freelist; il=til} =
            let insert_free vname = SDelete (String, SId(SFinalID(vname))) in
            match freelist with
              [] -> [], {scp=tscp; fl=[]; il=til}
                    (* semant_err "[COMPILER BUG] empty list passed to insert_frees"*)
            | hd :: tl -> (List.map insert_free hd), {scp=tscp; fl=tl; il=til}
          in

          match aexp with
            Expr e ->
            let exp = expr scope e in
            string_flatten exp, sp

          | Delete n ->
            let t, _ = type_of_identifier scope n in
            let err = "illegal identifier for delete: [" ^ string_of_typ t ^ " " ^ string_of_rid n ^
                      "]. Identifier should be of type Struct or Array" in
            let e = Rid(n) in
            let check_valid_delete =  function
                Array(_) | Struct(_) | File | Socket -> SDelete (expr scope e), sp (* Delete should only be called on arrays and structs *)
              | _ -> semant_err (err)
            in check_valid_delete t

          | Break when inloop -> SBreak, {scp = scope; fl = tofree; il = false}
          | Break -> semant_err ("break used without being in a loop")
          | Continue when inloop -> SContinue, {scp = scope; fl = tofree; il = true}
          | Continue -> semant_err ("continue used without being in a loop")

          | If(e_s_l, s) ->
            let sif_of_if (pre_l, fre_l, e_s_l) (e_i,s_i) =
              let e' = check_bool_expr scope e_i in
              let pres, e'', fres = U.handle_strings e' in
              let s_i' = fst (check_stmt new_scope s_i) in
              (pre_l @ pres, fre_l @ fres, (e'' , s_i') :: e_s_l)
            in
            let e_s_l' = (Intlit(1), s) :: e_s_l in
            let (pres, frees, e_s_l'') =
              List.fold_left sif_of_if ([], [], []) e_s_l'
            in
            SBlock(pres @ [SIf(e_s_l'')] @ frees), sp

          | For(e1, e2, e3, st) ->
            let for_blk = Block [Expr(e1); While(e2, Block([st; Expr(e3)]))] in
            check_stmt sp for_blk
            (* SFor(expr scope e1, check_bool_expr scope e2, expr scope e3, fst *)
            (*        (check_stmt new_loop_scope (mkblock st))), sp *)

          | While(p, s) ->
            let p' = check_bool_expr scope p in
            let pres, p'', fres = U.handle_strings p' in
            let s' = match fst (check_stmt new_loop_scope (mkblock s)) with
              SBlock s -> SBlock (s @ (U.strip_decls pres))
              | s -> SBlock ([s] @ (U.strip_decls pres)) (* add the computations to the end of the while *)
            in
            SBlock(pres @ [SWhile(p'', s')] @ fres), sp

          (* add variable to highest scope *)
          | Vdecl (vd) -> SVdecl_ass(vd, U.default_global vd.vtyp), add_free vd

          | Vdecl_ass ({vtyp; vname}, e) ->
            let (e', newscp) = expr scope e , check_binds_scoped scope {vtyp; vname} in
            ignore (expr newscp (Binassop(FinalID(vname), Assign, e)))
            ;
            let pres, e'', fres = U.handle_strings e' in
            let the_decl = SVdecl_ass({vname=vname; vtyp=vtyp}, U.default_global vtyp) in
            let the_ass = SExpr(vtyp, match vtyp with
                String ->
                let the_cp =
                  (String, SCall("cnet_strcpy", [String, SId(SFinalID(vname)); e'']))
                in
                SBinassop(SFinalID(vname), Assign, the_cp)
              | _ -> SBinassop(SFinalID(vname), Assign, e'')

              ) in
            (* let the_assignment = SVdecl_ass({vname=vname; vtyp=vtyp}, e'') in *)
            SBlock(pres @ [the_decl; the_ass] @ fres) , add_free {vtyp; vname}

          | Return e -> let (t, e') = expr scope e in
            if t = func.t && t != String then SReturn (t, e'), sp
            else if t = func.t && t == String then
              let free_stmts, _ = insert_frees sp in
              SBlock ([ SBlock(free_stmts);
                        SVdecl({vtyp=String; vname="ret_tmp"});
                        string_flatten (String, SBinassop(SFinalID("ret_tmp"), Assign, (t, e')));
                        SReturn(String, SId(SFinalID("ret_tmp")))
                      ])
            , {scp=scope; fl=[]; il=inloop}
            else semant_err ("return statement in function "^ func.name ^" has type " ^ string_of_typ t ^
                             " but expected " ^ string_of_typ func.t ^ " in " ^ string_of_expr e)

          (* A block is correct if each statement is correct and nothing
             follows any Return statement.  Nested blocks are flattened. *)
          | Block(sl) ->
            let block_scope = new_scope in
            let check_stmt_list (* foldable *)
                ((sstmts_so_far : sstmt list), tmp_scope)
                (tmp_stmt : stmt)
              : (sstmt list) * stmt_params =
              let _ = match sstmts_so_far with
                  SReturn(_) :: _ -> semant_err "nothing may follow a return"
                | _ -> ()
              in
              let (sstatement, new_scope) =
                check_stmt tmp_scope tmp_stmt
              in
              (sstatement :: sstmts_so_far, new_scope)
              (* | Block sl :: ss  -> check_stmt_list (sl @ ss) (1* Flatten blocks *1) *)
              (* | s :: ss         -> fst (check_stmt s block_scope) :: check_stmt_list ss *)
              (* | []              -> [] *)
            in
            let (checked_block, old_sp) =
              (List.fold_left check_stmt_list ([], block_scope) sl)
            in
            (* (SBlock(List.rev checked_block) *)
            let free_stmts, _ = insert_frees old_sp in
            (match checked_block with
               SReturn(s) :: tl -> SBlock (List.rev (SReturn(s) :: (free_stmts @ tl)))
             | _ -> SBlock (List.rev (free_stmts @ checked_block))
            ), sp

        in (* body of check_function *)

        { styp = func.t;
          sfname = if func.name = "main" then func.name else "user_" ^ func.name;
          sparameters = func.parameters;
          sbody =
            (* add formals to scope first *)
            let init_params =
              {scp = List.fold_left check_binds_scoped [StringMap.empty; globals] (U.ids_to_vdecls func.parameters);
               fl = []; il = false;
              }
            in
            match check_stmt init_params (Block(func.body)) with
              (SBlock(sl), _) ->
              (match List.rev sl with (* check there is a return statement for the function *)
               (* _ -> sl) *)
                 SReturn(_) :: _ when func.t != Void -> sl
               | SBlock(x) :: _ when func.t != Void && (match List.rev x with
                     SReturn(_) :: _ -> true | _ -> false)-> sl
               | _ when func.t = Void -> sl
               | _  -> semant_err ("no return statement found for non-void function " ^ func.name))

            | _ -> semant_err "[COMPILER BUG] block didn't become a block?"
        }
      in    (* (globals, List.map check_function functions) *)


      let decl_to_sdecl = function
          GVdecl(vdecl) -> SGVdecl_ass(vdecl, U.default_global vdecl.vtyp)
        | GVdecl_ass(vd, e) -> let t, v = U.compute_global vd e in
          let err = "incompatible type assignment from " ^ string_of_typ vd.vtyp ^
                    " to " ^ string_of_typ t ^ " in globale variable " ^ vd.vname
          in
          ignore (check_assign t vd.vtyp err); SGVdecl_ass (vd, (t,v))
        (* TODO check assignment*)
        | Sdecl(s) -> SSdecl s
        | Fdecl (func) -> SFdecl (check_function func)

      in
      let sdcls = List.map decl_to_sdecl all_decls in
      sdcls
