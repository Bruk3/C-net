(* Semantic checking for the MicroC compiler *)

module U = Utils
open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

(* let check (globals, functions) = *)

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

    (* The generic checkbinds function that takes the structs as an argument
     * checks the current variable declaration with all the one's it already has
     * in the following steps
     * 1) Checks for duplicates within the current scope
     * 2) Checks the validity of the declaration
     *  i) not void
     *  ii) if its a struct, it should be a valid struct
     * If all is well, it returns the the scope updated with the new variable
     * *)
    let check_binds_general
        ((full_scope : vdecl StringMap.t list), (structs : strct StringMap.t)) (v : vdecl)
      : vdecl StringMap.t list * strct StringMap.t
      =
      match full_scope with
        [] -> semant_err ("[COMPILER BUG] empty scope passed to check_binds_scoped for variable search " ^ v.vname)
      | scope :: tl ->
        let valid_struct (sname : string) = match StringMap.mem sname structs with
            true -> ()
          | false -> semant_err ("unrecognized struct type [struct " ^ sname ^ "]")
        in

        let _ = match v.vtyp with (* validate non-void / valid struct *)
            Void -> semant_err ("illegal void " ^ v.vname)
          | Struct(s) -> valid_struct s
          | _ -> ()
        in

        let _ = match StringMap.mem v.vname scope with (* check no duplicates in scope *)
            true -> semant_err ("duplicate " ^ v.vname)
          | false -> ()
        in

        (StringMap.add v.vname v scope) :: tl, structs

    in

    (* add the structs of a program to a StringMap and verify that they are
     * valid declarations *)
    let structs : strct StringMap.t =
      let add_struct m = function
          Sdecl(s) ->
          (match StringMap.mem s.name m with
             true -> semant_err ("Duplicate declaration of struct " ^ s.name)
           | false ->
             let structs_so_far =
               StringMap.add s.name s m (* include the current one *)
             in
             ignore (List.fold_left check_binds_general
               ([StringMap.empty],structs_so_far) s.members); structs_so_far)
        | _ -> m
      in
      List.fold_left add_struct StringMap.empty all_decls
    in

    (* The specific check binds that already has the structs *)
    let check_binds_scoped scope vd =
      fst (check_binds_general (scope,structs) vd)
    in


    (**** Check global variables ****)
    (* check_binds "global variable" globals; *)
    (* wrapper function for checking struct members. This is needed because a
     * struct can potentially include other structs or itself as a member *)
    (* let check_struct_binds (s : strct) (m : strct StringMap.t)= *)
    (*   let valid_member = function *)
    (*       {vtyp = Struct(sname); vname = _} -> ( *)
    (*         match StringMap.mem sname m with *)
    (*           true -> () | false -> *)
    (*           semant_err (Printf.sprintf "unrecognized member struct %s in struct %s" sname s.name) *)
    (*       ) *)
    (*     | _ -> () *)
    (*   in *)
    (*   check_binds "struct member" s.members; (1* first check normal conditions *1) *)
    (*   List.iter valid_member s.members (1* then check valid struct-typed members *1) *)
    (* in *)






    (* Collect function declarations for built-in functions: no bodies *)
    let built_in_decls =
      let add_bind map (return_type, name, params) = StringMap.add name {
          t = return_type;
          name = name;
          parameters = params;
          locals = [];
          body = [] } map
        in List.fold_left add_bind StringMap.empty
          [
            (Int, "println", [(String, "s")])
          ]
      in

      (* Add function name to symbol table *)
      let add_func map (fd: func) =
        let built_in_err = "function " ^ fd.name ^ " may not be defined"
        and dup_err = "duplicate function " ^ fd.name
        and n = fd.name (* Name of the function *)
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
        with Not_found -> semant_err ("unrecognized function " ^ s)
      in

      (* Ensure "main" is defined and has the correct prototype*)
      (* Allowed prototypes for main
        int main()
        int main(string[])
      *)
      let check_main =
        let {t; parameters} = try
            find_func "main"
          with _ -> semant_err "main function not found"
        in
          let check_return = match t with
            Int -> true
          | _ -> semant_err  "return type of main must be int."
        in
          match parameters with
            [] -> ()
          | [(Array(String), _)] -> ()
          | _ -> semant_err "Invalid prototype of main function."

      in

      let check_function func =
        (* Make sure no formals or locals are void or duplicates *)
        (* check_binds "formal" func.formals;
           check_binds "local" func.locals; *)

        (* Raise an exception if the given rvalue type cannot be assigned to
           the given lvalue type *)
        let check_assign lvaluet rvaluet err =
          if (lvaluet = rvaluet) || (lvaluet = Int && rvaluet == Char) ||
               (lvaluet = Char && rvaluet == Int) then lvaluet
          else semant_err err
        in

        (* Build local symbol table of variables for this function *)
        (* let symbols = List.fold_left (fun m ((ty, name)) -> StringMap.add name ty m) *)
        (*     StringMap.empty (func.parameters @ func.locals) (1* Should be globals @ func.parameters *1) *)
        (* in *)

        let rec verify_decl = function
            {vtyp = Struct(sn); vname = n} -> (try ignore (StringMap.find sn structs)
                                               with Not_found -> semant_err (n ^ " is of type [struct " ^ sn ^
                                                                            "] which doesn't exist"))
          | {vtyp = Void; vname = n} -> semant_err (n ^ " is a void type, which is illegal")
          | {vtyp = Array(t); vname = n} -> verify_decl {vtyp = t; vname = n ^ "[0]"}
          | _ -> () (* Char | Float | Int | String | Socket | File are all fine *)


        in


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
            FinalID s -> let the_var = find_var s scope in the_var.vtyp

          | RID(r, member) ->
            let the_struct = type_of_identifier scope r
            in (match the_struct with
              Struct(sname) ->
              (try
                 let the_struct = StringMap.find sname structs in
                 match List.filter (fun t -> t.vname = member) the_struct.members with
                   m :: [] -> m.vtyp
                 | [] -> semant_err ("struct " ^ sname ^ " has no member " ^ member)
                 | _ -> semant_err ("[COMPILER BUG] struct " ^ sname ^ " contains multiple members called " ^ member)
               with Not_found -> semant_err ("[COMPILER BUG] variable of type struct " ^ sname ^
                                             " allowed without the the struct begin declared"));
            | t -> semant_err ("dot operator not allowed on variable " ^
                               string_of_rid r ^ " of type " ^ string_of_typ t))

          | Index(r, e) ->
            let (t, _) = expr scope e in
            match t with
              Int ->
              (let vt = type_of_identifier scope r in
               match vt with
                 Array(at) -> at
               | _ -> semant_err ("cannot index non-array variable" ^
                                  (string_of_rid r)))
            | ot -> semant_err ("index into an array has to be of type int, " ^
                    "but the expression (" ^ (string_of_expr e) ^ ") has type " ^
                    (string_of_typ ot))




        (* in *)
(* Return a semantically-checked expression, i.e., with a type *)

        and expr (scope : vdecl StringMap.t list) = function
            Charlit l -> (Int, SCharlit l)
          | Intlit l -> (Int, SIntlit l)
          | Floatlit l -> (Float, SFloatlit l)
          | Strlit l -> (String, SStrlit l)
          | Noexpr     -> (Void, SNoexpr)
          | Rid rid      -> (type_of_identifier scope rid), SId (rid)
          | Binassop (var, op, e) as ex ->
            let lt = type_of_identifier scope var  (* TODO: Kidus: why doesn't this
                                               catch illegal assignments? *)
            and (rt, e') = expr scope e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                      string_of_typ rt ^ " in " ^ string_of_expr ex
            in (check_assign lt rt err, SBinassop((string_of_rid var), op, (rt, e')))
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
              | Add | Sub when t1 = Int && t2 = Char -> Int
              | Add | Sub when t1 = Char && t2 = Int -> Float
              | Eq | Neq            when same               -> Int
              | Lt | Leq | Gt | Geq
                when same && (t1 = Int || t1 = Float) -> Int
              | And | Or when same && t1 = Int -> Int
              | _ -> semant_err ("illegal binary operator " ^
                                 string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                 string_of_typ t2 ^ " in " ^ string_of_expr e)
            in (ty, SBinop((t1, e1'), op, (t2, e2')))
          | Call(fname, args) as call ->
            let fd = find_func (U.final_id_of_rid fname) in
            let param_length = List.length fd.parameters in
            if List.length args != param_length then
              semant_err ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call)
            else let check_call (ft, _) e =
                   let (et, e') = expr scope e in
                   let err = "illegal argument found " ^ string_of_typ et ^
                             " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
                   in (check_assign ft et err, e')
              in
              let args' = List.map2 check_call fd.parameters args
              in (fd.t, SCall(fname, args'))
          | New(NStruct(sn)) ->
              let ty =  try (ignore (StringMap.find sn structs)) ; Struct(sn) with
                Not_found -> semant_err("invalid new expression: type [struct " ^ sn ^ "] doesn't exist")
              in (ty, SNew(NStruct(sn)))

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


        (* a small helper function that adds a new scope to the one it is
         * passed. This is used when entering a function, if/else statement
         * blocks, loops and blocks i
         *)
        let new_scope (scope : vdecl StringMap.t list) =
          StringMap.empty :: scope
        in

        (* Take the current statement and the current scope.
         * Returns the new statement and the new scope appropriately.
        *)
        let rec check_stmt (scope : vdecl StringMap.t list) (aexp : stmt)
          : (sstmt * vdecl StringMap.t list)
          = match aexp with
            Expr e -> SExpr(expr scope e), scope
          | Delete n ->
                let t = type_of_identifier scope n in
                let err = "illegal identifier for delete: [" ^ string_of_typ t ^ " " ^ string_of_rid n ^
                "]. Identifier should be of type Struct or Array" in
                let e = Rid(n) in
                let check_valid_delete =  function
                    Array(_) | Struct(_) -> SDelete (expr scope e), scope
                  | _ -> semant_err (err) in check_valid_delete t
          | Break -> SBreak, scope (* TODO that we are in a loop context *)
          | Continue -> SContinue, scope (* TODO verify that we are in a loop context *)

          (* | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2) *)
          | If(e_s_l, s) ->
            let sif_of_if (e_i, s_i) =
              check_bool_expr scope e_i,
              (fst (check_stmt (new_scope scope) s_i))
            in
            SIf(List.rev (List.map sif_of_if e_s_l), fst (check_stmt (new_scope scope) s)), scope

          | For(e1, e2, e3, st) ->
            SFor(expr scope e1, check_bool_expr scope e2, expr scope e3, fst (check_stmt (new_scope scope) st)), scope

          | While(p, s) -> SWhile(check_bool_expr scope p, fst (check_stmt (new_scope scope) s)), scope

          | Vdecl (vd) -> verify_decl vd; SVdecl vd , (* add variable to highest scope *)
                                          check_binds_scoped scope vd

          | Vdecl_ass ({vtyp; vname}, e) -> verify_decl {vtyp; vname} ;
              let (d, newScope) = SVdecl_ass({vtyp; vname}, expr scope e) , check_binds_scoped scope {vtyp; vname} in
                      ignore (expr (newScope) (Binassop(FinalID(vname), Assign, e)) ) ; (d, newScope)
          | Return e -> let (t, e') = expr scope e in
            if t = func.t then SReturn (t, e'), scope
            else semant_err ("return gives " ^ string_of_typ t ^ " expected " ^
                             string_of_typ func.t ^ " in " ^ string_of_expr e)

          (* A block is correct if each statement is correct and nothing
             follows any Return statement.  Nested blocks are flattened. *)
          | Block(sl) ->
            let block_scope = new_scope scope in
            let check_stmt_list ((sstmts_so_far : sstmt list), tmp_scope) (tmp_stmt : stmt) : (sstmt list) * vdecl StringMap.t list =
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
            let (checked_block, _) =
              (List.fold_left check_stmt_list ([], block_scope) sl)
            in
            (SBlock(List.rev checked_block), scope)
          | _ -> semant_err "Statement not yet implemented"

        in (* body of check_function *)

        { styp = func.t;
          sname = func.name;
          sparameters = func.parameters;
          sbody =
            (* add formals to scope first *)
            let init_scope =
              List.fold_left check_binds_scoped [StringMap.empty] (U.ids_to_vdecls func.parameters)
            in
            match check_stmt init_scope (Block(func.body)) with
              (SBlock(sl), _) -> sl
            | _ -> semant_err "[COMPILER BUG] block didn't become a block?"
        }
      in    (* (globals, List.map check_function functions) *)

      let decl_to_sdecl = function
          GVdecl(vdecl) ->  SGVdecl_ass(vdecl, U.default_global vdecl.vtyp)
        | GVdecl_ass(vdecl, e) -> SGVdecl_ass (vdecl, U.compute_global vdecl e) (* TODO *)
        | Sdecl(s) -> SSdecl s
        | Fdecl (func) -> SFdecl (check_function func)

      in
      let sdcls = List.map decl_to_sdecl all_decls in
      sdcls
