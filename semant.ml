(* Semantic checking for the MicroC compiler *)

module U = Utils
open Ast
open Sast

module StringMap = Map.Make(String)

(* A semantic error in cnet. It contains a message and a line number *)

(* Kidus: The line number is not really implemented and is only there for
 * forward compatibility. Right now the function semant_err takes only a string,
 * but later it can take a line number as well
 * *)
exception SemanticError of string * int;;

(* The function for raising a semantic error *)
let semant_err (msg : string) =
  raise (SemanticError(msg, -1));;
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


    (* Verify a list of bindings has no void types or duplicate names *)
    (* kind can be global, struct member, formal or local *)
    (* TODO: check array types *)
    let check_binds (kind : string) (binds : vdecl list) =
      List.iter (function
            v when v.vtyp = Void  -> semant_err ("illegal void " ^ kind ^ " "
                                                 ^ v.vname)
          | _ -> ()) binds;
      let rec dups = function
          [] -> ()
        | v1 :: v2 :: _ when v1.vname = v2.vname ->
          semant_err ("duplicate " ^ kind ^ " " ^ v1.vname)
        | _ :: t -> dups t
      in dups (List.sort (fun v1 v2 -> compare v1.vname v2.vname) binds)
    in


    (**** Check global variables ****)

    (* check_binds "global variable" globals; *)

    (* wrapper function for checking struct members. This is needed because a
     * struct can potentially include other structs or itself as a member *)
    let check_struct_binds (s : strct) (m : strct StringMap.t)=
      let valid_member = function
          {vtyp = Struct(sname); vname = _} -> (
            match StringMap.mem sname m with
              true -> () | false ->
              semant_err (Printf.sprintf "unrecognized member struct %s in struct %s" sname s.name)
          )
        | _ -> ()
      in
      check_binds "struct member" s.members; (* first check normal conditions *)
      List.iter valid_member s.members (* then check valid struct-typed members *)

    in


    (* add the structs of the function to a StringMap and verify that they are
     * valid declarations *)

    let structs : strct StringMap.t =
      let add_struct m = function
          Sdecl(s) ->
          (match StringMap.mem s.name m with
             true -> semant_err ("Duplicate declaration of struct " ^ s.name)
           | false -> let structs_so_far = StringMap.add s.name s m in (* include the current one *)
             check_struct_binds s structs_so_far; structs_so_far)
        | _ -> m
      in
      List.fold_left add_struct StringMap.empty all_decls
    in

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

      (* Ensure "main" is defined *)
      let _ = try find_func "main"
        with _ -> semant_err "main function not found" in

      let check_function func =
        (* Make sure no formals or locals are void or duplicates *)
        (* check_binds "formal" func.formals;
           check_binds "local" func.locals; *)

        (* Raise an exception if the given rvalue type cannot be assigned to
           the given lvalue type *)
        let check_assign lvaluet rvaluet err =
          if lvaluet = rvaluet then lvaluet else semant_err err
        in

        (* Build local symbol table of variables for this function *)
        let symbols = List.fold_left (fun m ((ty, name)) -> StringMap.add name ty m)
            StringMap.empty (func.parameters @ func.locals) (* Should be globals @ func.parameters *)
        in

        let rec verify_decl = function
            {vtyp = Struct(sn); vname = n} -> (try ignore (StringMap.find sn structs)
                                               with Not_found -> semant_err (n ^ " is of type [struct " ^ sn ^
                                                                            "] which doesn't exist"))
          | {vtyp = Void; vname = n} -> semant_err (n ^ " is a void type, which is illegal")
          | {vtyp = Array(t); vname = n} -> verify_decl {vtyp = t; vname = n ^ "[0]"}
          | _ -> () (* Char | Float | Int | String | Socket | File are all fine *)


        in

        (* recursively verify an rid to be valid *)
        let rec type_of_identifier = function
            FinalID s ->
            (try StringMap.find s symbols
              with Not_found -> semant_err ("undeclared identifier " ^ s))

          | RID(r, member) ->
            let the_struct = type_of_identifier r
            in match the_struct with
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
                               string_of_rid r ^ " of type " ^ string_of_typ t)
        in
(* Return a semantically-checked expression, i.e., with a type *)
let rec expr = function
    Charlit l -> (Int, SCharlit l)
  | Intlit l -> (Int, SIntlit l)
          | Floatlit l -> (Float, SFloatlit l)
          | Strlit l -> (String, SStrlit l)
          | Noexpr     -> (Void, SNoexpr)
          | Rid rid      -> (type_of_identifier rid), SId (rid)
          | Binassop (var, op, e) as ex ->
            let lt = type_of_identifier var (* TODO: Kidus: why doesn't this
                                               catch illegal assignments? *)
            and (rt, e') = expr e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                      string_of_typ rt ^ " in " ^ string_of_expr ex
            in (check_assign lt rt err, SBinassop((string_of_rid var), op, (rt, e')))
          | Unop(op, e) as ex ->
            let (t, e') = expr e in
            let ty = match op with
                (Minus | Not) when t = Int || t = Float -> t
              | _ -> semant_err ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex)
            in (ty, SUnop(op, (t, e')))
          | Binop(e1, op, e2) as e ->
            let (t1, e1') = expr e1
            and (t2, e2') = expr e2 in
            (* All binary operators require operands of the same type *)
            let same = t1 = t2 in
            (* Determine expression type based on operator and operand types *)
            let ty = match op with
                Add | Sub | Mul | Div when same && t1 = Int   -> Int
              | Add | Sub | Mul | Div when same && t1 = Float -> Float
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
                   let (et, e') = expr e in
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
          | _ -> semant_err "Expression not yet implemented"
        in

        let check_bool_expr e =
          let (t', e') = expr e
          and err = "expected Boolean expression in " ^ string_of_expr e
          in if t' != Int then semant_err err else (t', e')
        in

        (* Return a semantically-checked statement i.e. containing sexprs *)
        let rec check_stmt = function
            Expr e -> SExpr (expr e)
          (* | Delete n -> SDelete (expr n) *)
          | Break -> SBreak
          | Continue -> SContinue
          (* | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2) *)
          | If(e_s_l, s) -> SIf(List.rev (List.map (fun(e_i, s_i) -> (check_bool_expr e_i, check_stmt s_i)) e_s_l), check_stmt s)
          | For(e1, e2, e3, st) ->
            SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
          | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
          | Vdecl (vd) -> verify_decl vd; SVdecl vd
          | Vdecl_ass (vd, e) -> verify_decl vd; SVdecl_ass(vd, expr e)

          | Return e -> let (t, e') = expr e in
            if t = func.t then SReturn (t, e')
            else semant_err ("return gives " ^ string_of_typ t ^ " expected " ^
                             string_of_typ func.t ^ " in " ^ string_of_expr e)

          (* A block is correct if each statement is correct and nothing
             follows any Return statement.  Nested blocks are flattened. *)
          | Block sl ->
            let rec check_stmt_list = function
                [Return _ as s] -> [check_stmt s]
              | Return _ :: _   -> semant_err "nothing may follow a return"
              | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
              | s :: ss         -> check_stmt s :: check_stmt_list ss
              | []              -> []
            in SBlock(check_stmt_list sl)
          | _ -> semant_err "Statement not yet implemented"

        in (* body of check_function *)
        { styp = func.t;
          sname = func.name;
          sparameters = func.parameters;
          sbody = match check_stmt (Block func.body) with
              SBlock(sl) -> sl
            | _ -> semant_err "[COMPILER BUG] block didn't become a block?"
        }
      in    (* (globals, List.map check_function functions) *)

      let decl_to_sdecl = function
          GVdecl(vdecl) ->  SGVdecl(vdecl)
        | GVdecl_ass(vdecl, _) -> SGVdecl_ass (vdecl, (Void, SNoexpr)) (* TODO *)
        | Sdecl(s) -> SSdecl s
        | Fdecl (func) -> SFdecl (check_function func)

      in
      let sdcls = List.map decl_to_sdecl all_decls in
      sdcls
