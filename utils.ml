module A = Ast
open Sast;;
open Ast;;

module StringMap = Map.Make(String);;

                              (* Scanner utils *)
let count_new_lines whitespace lexbuf =
  String.iter
    (fun c -> if c = '\n' then Lexing.new_line lexbuf else ()) whitespace;;

(* Get the current line number from the lexbuf *)
let line_num lexbuf = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum


                                (* SAST utils *)
(* Gets the FinalID part of a recursive id. For example, it extracts println
 * from my_struct.my_other_struct.my_file.println
 *)
let rec final_id_of_rid = function
  A.FinalID(fid) -> fid
  | A.RID(_, mem) -> mem
  | A.Index(rid, _) -> final_id_of_rid rid
;;

let rec final_id_of_sid = function
  SFinalID(fid) -> fid
  | SRID(_, mem) -> mem
  | SIndex(sid, _) -> final_id_of_sid sid
;;

(* Get a default value for a global variable based on its type *)
let default_global = function
    A.Char | A.Int -> (A.Int, SIntlit(0))
  | A.Float -> (A.Float, SFloatlit(0.0))
  | A.String -> (A.String, SStrlit(""))
  | A.Void   -> semant_err "[COMPILER BUG] uncaught void global variable detected"
  | t -> (t, SNoexpr)
;;

(* compute the value of a global variable assignment at compile time. Global
 * variables can only be assigned to constant expressions at declaration *)
let compute_global vdecl exp =
  let verify_types (t1 : A.typ) (t2 : A.typ) =
    if t1 = t2 then ()
     else (semant_err ("incompatible types " ^ A.string_of_typ t1 ^ " and " ^
           A.string_of_typ t2 ^ " in global variable " ^ vdecl.A.vname))
  in
  let bool_int b = if b then 1 else 0 in

  let rec eval_constant = function
      A.Noexpr | A.Binassop(_) | A.New(_) | A.ArrayLit(_) | A.Call(_) ->
      semant_err ("non-constant expression used for global variable " ^ vdecl.A.vname)
    | A.Rid(_) ->
      semant_err "global declaration using another global variable not implemented"

    | A.Intlit(i) -> (A.Int, SIntlit(i))
    | A.Charlit(c) -> (A.Int, SIntlit(c))
    | A.Floatlit(f) -> (A.Float, SFloatlit(f))
    | A.Strlit(s) -> (A.String, SStrlit(s))
    | A.Binop(e1, op, e2) -> let e1' = eval_constant e1 and e2' = eval_constant e2
      in verify_types (fst e1') (fst e2');
      (match (e1', e2') with
         ((A.Int, SIntlit(i)), (A.Int, SIntlit(i2))) ->
         (match op with
            A.Add -> (A.Int, SIntlit(i + i2))
          | A.Sub -> (A.Int, SIntlit(i - i2))
          | A.Mul -> (A.Int, SIntlit(i * i2))
          | A.Div -> (A.Int, SIntlit(i / i2))
          | A.Mod -> (A.Int, SIntlit(i mod i2))
          | A.And -> (A.Int, SIntlit(bool_int ((i land i2) != 0)))
          | A.Or  -> (A.Int, SIntlit(bool_int ((i lor i2) != 0))) (* TODO: these what we want? *)
          | A.Eq  -> (A.Int, SIntlit(bool_int (i = i2)))
          | A.Neq -> (A.Int, SIntlit(bool_int (i != i2)))
          | A.Lt  -> (A.Int, SIntlit(bool_int (i < i2)))
          | A.Gt  -> (A.Int, SIntlit(bool_int (i > i2)))
          | A.Geq -> (A.Int, SIntlit(bool_int (i >= i2)))
          | A.Leq -> (A.Int, SIntlit(bool_int (i <= i2)))
         )
         (* TODO *)
       | (A.Float, SFloatlit(_)), (A.Float, SFloatlit(_))
       | (A.Float, SFloatlit(_)), (A.Int, SIntlit(_)) (*float string operations *)
       | (A.Int, SIntlit(_)), (A.Float, SFloatlit(_)) (*float string operations *)
       | (A.Int, SIntlit(_)), (A.String, SStrlit(_)) (* string int operations *)
       | (A.String, SStrlit(_)), (A.Int, SIntlit(_)) (* string int operations *)
                                 -> semant_err "global expression type not implemented"
       | _ -> semant_err ("non-constant expression used for global variable " ^ vdecl.A.vname)
      )
    | A.Unop(op, e) ->
      let _, e' = eval_constant e in
      match e' with
        SIntlit(i) -> ( match op with
            A.Not -> (A.Int, SIntlit(bool_int (i == 0)))
          | A.Minus -> (A.Int, SIntlit( -1 * i ))
        )
      | _ -> semant_err ("operator [" ^ (A.string_of_uop op) ^ "] only valid for integers")

  in
  eval_constant exp ;;

let ids_to_vdecls (ids : A.id list)=
  let id_to_vdecl ((t, n) : A.id) =
    {A.vtyp = t; A.vname = n}
  in List.map id_to_vdecl ids;;

(* Goes through an expression and substitues operations on strings with the call
 * to the appropriate string library functions. It also creats temporary
 * variables for handling return values of things
 * *)
let handle_strings sexp : sstmt list * sexpr * sstmt list=
  let assign a b = SVdecl_ass({A.vtyp=String; A.vname = a}, b) in
  let styp = A.String in (* just for easier reading *)

  let rec handle_helper stmts cur_exp n = match cur_exp with

      (* (A.String as st, SCall(fn, args)) -> *)
      (* let cur_tmp = "tmp" ^ (string_of_int n) in *)
      (* assign cur_tmp (st, SCall(fn, args)) :: stmts, (st, SId(SFinalID(cur_tmp))), n + 1 *)

    (st, SCall(fn, args)) ->
      let foldable_helper (pres, es, n) exp =
        let cur_pres, cur_e, n' = handle_helper [] exp n in
        pres @ cur_pres, cur_e :: es, n'
      in
      let pres, es, n' = List.fold_left foldable_helper (stmts,[],n) args in
      (match st with
         String ->
         let cur_tmp = "tmp" ^ (string_of_int n') in
         assign cur_tmp (st, SCall(fn, List.rev es)) :: pres, (st, SId(SFinalID(cur_tmp))), n' + 1
       | _ -> pres, (st, SCall(fn, List.rev es)), n'
      )

    (* All binary assignments should have been converted to = in semant *)
    | (A.String, SBinassop(s1, _, s2)) -> let new_stmts, s2', n' = handle_helper stmts s2 n
      in new_stmts, (String, SBinassop(s1, Assign, (String, SCall("cnet_strcpy", [String, SId(s1); (s2')])))), n'

    | (fst_typ, SBinop((t1, e1), op, (t2, e2))) -> (match (t1, t2) with

          (String, String) ->
          (match op with
             Add -> (* "hello" + "there" *)
             let cs1, e1', n' = handle_helper stmts (t1, e1) n
             in
             let cs2, e2', n'' = handle_helper cs1 (t1,e2) n'
             in
             let cur_tmp = "tmp" ^ (string_of_int n'') in
             assign cur_tmp (String, SCall("cnet_strcat", [e1'; e2'])) :: cs2,
                (String, SId(SFinalID(cur_tmp))), n'' + 1
           | Eq ->
             let cs1, e1', n' = handle_helper stmts (t1, e1) n
             in
             let cs2, e2', n'' = handle_helper cs1 (t1,e2) n'
             in
             let cur_tmp = "tmp" ^ (string_of_int n'') in
             let cur_exp = (Int, SCall("cnet_strcmp", [e1'; e2'])) in
             let cur_ass = SVdecl_ass({vtyp=Int; vname = cur_tmp}, cur_exp) in
             cur_ass :: cs2,
             (Int, SId(SFinalID(cur_tmp))), n'' + 1
           | _ -> semant_err ("[COMPILER BUG] only + should be allowed on two strings (handle_strings)"))

        | (String, Int) | (Int, String) ->
          let the_str, the_int = (if t1 = String then e1, e2 else e2, e1) in
          (match op with
             Mul ->
             let cs1, the_str', n' = handle_helper stmts (String, the_str) n in
             let cur_tmp = "tmp" ^ (string_of_int n') in
             assign cur_tmp (String, SCall("cnet_strmult", [the_str'; Int, the_int ])) :: cs1 ,
             (String, SId(SFinalID(cur_tmp))), n' + 1
           | _ -> semant_err "[COMPILER BUG] only * should be allowed on string-int (hanlde_strings)")

        | _ -> [], (fst_typ, SBinop((t1, e1), op, (t2, e2))), n
      )

    |(A.String, x) -> stmts , (A.String, x), n
    | _ -> stmts, cur_exp, n
  in
  let pre_stmts, new_exp, _  = handle_helper [] sexp 1000 in
  let convert_to_free = function
      SVdecl_ass({vtyp=String; vname=vn}, _) -> SDelete(String, SId(SFinalID(vn)))
    | SVdecl_ass({vtyp=Int; vname=vn}, _) -> SExpr(Void, SNoexpr)
    | _ -> semant_err ("[COMPILER BUG] convert_to_free not setup properly")
  in
  let l = List.rev pre_stmts in
  let free_stmts = List.map convert_to_free l in
  l , new_exp , free_stmts
;;

let strip_decls dl =
  let strip_helper = function
    SVdecl_ass({vtyp=t; vname=vn}, exp) ->
    SExpr(t, SBinassop(SFinalID(vn), Assign, exp))
    | _ ->
      semant_err "[COMPILER BUG] strip_decls passed something other than a declaration_assign list"
  in
  List.map strip_helper dl
;;



(* the built-in variables in cnet that cannot be declared by users *)
let builtin_vars =
  let add_builtinvar m vd = StringMap.add vd.vname vd m in
  List.fold_left add_builtinvar StringMap.empty
    [
      {vname="stdout"; vtyp=File};
      {vname="stdin"; vtyp=File}
    ]
;;

(* the built-in functions in cnet that cannot be declared by users *)
let builtin_funcs, builtin_funcs_l =
  let add_bind (map, l) (return_type, name, params) =
    let f = { t = return_type; name = name; parameters = params; locals = []; body = [] }
    in
        StringMap.add name f map, f :: l
  in List.fold_left add_bind (StringMap.empty, [])
    [
      (* I/O *)
      (* Sockets *)
      (Socket, "user_nopen", [(String, "host"); (String, "protocol"); (Int, "port"); (String, "type")]);
      (Int, "writeln", [(Socket, "f"); (String, "s")]);

      (Int, "write", [(Socket, "sock"); (String, "s")]);
      (String, "readln", [(Socket, "sock")]);
      (String, "read", [(Socket, "sock"); (Int, "len")]);

      (* Files *)
      (File, "user_fopen", [(String, "name"); (String, "mode");]);
      (Int, "writeln", [(File, "f"); (String, "s")]);
      (Int, "write", [(File, "f"); (String, "s")]);
      (String, "readln", [(File, "f")]);
      (String, "read", [(File, "f"); (Int, "len")]);

      (* Strings *)
      (Int, "slength", [(String, "s")]);
      (Int, "use_toint", [(String, "s")]); (* string of int *)
      (String, "user_soi", [(Int, "i")]); (* string of int *)
      (String, "cnet_strcpy", [(String, "t"); (String, "s")]);
      (String, "cnet_strmult", [(String, "t"); (Int, "i")]);
      (String, "cnet_strcat", [(String, "t"); (String, "s")]);
      (Int, "cnet_strcmp", [(String, "t"); (String, "s")]);
      (String, "cnet_str_upper", [(String, "t")]);

      (* Arrays *)
      (Int, "alength", [((Array(Void)), "s")]);

      (* Cnet *)
      (Int, "cnet_free", [(String, "s")])
    ]
;;


(* sast version of built-in functions *)
let sbuiltin_funcs_l =
  List.map
    (fun {t=ty; name=n ; parameters=params; body=_; locals: _} ->
       {styp=ty; sfname=n; sparameters=params; sbody=[]}) builtin_funcs_l
;;

(* Codegen utils *)
(* Changes the format of an sast program, which is a list of sdecls, to one the
 * codegen can accept, which is a tuple of lists of vdecls, struct_decls and
 * fdecls
 *)
let decompose_program (sprog : sdecl list) =
  let helper (vdecls, strct_decls, fdecls) decl = match decl with
    | SGVdecl_ass (vd, v) -> ((vd, v) :: vdecls, strct_decls, fdecls) (* TODO: handle SGVdecl_ass properly *)
    | SSdecl(sd) -> (vdecls, sd :: strct_decls, fdecls)
    | SFdecl(fd) -> (vdecls, strct_decls, fd :: fdecls)
  in
  List.fold_left helper ([], [], []) sprog


(* the built-in structs in cnet. These MUST be in exact conjunction with those
 * declared in the libcnet/*.c and libcnet/*.h source files
 *)
let builtin_structs_l =
  let vd t n = {vtyp=t;vname=n} in
    [
      (* Some of the String types here are actually just void *s that will be
       * cast later.
       *)
      {sname="string"; members=[vd String "stub"; vd String "data"; vd Int "length"]};
      {sname="array"; members=[vd String "stub"; vd String "data"; vd Int "length"; vd Int "i_t"]};
      {sname="cnet_file"; members=[vd String "stub"; vd String "f"; vd Int "io_type"]};
      {sname="cnet_socket"; members=[vd String "stub"; vd String "f"; vd Int
                                       "io_type"; vd Int "fd"; vd Int "port"; vd
                                       Int "type"; vd String "addr"]}
    ]
;;

let builtin_structs =
  let add_builtin_strct m s = StringMap.add s.sname s m in
  List.fold_left add_builtin_strct StringMap.empty
    builtin_structs_l
;;


let mem_to_idx sd member =
  let rec helper n l = match l with
    hd :: tl when hd.vname = member -> n
    | hd :: tl -> helper (n + 1) tl
  in
  helper 0 sd.members
;;
