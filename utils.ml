module A = Ast
open Sast

                              (* Scanner utils *)
let count_new_lines whitespace lexbuf =
  String.iter
    (fun c -> if c = '\n' then Lexing.new_line lexbuf else ()) whitespace;;

(* Get the current line number from the lexbuf *)
let line_num lexbuf = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum


                                (* SAST utils *)
let my_sast = (
  [],
  [],
  [
    {
      styp = A.Int;
      sname = "main";
      sparameters = [];
      sbody =
        [
          SExpr( A.Int,
                 SCall
                   (
                     A.RID(A.FinalID("stdout"), "println"),
                     [ (A.String, SStrlit("Hello World!\n")) ]
                   )
               );
          SReturn(A.Int, SIntlit(0))
        ]
    }
  ]
)

(* Gets the FinalID part of a recursive id. For example, it extracts println
 * from my_struct.my_other_struct.my_file.println
 *)
let rec final_id_of_rid = function
  A.FinalID(fid) -> fid
  | A.RID(_, mem) -> mem
  | A.Index(rid, _) -> final_id_of_rid rid
;;

(* Get a default value for a global variable based on its type *)
let default_global = function
    A.Char | A.Int -> (A.Int, SIntlit(0))
  | A.Float -> (A.Float, SFloatlit(0.0))
  | A.String -> (A.String, SStrlit(""))
  | A.Void   -> semant_err "[COMPILER BUG] uncaught void global variable detected"
  | _ -> (A.Void, SNoexpr)
;;

(* compute the value of a global variable assignment at compile time. Global
 * variables can only be assigned to constant expressions at declaration *)
let compute_global vdecl exp =
  let verify_types (t1 : A.typ) (t2 : A.typ) =
    if t1 = t2 then ()
     else (semant_err ("incompatible types " ^ A.string_of_typ t1 ^ " and " ^
           A.string_of_typ t2 ^ " in global variable " ^ vdecl.A.vname))
  in

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
      let bool_int b = if b then 1 else 0 in
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
                                 -> semant_err "global expression type not
                                 implemented"
       | _ -> semant_err ("non-constant expression used for global variable " ^ vdecl.A.vname)
      )

  in
  eval_constant exp ;;



                              (* Codegen utils *)
(* Changes the format of an sast program, which is a list of sdecls, to one the
 * codegen can accept, which is a tuple of lists of vdecls, struct_decls and
 * fdecls
 *)
let decompose_program (sprog : sdecl list) =
  let helper (vdecls, strct_decls, fdecls) decl = match decl with
    | SGVdecl_ass (vd, _) -> (vd :: vdecls, strct_decls, fdecls) (* TODO: handle SGVdecl_ass properly *)
    | SSdecl(sd) -> (vdecls, sd :: strct_decls, fdecls)
    | SFdecl(fd) -> (vdecls, strct_decls, fd :: fdecls)
  in
  List.fold_left helper ([], [], []) sprog

