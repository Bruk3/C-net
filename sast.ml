(******************************************************************************
                       Semantically checked ast for cnet
*******************************************************************************)

open Ast

type sid =
  Sid of typ * rid

type sexpr = typ * sx
and sx =
    SNoexpr
  | SIntlit of int
  | SCharlit of int
  | SFloatlit of float
  | SStrlit of string
  | SId of rid
  (* Operators *)
  | SBinop of sexpr * binop * sexpr
  | SBinassop of string * bin_assign_op * sexpr
  | SUnop of unop * sexpr
  | SNew of newable
  | SArrayLit of typ * sexpr * sexpr list
  | SIndex of rid * sexpr
  | SCall of rid * sexpr list

type sstmt =
    SExpr of sexpr
  | SReturn of sexpr
  | SDelete of sexpr
  | SBreak
  | SContinue
  | SIf of (sexpr * sstmt) list * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SVdecl of vdecl
  | SVdecl_ass of vdecl * sexpr
  | SBlock of sstmt list

type sfunc = {
  styp: typ;
  sname: string;
  sparameters: id list;
  sbody: sstmt list
}

(* here the sid list is typ * name so its the variable declarations *)
(* type sstrct = {sname: string; smembers: sid list} *)

type sdecl =
    SGVdecl of vdecl
  | SGVdecl_ass of (vdecl * sexpr)
  | SSdecl of strct
  | SFdecl of sfunc

type sprogram =
  SProgram of sdecl list

  (* type sprogram = {
    vdecls : (vdecl * expr) list ;
    strct_decls: strct list;
    fdecls : sfunc list;
  }
   *)
  (* Pretty-printing functions *)


  let rec string_of_sexpr (t, e) =
    "(" ^ string_of_typ t ^ " : " ^ (match e with
      SNoexpr -> ""
    | SIntlit(l) -> string_of_int l
    | SCharlit(l) -> "" ^ (Char.escaped(Char.chr(l)))
    | SFloatlit(l) -> string_of_float l
    | SStrlit(l) -> "\"" ^ l ^ "\""
    | SId(s) -> string_of_rid s
    | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | SBinassop(v, o, e) -> v ^ string_of_binassop o ^ string_of_sexpr e
    | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
    | SNew (n) -> string_of_newable n
    | SArrayLit (t, e, el) ->
    "new " ^ string_of_typ t ^ "[" ^ string_of_sexpr e ^ "] = {" ^
    String.concat ", " (List.map string_of_sexpr el) ^ "}"
    | SIndex (s, e) -> string_of_rid s ^ "[" ^ string_of_sexpr e ^ "]"
    | SCall(f, el) ->
        string_of_rid f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
            ) ^ ")"

let string_of_svdecl_assign (t, id, e) =
  string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr e ^ ";\n"

  let rec string_of_sstmt = function
    SExpr(expr) -> string_of_sexpr expr ^ ";\n";
    | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
    | SIf(e_s_l, SExpr(Void, SNoexpr)) -> let string_of_sif ((e, _)) =
    "if (" ^ string_of_sexpr e ^ ")\n" in String.concat "else " (List.map string_of_sif e_s_l)
    | SIf(e_s_l, s) -> let string_of_sif ((e, _)) =
        "if (" ^ string_of_sexpr e ^ ")\n" in String.concat "else " (List.map string_of_sif e_s_l)  ^
        "else\n{" ^ string_of_sstmt s ^ "\n}";
    | SFor(e1, e2, e3, s) ->
        "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
        string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
    | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
    | SVdecl(v)           -> string_of_vdecl v
    | SVdecl_ass({vtyp; vname}, e)
      -> string_of_svdecl_assign(vtyp, vname, e)

    | SBlock(stmts) ->
        "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"

    | _ -> raise (Failure("Unimplemented statement type in sast prettyprint"))

  (* let string_of_sfdecl fdecl =
    string_of_typ fdecl.styp ^ " " ^
    fdecl.sname ^ "(" ^ String.concat ", " (List.map snd fdecl.sparameters) ^
    ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
    String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
    "}\n" *)

let string_of_sfunc (t, n, p, b) =
  string_of_typ t ^ " " ^ n ^ "(" ^ String.concat "," (List.map string_of_id p) ^
  ")\n{\n" ^ String.concat ""
    (List.map
       string_of_sstmt
       b
       (* (List.map (fun stmt -> (stmt, 1)) b) *)
    ) ^ "}\n\n"


let string_of_sdecl = function
    SGVdecl(vdecl) -> string_of_vdecl vdecl
  | SGVdecl_ass({vtyp; vname}, e) -> string_of_svdecl_assign(vtyp, vname, e)
  | SSdecl({name; members}) -> string_of_strct(name, members)
  | SFdecl({styp; sname; sparameters; sbody; _}) -> string_of_sfunc(styp, sname, sparameters, sbody)
  (* let string_of_sprogram ((vdecls : (vdecl * sexpr) list), (strct_decls : strct list), (fd : sfunc list))  =
    String.concat "" (List.map string_of_sfunc(fd)) ^ "\n" *)

    (* TODO *)
let string_of_sprogram  = function
    SProgram(decls) -> String.concat "" (List.map string_of_sdecl decls) ^ "\n"
