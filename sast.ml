(******************************************************************************
                       Semantically checked ast for cnet
*******************************************************************************)

open Ast

(* A semantic error in cnet. It contains a message and a line number *)

(* Kidus: The line number is not really implemented and is only there for
 * forward compatibility. Right now the function semant_err takes only a string,
 * but later it can take a line number as well
 * *)
exception SemanticError of string * int;;

(* The function for raising a semantic error *)
let semant_err (msg : string) =
  raise (SemanticError(msg, -1));;

type sid =
    SFinalID of string
  | SRID of sid * string
  | SIndex of sid * sexpr

and sexpr = typ * sx
and sx =
    SNoexpr
  | SIntlit of int
  | SCharlit of int
  | SFloatlit of float
  | SStrlit of string
  | SId of sid
  (* Operators *)
  | SBinop of sexpr * binop * sexpr
  | SBinassop of sid * bin_assign_op * sexpr
  | SUnop of unop * sexpr
  | SNew of string
  | SArrayLit of typ * sexpr * sexpr list
  | SCall of string * sexpr list

type sstmt =
    SExpr of sexpr
  | SReturn of sexpr
  | SDelete of sexpr
  | SBreak
  | SContinue
  | SIf of (sexpr * sstmt) list
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SVdecl of vdecl
  | SVdecl_ass of vdecl * sexpr
  | SBlock of sstmt list

type sfunc = {
  styp: typ;
  sfname: string;
  sparameters: id list;
  sbody: sstmt list
}

(* here the sid list is typ * name so its the variable declarations *)
(* type sstrct = {sname: string; smembers: sid list} *)

type sdecl =
  | SGVdecl_ass of (vdecl * sexpr)
  | SSdecl of strct
  | SFdecl of sfunc

type sprogram = sdecl list;;

 (* Pretty-printing functions *)

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
      try ignore (Str.search_forward re s1 0); true
      with Not_found -> false
;;

let remove_prefix (str: string) (prefix: string) =
  let strlen = String.length str in
  let prelen = String.length prefix in
  let prefix_found = contains str prefix in
   if prefix_found then String.sub str prelen (strlen - prelen) else str
;;

let rec string_of_sid = function
  | SFinalID(id) -> id
  | SRID(r, final) -> string_of_sid r ^ "." ^ final
  | SIndex(r, sexpr) -> string_of_sid r ^ "[" ^ (string_of_sexpr sexpr) ^ "]"

and string_of_sexpr (t, e) =
    "(" ^ string_of_typ t ^ " : " ^ (match e with
          SNoexpr -> ""
      | SIntlit(l) -> string_of_int l
      | SCharlit(l) -> "" ^ "\'" ^ (Char.escaped(Char.chr(l))) ^ "\'"
      | SFloatlit(l) -> string_of_float l
      | SStrlit(l) -> "\"" ^ l ^ "\""
      | SId(s) -> string_of_sid s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | SBinassop(v, o, e) -> string_of_sid v ^ string_of_binassop o ^ string_of_sexpr e
    | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
    | SNew (n) -> "new " ^ n
    | SArrayLit (t, e, el) ->
    "new " ^ string_of_typ t ^ "[" ^ string_of_sexpr e ^ "] = {" ^
    String.concat ", " (List.map string_of_sexpr el) ^ "}"
    | SCall(f, el) ->
        remove_prefix f "user_" ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
           ) ^ ")"
;;

let string_of_svdecl_assign (t, id, e) =
  string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr e ^ ";\n"
;;

let rec string_of_sstmt = function
  SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SDelete(expr) -> "delete " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e_s_l) ->
    let string_of_sif (e, s) =
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
    in
    String.concat "else " (List.map string_of_sif e_s_l)
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ")\n\t " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SVdecl(v)           -> string_of_vdecl v
  | SVdecl_ass({vtyp; vname}, e)
    -> string_of_svdecl_assign(vtyp, vname, e)
  | SBreak -> "break;"
  | SContinue -> "continue;"

  | SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n";;

let string_of_sfunc (t, n, p, b) =
  string_of_typ t ^ " " ^ remove_prefix n "user_" ^ "(" ^ String.concat "," (List.map string_of_id p) ^
  ")\n{\n" ^ String.concat ""
    (List.map
       string_of_sstmt
       b
    ) ^ "}\n\n";;

let string_of_sdecl = function
  | SGVdecl_ass({vtyp; vname}, e) -> string_of_svdecl_assign(vtyp, vname, e)
  | SSdecl({sname; members}) -> string_of_strct(sname, members)
  | SFdecl({styp; sfname; sparameters; sbody; _}) -> string_of_sfunc(styp, sfname, sparameters, sbody)

    (* TODO *)
let string_of_sprogram (decls : sprogram) =
  String.concat "" (List.map string_of_sdecl decls) ;;
