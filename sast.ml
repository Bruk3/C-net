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
  | SFloatlit of string
  | SStrlit of string
  | Sid of sid
  (* Operators *)
  | SBinop of sexpr * binop * expr
  | SBinassor of sid * bin_assign_op * sexpr
  | SUnop of unop * sexpr
  | SNew of newable
  | SArrayLit of typ * sexpr * sexpr list
  | SIndex of sid * expr
  | SCall of rid * expr list

type sstmt =
    SExpr of sexpr
  | SReturn of sexpr
  | SDelete of sid
  | SBreak
  | SContinue
  | SIf of (sexpr * sstmt) list * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SVdecl of sid
  | SVdecl_ass of sid * sexpr
  | SBlock of sstmt list

(*
  fid: typ * name : return type and name
  formals : (typ * name) list : the formals of the function
  fbody: the statements in the body of the function
*)
type sfunc = {fid: sid; formals: sid list; fbody: sstmt list}

(* here the sid list is typ * name so its the variable declarations *)
type strct = {sname: string; smembers: sid list}

type sdecl =
    SGVdecl of sid
  | SGVdecl_ass of
  | SSdecl of sstrct
  | SFdecl of sfunc

type sprogram =
  Program of sdecl list
