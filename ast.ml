(******************************************************************************
                      Abstract syntax tree types for C-net
*******************************************************************************)

                          (*  Relational operators  *)
type bin_relational_op =
  Eq | Neq | Lt | Leq | Gt | Geq


                           (*  Logical operators  *)
type bin_logical_op =
  And | Or

type un_logical_op =
  Not

                          (*  Arithmetic operators  *)
type bin_arithmetic_op =
  Add | Sub | Mul | Div | Mod

type un_arithmetic_op =
  Minus

                           (* assignment operators *)
type bin_assign_op =
  Assign | PlusEq | MinusEq

      (* 'recursive' id that can be an id or a member of a struct *)
type id =
  Id of typ * string

and rid =
    FinalID of id (* An id always has a type and a name *)
  | RID of rid * string

                              (* types in C-net *)
and typ =
    Char | Int | Float | String | Socket | Struct of string
  | Array of typ * int * expr list (* int:length and expr list:array literal *)

             (* Arguments to a function call or an array literal *)
and args =
  Args of expr list

                                (* Expression *)
and expr =
  (* Literals *)
    Intlit of int
  | Charlit of int
  | Floatlit of float
  | Strlit of string
  | Rid of rid
  (* | Expr of expr *)
  (* Operators *)
  | Binrelop of expr * bin_relational_op * expr
  | Binlogop of expr * bin_logical_op    * expr
  | Unlogop  of un_logical_op     * expr
  | Binariop of expr * bin_arithmetic_op * expr
  | Unariop  of un_arithmetic_op  * expr
  | Binassop of rid * bin_assign_op      * expr
  (* Arrays and new/delete *)
  | Delete of rid
  | New of typ
  | Index  of rid * expr
  (* Function calls *)
  | Call of rid * args


                                (* Statements *)
type vdecl = {vtyp : typ ; vname : string}

type stmt =
    Statement         of expr
  | Return            of expr
  | If                of expr  * stmt  * stmt 
  | For               of expr  * expr * expr * stmt
  | While             of expr  * stmt 
  | Vdecl             of vdecl
  | Vdecl_assign      of vdecl * expr
  | Block   of stmt list

                                (* Parameters *)
type params = Params of id list

                                (* Functions *)
type func = { name : string ; parameters : params ; body : stmt list }

                                 (* Structs *)
type strct = { name : string ; members : vdecl list }

                                 (* Program *)
type decl =
    Vdecl of vdecl
  | Sdecl of strct
  | Fdecl of func

type program =
  Program of decl list
