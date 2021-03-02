(******************************************************************************
                      Abstract syntax tree types for C-net
*******************************************************************************)


type unop = Not | Minus
type binop = 
      (*  Relational operators  *)
    Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
      (*  Logical operators  *)
  | And
  | Or
      (*  Arithmetic operators  *)
  | Add 
  | Sub 
  | Mul 
  | Div 
  | Mod

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
  | ArrayLit of typ * expr * expr list (* expr:length and expr list:array literal *)
  | Array of typ

             (* Arguments to a function call or an array literal *)
and args =
  Args of expr list

                                (* Expression *)
and expr =
  Noexpr
  (* Literals *)
  | Intlit of int
  | Charlit of int
  | Floatlit of float
  | Strlit of string
  | Rid of rid
  (* | Expr of expr *)
  (* Operators *)
  | Binop of expr * binop * expr
  | Unop of unop * expr
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
    Expr of expr
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

