
(* Abstract syntax tree types for C-net *)
type program = Program of string


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
type rid =
    FinalID of typ * string (* An id always has a type and a name *)
  | RID of rid * string

                              (* types in C-net *)
and typ = 
    Char | Int | Float | String | Socket | Struct of string 
  | Array of typ * int * expr list (* int:length and expr list:array literal *)

             (* Arguments to a function call or an array literal *)
and args = 
    Empty of unit
  | Nonempty of expr list

                                (* Expression *)
and expr = 
  (* Literals *)
    Intlit of typ * int 
  | Charlit of typ * expr
  | Floatlit of typ * float
  | Strlit of typ * string
  | Id of rid
  | Expr of expr
  (* Operators *)
  | Binrelop of expr * bin_relational_op * expr
  | Binlogop of expr * bin_logical_op    * expr
  | Unlogop  of un_logical_op     * expr
  | Binariop of expr * bin_arithmetic_op * expr
  | Unariop  of un_arithmetic_op  * expr 
  | Binassop of rid * bin_assign_op      * expr
  (* Arrays and new/delete *)
  | Delete of rid
  | Index  of rid * expr
              (* TODO new for arrays and structs *)
  (* Function calls *)
  | Call of rid * args
  | New of typ

