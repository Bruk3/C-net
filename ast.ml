
(* Abstract syntax tree types for C-net *)
type program = Program of string



type rid =  (* 'recursive' id that can be an id or a struct member DOT an id) *)
    FinalID of string
  | RID of rid * string

(*  Relational operators  *)
type bin_relational_op = (* binary relational operators *)
  Eq | Neq | Lt | Leq | Gt | Geq

type un_relational_op = (* unary relational operator *)
  Not

(*  Logical operators  *)
type bin_logical_op = (* binary logical operators *)
  And | Or 

(*  Arithmetic operators  *)
type bin_arithmetic_op = (* binary arithmetic operators *)
  Add | Sub | Mul | Div | Mod

type un_arithmetic_op = (* unary arithemetic operator *)
  Minus

type bin_assign_op =  (* assignment operators *)
  Assign | PlusEq | MinusEq



type expr = 
    Intlit of int
  | Charlit of int
  | Floatlit of float
  | Strlit of string
  | Id of rid
  | Expr of expr
  | Binrelop of expr * bin_relational_op * expr
  | Binlogop of expr * bin_logical_op    * expr
  | Binariop of expr * bin_arithmetic_op * expr
  | Binassop of rid * bin_assign_op      * expr
       


    
