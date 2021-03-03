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
  Char | Int | Float | String | Socket | Struct of string | Void
  | ArrayLit of typ * expr * expr list (* expr:length and expr list:array literal *)
  | Array of typ

                                (* Expression *)
and expr =
  Noexpr
  (* Literals *)
  | Intlit of int
  | Charlit of int
  | Floatlit of float
  | Strlit of string
  | Rid of rid
  (* Operators *)
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Binassop of rid * bin_assign_op      * expr
  (* Arrays and new/delete *)
  | Delete of rid
  | New of typ
  | Index  of rid * expr
  (* Function calls *)
  | Call of rid * expr list  


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

                                (* Functions *)
type func = {t: typ ; name : string ; parameters : id list ; body : stmt list }

                                 (* Structs *)
type strct = { name : string ; members : vdecl list }

                                 (* Program *)
type decl =
    Vdecl of vdecl
  | Sdecl of strct
  | Fdecl of func

type program =
  Program of decl list

  (* Pretty-printing functions *)
  let string_of_op = function
      Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Leq -> "<="
    | Gt -> ">"
    | Geq -> ">="
    | And -> "&&"
    | Or -> "||"
    | Mod -> "%"
  
  let string_of_uop = function
      Minus -> "-"
    | Not -> "!"

  let string_of_binassop = function
    Assign -> "="
    | PlusEq -> "+="
    | MinusEq -> "-="
  


  (* // TODO  *)

  
  let rec string_of_typ = function
    Char        -> "char"
    | Int       -> "int"
    | Float     -> "float"
    | Socket    -> "socket"
    | String    -> "string"
    | Struct(t)    -> "struct " ^ t
    | Void      -> "void"
    | Array(t) ->  "" ^ string_of_typ t ^ "[]" 
    | ArrayLit(t, e, el) -> "ArrayLiteral here" 
    (* TODO: Replace by fixed version of 
     ^ string_of_typ t  ^ "[" ^ String.concat "," (List.map string_of_expr  el) ^ "]"
     *)

  let string_of_id = function 
  | Id(t, n) -> "" ^ string_of_typ t ^ n 
  let rec string_of_rid = function  
  | FinalID(id) -> string_of_id id 
  | RID(r, final) -> string_of_rid r ^ "." ^ final

    let rec string_of_expr = function
      | Noexpr -> ""
      | Intlit(id) -> string_of_int id
      | Charlit(id) -> "" ^ (Char.escaped(Char.chr(id)))
      | Floatlit(id) -> string_of_float id
      | Strlit(id) -> id 
      | Rid(id) -> string_of_rid id (* TODO *)
      | Binop(e1, o, e2) ->
          string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
      | Unop(o, e) -> string_of_uop o ^ string_of_expr e
      | Binassop(id, op, r) -> string_of_rid id ^ string_of_binassop op ^ " " ^ string_of_expr r 
      | Delete(id) -> "delete " ^ string_of_rid id
      | New(typ) -> "new " ^  string_of_typ typ
      | Index(id, e) -> string_of_rid id ^ "[" ^ string_of_expr e ^ "]"
      | Call(f, el) ->   
          string_of_rid f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  

let string_of_vdecl vdecl  = string_of_typ vdecl.vtyp ^ " " ^ vdecl.vname ^ ";\n"
let string_of_vdecl_assign (t, id, e) 
= string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^ ";\n"
let string_of_strct (name, members) = 
  "struct " ^ name ^ "{\n" ^ 
  String.concat "" (List.map string_of_vdecl members) ^ "\n}\n"

(* TODO *)

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Vdecl(vdecl) -> string_of_vdecl vdecl
  | Vdecl_assign({vtyp; vname}, e) -> string_of_vdecl_assign(vtyp, vname, e) 

let string_of_func (t, n, p, b) = 
  string_of_typ t ^ " " ^ n ^ "(" ^ String.concat "," (List.map string_of_id p) ^ 
  ")\n{\n" ^ 
  String.concat "" (List.map string_of_stmt b ) ^ 
  "}\n"

let string_of_decl = function 
  Vdecl(vdecl) -> string_of_vdecl vdecl
  | Sdecl({name; members}) -> string_of_strct(name, members) 
  | Fdecl({t; name; parameters; body}) -> string_of_func(t, name, parameters, body) 
    

let string_of_program (decls) = 
  String.concat "" (List.map string_of_decl decls) ^ "\n"

