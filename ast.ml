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
and id = typ * string

and rid =
    FinalID of string
  | RID of rid * string (* my_struct.my_member *)
  | Index of rid * expr (* my_struct.my_member[3] *)

(* So for eg. my_struct.ms2.ms_array[2].my_member is valid *)

                              (* types in C-net *)
and typ =
  Char | Int | Float | String | Socket | File | Struct of string | Void
  | Array of typ


                                (* Expression *)
and newable =
    NStruct of string

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
  | New of newable
  | ArrayLit of typ * expr * expr list (* expr:length and expr list:array literal *)
  (* | Index  of rid * expr *)
  (* Function calls *)
  | Call of rid * expr list


                                (* Statements *)
type vdecl = {vtyp : typ ; vname : string}

type stmt =
    Expr of expr
  | Return            of expr
  | Delete of rid
  | Break
  | Continue
  | If                of (expr  * stmt) list * stmt
  | For               of expr  * expr * expr * stmt
  | While             of expr  * stmt
  | Vdecl             of vdecl
  | Vdecl_ass      of vdecl * expr
  | Block   of stmt list

                                (* Functions *)
type func = {t: typ ; name : string ; parameters : id list ; body : stmt list; locals : id list}

                                 (* Structs *)
type strct = { sname : string ; members : vdecl list }

                                 (* Program *)
type decl =
    GVdecl of vdecl (* Renamed to GVdecl to avoid collision with Vdecl of stmt which was giving errors*)
  | GVdecl_ass of (vdecl * expr)
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
    Char      -> "char"
  | Int       -> "int"
  | Float     -> "float"
  | Socket    -> "socket"
  | File      -> "file"
  | String    -> "string"
  | Struct(t) -> "struct " ^ t
  | Void      -> "void"
  | Array(t)  ->  "" ^ string_of_typ t ^ "[]"

let string_of_id (t, n) = string_of_typ t ^ " " ^ n
let rec string_of_rid = function
  | FinalID(id) -> id
  | RID(r, final) -> string_of_rid r ^ "." ^ final
  | Index(r, expr) -> string_of_rid r ^ "[" ^ (string_of_expr expr) ^ "]"

and string_of_newable = function
    NStruct(n)  -> "struct " ^ n

and string_of_expr = function
  | Noexpr -> ""
  | Intlit(id) -> string_of_int id
  | Charlit(id) -> "" ^ "\'" ^ (Char.escaped(Char.chr(id))) ^ "\'"
  | Floatlit(id) -> string_of_float id
  | Strlit(id) -> "\"" ^ id ^ "\""
  | Rid(id) -> string_of_rid id (* TODO *)
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Binassop(id, op, r) -> string_of_rid id ^ " " ^ string_of_binassop op ^ " " ^ string_of_expr r
  | New(n) -> "new " ^  string_of_newable n
  | ArrayLit(t, e, el) ->
    "new " ^ string_of_typ t ^ "[" ^ string_of_expr e ^ "] = {" ^
    String.concat ", " (List.map string_of_expr el) ^ "}"
  (* | Index(id, e) -> string_of_rid id ^ "[" ^ string_of_expr e ^ "]" *)
  | Call(f, el) ->
    string_of_rid f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"


let string_of_vdecl vdecl  =
  string_of_typ vdecl.vtyp ^ " " ^ vdecl.vname ^ ";\n"
let string_of_vdecl_assign (t, id, e) =
  string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^ ";\n"
let string_of_strct (name, members) =
  "struct " ^ name ^ " {\n\t" ^
  String.concat "\t" (List.map string_of_vdecl members) ^ "};\n\n"

let tabs num = (* tabs 5 returns "\t\t\t\t\t" *)
  let rec helper s =  function
    | 1 -> "\t" ^ s
    | n when n > 1 -> helper (s ^ "\t") (n-1)
    | _ -> ""
  in helper "" num;;

(* TODO *)
let rec string_of_stmt (main_stmt, main_indent) =
  let print_block b ind = match b with
      Block(_)    -> string_of_stmt (b, ind)
    | _           -> (tabs ind) ^
                     "{\n" ^ string_of_stmt (b, ind + 1) ^
                     (tabs ind) ^ "}\n"
  in
  let helper (stmt, indent) = match stmt with
      Block(stmts)       -> "{\n" ^ String.concat ""
                              (List.map string_of_stmt
                                 (List.map
                                    (fun stmt -> (stmt, indent + 1))
                                    stmts
                                 )
                              )
                            ^ (tabs indent) ^ "}\n"
    | Expr(expr)         -> string_of_expr expr ^ ";\n"
    | Return(expr)       -> "return " ^ string_of_expr expr ^ ";\n"
    | Delete(id)         -> "delete " ^ string_of_rid id ^ ";"
    | Break              -> "break;"
    | Continue           -> "continue;"
    | If(e_s_l,Expr(Noexpr)) -> let string_of_if ((e, s))  =
        "if (" ^ string_of_expr e ^ ")\n"  ^ (print_block s indent)
      in String.concat (tabs indent ^ "else ") (List.map string_of_if e_s_l)
    | If(e_s_l, s)       ->
      let string_of_if ((e, s))  =
        "if (" ^ string_of_expr e ^ ")\n"  ^ (print_block s indent)
      in String.concat (tabs indent ^ "else ") (List.map string_of_if e_s_l) ^
         (tabs indent) ^ "else\n" ^ (print_block s indent)
    | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^
                            string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") "
                            ^ (print_block s indent)
    | While(e, s)        -> "while (" ^ string_of_expr e ^ ")\n"
                            ^ (print_block s indent)
    | Vdecl(v)           -> string_of_vdecl v
    | Vdecl_ass({vtyp; vname}, e)
      -> string_of_vdecl_assign(vtyp, vname, e)

  in (tabs main_indent) ^ helper (main_stmt, main_indent)


let string_of_func (t, n, p, b) =
  string_of_typ t ^ " " ^ n ^ "(" ^ String.concat "," (List.map string_of_id p) ^
  ")\n{\n" ^ String.concat ""
    (List.map
       string_of_stmt
       (List.map (fun stmt -> (stmt, 1)) b)
    ) ^ "}\n\n"

let string_of_decl = function
    GVdecl(vdecl) -> string_of_vdecl vdecl
  | GVdecl_ass({vtyp; vname}, e) -> string_of_vdecl_assign(vtyp, vname, e)
  | Sdecl({sname; members}) -> string_of_strct(sname, members)
  | Fdecl({t; name; parameters; body; _}) -> string_of_func(t, name, parameters, body)

let string_of_program  = function
  Program(decls) -> String.concat "" (List.map string_of_decl decls) ^ "\n"
