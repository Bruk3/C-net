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

                              (* Codegen utils *)
(* Changes the format of an sast program, which is a list of sdecls, to one the
 * codegen can accept, which is a tuple of lists of vdecls, struct_decls and
 * fdecls
 *)
let decompose_program (sprog : sdecl list) =
  let helper (vdecls, strct_decls, fdecls) decl = match decl with
    SGVdecl(vd) -> (vd :: vdecls, strct_decls, fdecls)
    | SGVdecl_ass (vd, _) -> (vd :: vdecls, strct_decls, fdecls) (* TODO: handle SGVdecl_ass properly *)
    | SSdecl(sd) -> (vdecls, sd :: strct_decls, fdecls)
    | SFdecl(fd) -> (vdecls, strct_decls, fd :: fdecls)
  in
  List.fold_left helper ([], [], []) sprog

