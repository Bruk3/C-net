module A = Ast
open Sast

                              (* Scanner utils *)
let count_new_lines whitespace lexbuf =
  String.iter
    (fun c -> if c = '\n' then Lexing.new_line lexbuf else ()) whitespace;;


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
let final_id_of_rid = function
  A.FinalID(fid) -> fid
  | A.RID(rid, mem) -> mem

                              (* Codegen utils *)
(* Changes the format of an sast program, which is a list of sdecls, to one the
 * codegen can accept, which is a tuple of lists of vdecls, struct_decls and
 * fdecls
 *)
let decompose_program (sprog : sdecl list) =
  let helper (vdecls, strct_decls, fdecls) decl = match decl with
    SGVdecl(vd) -> (vd :: vdecls, strct_decls, fdecls)
    | SSdecl(sd) -> (vdecls, sd :: strct_decls, fdecls)
    | SFdecl(fd) -> (vdecls, strct_decls, fd :: fdecls)
  in
  List.fold_left helper ([], [], []) sprog

