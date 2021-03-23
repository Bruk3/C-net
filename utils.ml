module A = Ast
open Sast

let count_new_lines whitespace lexbuf =
  String.iter
    (fun c -> if c = '\n' then Lexing.new_line lexbuf else ()) whitespace;;


let my_sast = (
  [],
  [],
  [
    {
      fid = A.Int;
      sname = "main";
      sparameters = [];
      sbody =
        [
          SExpr( A.Int,
                 SCall
                   (
                     Sid((A.Int, RID(FinalID(Nid("stdout")), "println") )),
                     [ (A.String, SStrlit("Hello World!\n")) ]
                   )
               );
          SReturn(A.Int, SIntlit(0))
        ]
    }
  ]
)
