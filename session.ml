(*
 * The current compiling session.
 * For now, just contains the codemap, but additional
 * context informations may be added later
 *)

open Codemap

type session = {
    cm: codemap ;
}

(* creates a codemap from the lexmap *)
let codemap_of lexmap filename =
    let cm = Hashtbl.create 10 in
    Hashtbl.add cm filename {
        Codemap.name = filename ;
        Codemap.contents = lexmap.Lexer.contents ;
        Codemap.lines = lexmap.Lexer.lines
    } ; cm

(*
 * Opens a new compiler session from the given filename
 * If no filename is provided, uses stdin
 * Builds the AST for the current parse session and
 * returns a pair (session, ast)
 *)
let sess_open ?file () =
  let (ic, name) = match file with
  | Some f -> (open_in f, f)
  | _ -> (stdin, "<stdin>") in
  let lexmap = Lexer.new_lexmap () in
  let lexbuf = Lexing.from_function (Lexer.read ic lexmap) in
  Lexing.(lexbuf.lex_curr_p <- {
      pos_fname = name ; pos_lnum = 1;
      pos_bol = 0 ; pos_cnum = 0
  }) ;
  let ast    = Parser.main (Lexer.token lexmap) lexbuf in
  ({ cm = codemap_of lexmap name ; }, ast)

(*
 * Opens a new compiler session from a string containing the
 * program text. Builds the AST and returns a pair (session, ast)
 *)
let sess_open_str contents =
    let lexmap = Lexer.lexmap_from_str contents in
    let lexbuf = Lexing.from_function (Lexer.read_str lexmap) in
    Lexing.(lexbuf.lex_curr_p <- {
        pos_fname = "<toplevel>" ; pos_lnum = 1;
        pos_bol = 0 ; pos_cnum = 0
    }) ;
    let ast    = Parser.main (Lexer.token lexmap) lexbuf in
    ({ cm = codemap_of lexmap "<toplevel>" ; }, ast)
