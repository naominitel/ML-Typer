open Codemap
open Session

exception CompileFailure

type callback = span -> string -> unit

(* Display a pretty error message along with the errorneous piece of code *)
let span_err sess sp msg =
    let open Lexing in
    let open Format in
    let (s, e) = sp in
    let file = Hashtbl.find sess.cm s.pos_fname in
    let scol = s.pos_cnum - s.pos_bol + 1 in
    let ecol = e.pos_cnum - e.pos_bol + 1 in
    let ff = Format.err_formatter in
    fprintf ff "%s:%d:%d: %d:%d \x1b[1;31merror: \x1b[1;37m%s\x1b[0m\n"
                    file.name s.pos_lnum scol e.pos_lnum ecol msg ;
    if s.pos_lnum = e.pos_lnum then
        (* The errorneous expression spans over a single line
         * Draw an arrow to point the exact position *)
        let line = pos_line sess.cm s in
        let pre  = sprintf "%s:%d" file.name s.pos_lnum in
        let pad  = String.make (String.length pre + scol) ' ' in
        let arr  =
            if scol = ecol then ""
            else String.make (ecol - scol - 1) '~'
        in
        fprintf ff "%s %s\n" pre line ;
        fprintf ff "%s\x1b[1;31m^%s\x1b[0m\n" pad arr
    else for i = s.pos_lnum to e.pos_lnum do
        let line = line sess.cm s.pos_fname i in
        fprintf ff "%s:%d %s\n" file.name i line
    done

let span_fatal sess sp msg =
    span_err sess sp msg ;
    raise CompileFailure
