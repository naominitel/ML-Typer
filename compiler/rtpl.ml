(* Read-Type-Print-Loop *)

open Codemap
open Utils

module type RTPL = sig
    val run: unit -> unit
end

exception EmptyInput

module MakeRTPL (Typer: Typers.GenTyper): RTPL = struct
    let rec iter env =
        let open Errors in
        Printf.printf "µλ > " ;
        flush stdout ;
        (try
            let (sess, defs) = Session.sess_open () in
            let res = bind
                (fun defs -> match defs.d with
                    | `Defs [] -> raise EmptyInput

                    | `Defs defs ->
                        List.fold_left
                            (fun local_env (pat, expr) ->
                                map3
                                    (fun local_env ast pat ->
                                        let nenv = Typer.def_infer
                                                       (module Types.Substitutions) 
                                                       sess
                                                       (local_env @ env)
                                                       pat ast in
                                        (nenv @ local_env))
                                    local_env
                                    (Typer.from_ast expr)
                                    (Typer.from_pat pat))
                            (Ok []) defs

                    | `Expr ast ->
                        map (fun ast ->
                                let sty = Typer.infer (module Types.Substitutions) (Types.Substitutions.make ()) sess env ast in
                                Printf.printf "type: %s\n"
                                              (Typer.ty_to_string sty) ;
                                [])
                            (Typer.from_ast ast))
                defs
            in match res with
                | Err errors ->
                    LazyList.iter (Session.span_err sess) errors ;
                    iter env
                | Ok new_env ->
                    List.iter
                        (fun (str, ty) ->
                            Printf.printf "%s: %s\n" str (Typer.ty_to_string ty))
                        new_env ;
                        iter (new_env @ env)
        with
            | Session.CompileFailure -> iter env
            | EmptyInput -> Printf.printf "Bye.\n")

    let run () =
        Printf.printf "\t\tµλ Read Type Print Loop version 0.1. %s typer.\n\n"
                      Sys.argv.(1) ;
        iter []
end

let () =
    if (Array.length Sys.argv) <> 2 then
        Printf.printf "Syntax: %s {simple,core,poly}\n" Sys.argv.(0)
    else (
        let module R = (val match Sys.argv.(1) with
            | "simple" -> (module MakeRTPL (Typers.Generic) : RTPL)
            | "poly"   -> (module MakeRTPL (Typers.Poly)    : RTPL)
            | _        -> failwith "unknown typing algorithm")
        in R.run ()
    )
