(* Read-Type-Print-Loop *)

module type RTPL = sig
    val run: unit -> unit
end

module MakeRTPL (Typer: Typers.GenTyper): RTPL = struct
    let rec iter env =
        let open Utils.Maybe in
        Printf.printf "λ > " ;
        flush stdout ;
        (try
            let (sess, (sp, defs)) = Session.sess_open () in
            match defs with
                | `Defs [] -> ()

                | `Defs defs ->
                    let new_env =
                        (List.fold_left
                            (fun local_env (pat, expr) ->
                                let err = Errors.span_err sess in
                                bind3
                                    local_env
                                    (Ast.check_pat err pat)
                                    (Ast.check err expr)
                                    (fun local_env pat expr ->
                                        bind2
                                            (Typer.from_ast err expr)
                                            (Typer.from_pat err pat)
                                            (fun ast pat ->
                                                let nenv = Typer.def_infer
                                                                sess
                                                                (local_env @ env)
                                                                pat ast in
                                                return (nenv @ local_env))))
                            (return []) defs)
                    in (match new_env with
                            | None         -> iter env
                            | Some new_env ->
                                List.iter
                                    (fun (str, ty) ->
                                        Printf.printf
                                            "%s: %s\n" str
                                            (Typer.ty_to_string ty))
                                    new_env ;
                                iter (new_env @ env))

                | `Expr ast ->
                    ignore (
                        (Ast.check (Errors.span_err sess) ast) >>=
                            (fun ast ->
                                Typer.from_ast (Errors.span_err sess) ast >>=
                                    (fun ast ->
                                        let sty = Typer.infer sess env ast in
                                        Printf.printf
                                            "type: %s\n"
                                            (Typer.ty_to_string sty) ;
                                        None ))) ;
                    iter env
             with Errors.CompileFailure -> iter env)

    let run () =
        Printf.printf "\t\tRead Type Print Loop version 0.1. %s typer.\n\n"
                      Sys.argv.(1) ;
        iter []
end

let () =
    if (Array.length Sys.argv) <> 2 then
        Printf.printf "Syntax: %s {simple,core,poly}\n" Sys.argv.(0)
    else (
        let module R = (val match Sys.argv.(1) with
            | "simple" -> (module MakeRTPL (Typers.Simple) : RTPL)
            | "core"   -> (module MakeRTPL (Typers.Core)   : RTPL)
            | "poly"   -> (module MakeRTPL (Typers.Poly)   : RTPL)
            | _        -> failwith "unknown typing algorithm")
        in R.run ()
    )
