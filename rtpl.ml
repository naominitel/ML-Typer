(* Read-Type-Print-Loop *)

open Session

(* TODO: improve genericity by geleralizing the use of type schemes *)

let () =
    if (Array.length Sys.argv) <> 2 then
        Printf.printf "Syntax: %s {simple,core,poly}\n" Sys.argv.(0)
    else (
        let open Utils.Maybe in
        let typer = match Sys.argv.(1) with
          | "simple" -> (module Typers.Simple : Typers.BasicPolyTyper)
          | "core"   -> (module Typers.Core   : Typers.BasicPolyTyper)
          | "poly"   -> (module Typers.Poly   : Typers.BasicPolyTyper)
          | _        -> failwith "unknown typing algorithm"
        in
        let module Typer : Typers.BasicPolyTyper = (val typer) in
        let rec iter env =
            Printf.printf "λ > " ;
            flush stdout ;
            (try
                let (sess, (sp, defs)) = sess_open () in
                match defs with
                    | `ParseError err ->
                        Errors.span_err sess sp err ;
                        iter env

                    | `Expr ast ->
                        ignore (
                            (Ast.check (Errors.span_err sess) ast) >>=
                                (fun ast ->
                                    Ast.simple_ast (Errors.span_err sess) ast >>=
                                        (fun ast ->
                                            let sty = Typer.infer sess env ast in
                                            Printf.printf
                                                "type: %s\n"
                                                (Types.sty_to_string sty) ;
                                            None ))) ;
                        iter env

                    | `Defs [] -> exit 0

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
                                            (*
                                             * For now, we just tranform defs into
                                             * lets to pass them to the typer
                                             * TODO: extract bindings to keep
                                             * environment
                                             *)
                                            bind2
                                                (Ast.simple_ast err expr)
                                                (Ast.simple_pat err pat)
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
                                                 (Types.sty_to_string ty))
                                        new_env ;
                                    iter (new_env @ env))
             with Errors.CompileFailure -> iter env)
        in
        Printf.printf "\t\tRead Type Print Loop version 0.1. %s typer.\n\n"
                      Sys.argv.(1) ;
        iter []
    )
