(* Read-Type-Print-Loop *)

let rec rtpl env =
    let open Utils.Maybe in
    Printf.printf "λ > " ;
    flush stdout ;
    (try
        let (sess, (sp, defs)) = Session.sess_open () in
        match defs with
            | `ParseError err -> Errors.span_err sess sp err ; rtpl env
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
                                    let nenv = Typer.def_infer
                                                    sess local_env
                                                    pat expr in
                                    return nenv))
                        (return env) defs)
                in (match new_env with
                        | None         -> rtpl env
                        | Some new_env ->
                            let rec iter l = match l with
                                | [] -> ()
                                | l when l == env -> ()
                                | ((id, ty) :: l) ->
                                    Printf.printf
                                        "%s: %s\n" (Ident.show id)
                                        (Printer.show_sch ty) ;
                                    iter l
                            in iter new_env ;
                            rtpl new_env)

            | `Expr ast ->
                ignore (
                    (Ast.check (Errors.span_err sess) ast) >>=
                        (fun ast ->
                            let sty = Typer.infer sess env ast in
                            Printf.printf
                                "type: %s\n"
                                (Printer.show_sch (Hmx.sch sty)) ;
                            None )) ;
                rtpl env
         with Errors.CompileFailure -> rtpl env)

let run init_ty_env =
    Printf.printf "\t\tRead Type Print Loop version 0.1. \n\n" ; rtpl init_ty_env
