(*
 * FIXME: should be linked to codegen somehow. and should probably be
 * elsewhere
 *)
let init_ty_env = [
    (Ident.intern "fst", let v = Types.next_var () in
                         Types.gen (`TFunc (`TList (`TVar v), `TVar v))) ;
    (Ident.intern "snd", let v = Types.next_var () in
                         Types.gen (`TFunc (`TList (`TVar v), `TList (`TVar v)))) ;
    (Ident.intern "print_int", `TSTy (`TFunc (`TInt, `TUnit)))
]

let compile_and_save path =
    let (sess, (sp, defs)) = Session.sess_open ~file:path () in
    match defs with
        | `ParseError err -> Errors.span_err sess sp err ; `Err
        | `Defs [] -> `Ok

        | `Defs defs ->
            let filename = Filename.basename path in
            let mod_name =
                try Filename.chop_extension filename
                with Invalid_argument _ -> filename
            in

            let err = Errors.span_err sess in
            let open Utils in

            let defs =
                List.fold_left
                    (fun acc (pat, expr) ->
                        Maybe.bind3
                            acc
                            (Ast.check_pat err pat)
                            (Ast.check err expr)
                            (fun (acc, env) pat expr ->
                                let nenv = Typer.def_infer sess env pat expr in
                                Maybe.return @@
                                    ((true,
                                     Codegen.from_pat pat,
                                     Codegen.from_ast expr)
                                    :: acc, nenv @ env)))
                    (Maybe.return ([], init_ty_env)) defs

            in begin match defs with
                | Some (defs, _) ->
                    let md = Codegen.codegen_module mod_name (List.rev defs) in
                    Llvm.print_module (mod_name ^ ".out") md ;
                    `Ok
                | None -> `Err
            end

        | `Expr expr ->
            Errors.span_err sess sp "expressions are not allowed at toplevel" ;
            `Err

let () =
    let pname = Sys.argv.(0) in
    match List.tl @@ Array.to_list Sys.argv with
        | [file] ->
            begin match compile_and_save file with
                | `Ok -> exit 0
                | `Err -> exit 1
            end
        | [] -> Rtpl.run init_ty_env ()
        | _ -> Printf.fprintf stderr "usage: %s [FILE]\n" pname ; exit 1
