module DummyTyper: Typer.S = struct
    type ty = unit

    let from_ast _ = ()
    let show _ = "()"
end

module DummyAst = Typer.MakeAst(DummyTyper)

let rec iter env =
    Printf.printf "µλ > " ;
    flush stdout ;
    (*(try*)
    let (sess, ast) = Session.sess_open () in
         (*with Errors.CompileFailure -> iter env)*)
    match ast with
        | Some ast ->
            let ast = DummyAst.from_frontend_ast ast in
            print_endline @@ DummyAst.show ast ;
            iter env
        | None -> ()

let () = iter []
