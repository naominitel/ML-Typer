(*
module DummyTyper: Typer.S = struct
    type ty = unit

    let from_ast _ = ()
    let show _ = "()"

    let primitives = [
        Primitives.if_ ;
    ] @ Primitives.basic_constants
end
*)

module DummyAst = Typer.MakeAst(Stlc)

let rec iter env =
    Printf.printf "µλ > " ;
    flush stdout ;

    let (sess, ast) = Session.sess_open () in
    match ast with
        | Some ast ->
            let ast = DummyAst.from_frontend_ast ast in
            let ty = Stlc.run_typer ast in
            Printf.printf "- : %s = %s\n" (Stlc.show ty) (DummyAst.show ast) ;
            iter env
        | None -> ()

let () = iter []
