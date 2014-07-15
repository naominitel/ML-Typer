module Typer = Typers.Poly

let run_typer inp out =
    let open Utils.Maybe in
    let open Printf in
    let env = ref [] in
    (fun _ -> (try
        let print s =
            let str = ref "" in
            String.iter
                (fun c -> str := !str ^
                    if c == '\n' then "<br />" else String.make 1 c)
                s ;
            out##innerHTML <- Js.string ((Js.to_string out##innerHTML) ^ !str)
        in

        let err sess sp msg =
            let open Codemap in
            let open Session in
            let (s, e) = sp in
            let file = sess.cm.(s.fileno) in
            print (sprintf "%s:%d:%d: %d:%d \x1b[1;31merror: \x1b[1;37m%s\x1b[0m\n"
                           file.name s.line s.col e.line e.col msg) ;
            if s.line = e.line then
                (*
                * The errorneous expression spans over a single line
                * Draw an arrow to point the exact position
                *)
                let line = pos_line sess s in
                let pre  = Printf.sprintf "%s:%d" file.name s.line in
                let pad  = String.make (String.length pre + s.col) ' ' in
                let arr  = String.make (e.col - s.col - 1) '~' in
                print (sprintf "%s %s\n" pre line) ;
                print (sprintf "%s\x1b[1;31m^%s\x1b[0m\n" pad arr)
            else for i = s.line to e.line do
                let line = line sess s.fileno i in
                print (sprintf "%s:%d %s\n" file.name i line)
            done
        in

        let input = Js.to_string inp##value in
        let (sess, (sp, defs)) = Session.sess_open_str input in
        match defs with
            | `ParseError e -> err sess sp e
            | `Defs [] -> ()

            | `Defs defs ->
                let new_env =
                    (List.fold_left
                        (fun local_env (pat, expr) ->
                            let err = err sess in
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
                                                            (local_env @ !env)
                                                            pat ast in
                                            return (nenv @ local_env))))
                        (return []) defs)
                in (match new_env with
                        | None         -> ()
                        | Some new_env ->
                            List.iter
                                (fun (str, ty) ->
                                    print (sprintf
                                               "%s: %s\n" str
                                                (Typer.ty_to_string ty)))
                                new_env ;
                            env := (new_env @ !env))

            | `Expr ast ->
                ignore (
                    (Ast.check (err sess) ast) >>=
                        (fun ast ->
                            Typer.from_ast (err sess) ast >>=
                                (fun ast ->
                                    let sty = Typer.infer sess !env ast in
                                    print (sprintf
                                               "type: %s\n"
                                               (Typer.ty_to_string sty)) ;
                                    None )))
        with Errors.CompileFailure -> ()) ;
        Js._false
    )

let () =
    let document = Dom_html.document in
    let main =
        Js.Opt.get
            (document##getElementById (Js.string "main"))
            (fun _ -> failwith "eh")
    in

    let inp = Dom_html.createTextarea document in
    let btn = Dom_html.createButton document in
    let out = Dom_html.createDiv document in
    inp##value <- Js.string "def a = 0 ;;" ;
    btn##innerHTML <- Js.string "type!" ;
    btn##onclick <- Dom_html.handler (run_typer inp out) ;

    Dom.appendChild main inp ;
    Dom.appendChild main (Dom_html.createBr document) ;
    Dom.appendChild main btn ;
    Dom.appendChild main (Dom_html.createBr document) ;
    Dom.appendChild main out ;
