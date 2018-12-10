(* Read-Type-Print-Loop *)

let process_phrase (env : Typer.env) : Typer.env =
    let phrase = Parse.toplevel_phrase (Lexing.from_channel stdin) in
    match phrase with
        | Ptop_def strukt ->
            List.iter
                (fun si ->
                     match si.Parsetree.pstr_desc with
                         | Pstr_eval (expr, _) ->
                             let sty = Typer.infer env expr in
                             Format.printf "type: %s\n" (Printer.show_sch (Hmx.sch sty)) ;
                             Format.pp_print_flush Format.std_formatter ()
                         | _ -> failwith "notexpr") strukt ;
            env
        | Ptop_dir _ -> env

let rec rtpl (env : Typer.env) =
    Printf.printf "λ > " ;
    flush stdout ;
    match process_phrase env with
        | exception End_of_file -> ()
        | exception e ->
            Location.report_exception (Format.err_formatter) e ;
            rtpl env

        | env ->
            rtpl env

let run (init_ty_env : Typer.env) =
    Printf.printf "\t\tRead Type Print Loop version 0.1. \n\n" ; rtpl init_ty_env

let compile_and_save path =
    `Ok

let () =
    let pname = Sys.argv.(0) in
    match List.tl @@ Array.to_list Sys.argv with
        | [file] ->
            begin match compile_and_save file with
                | `Ok -> exit 0
                | `Err -> exit 1
            end
        | [] -> run Typer.init_ty_env
        | _ -> Printf.fprintf stderr "usage: %s [FILr]\n" pname ; exit 1
