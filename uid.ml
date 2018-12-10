type t = string

let dummy_id = "#gensym#"
let gensym () = String.copy dummy_id

let show ident = ident

(* physical equality because strings are interned *)

let eq = (==)

let interner: (string, string) Hashtbl.t =
    Hashtbl.create 100

let intern ident =
    try (Hashtbl.find interner ident)
    with Not_found -> begin
        Hashtbl.add interner ident ident ;
        ident
    end
