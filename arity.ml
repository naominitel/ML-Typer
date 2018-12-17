(* infer arity of expressions:
 * - a function has arity 1 ;
 * - a chain of functions fun a -> fun b ... has arity n
 * - higher-order functions are *not* supported, so any
 *   other expression kind has arity 0
 * - only lets are descended into to support patterns like this:
 *   let counter =
 *       let inner = ref 0 in
 *       fun () -> incr inner ; !inner
 * results of the analysis are stored in an hashtbl indexed by nodes. *)

module ASTMap =
    Hashtbl.Make (struct
        type t = Parsetree.expression
        let equal = (==)
        let hash = Hashtbl.hash
    end)

let rec infer tbl expr =
    let arity = match expr.Parsetree.pexp_desc with
        | Pexp_construct (_, Some arg) -> ignore @@ infer tbl arg ; 0
        | Pexp_construct (_, None) -> 0
        | Pexp_ident _ -> 0
        | Pexp_tuple tup -> ignore @@ List.map (infer tbl) tup ; 0
        | Pexp_ifthenelse (ec, et, Some ef) ->
            ignore @@ infer tbl ec ;
            ignore @@ infer tbl et ;
            ignore @@ infer tbl ef ;
        | Pexp_ifthenelse (ec, et, None) ->
            ignore @@ infer tbl ec ;
            ignore @@ infer tbl et
        | Pexp_fun (_, _, _, body) -> 1 + infer tbl body
        | Pexp_let (_, vbs, body) -> ignore @@ (infer tbl) infer tbl body
        | Pexp_apply (_, _, body) -> ignore  infer tbl body
        | _ -> 0
    in ASTMap.replace tbl expr arity ; arity

let run ast =
    let tbl = ASTMap.create 128 in
    let _ = infer ast tbl in
    tbl
