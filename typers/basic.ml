(*
 * Provides common definitions used by all `basic' typers (currently `simple',
 * `core' and `poly'). Those typers use a subset of the AST that allow only
 * simple constructions (e.g. no sum types).
 * This module also provides functions to convert from a complete AST to a
 * basic AST, reporting unsupported constructions as errors, if any.
 *)

module Maybe = Utils.Maybe

type 'a s_pat = [
    | `PatUnit
    | `PatWildcard
    | `PatCst     of int
    | `PatVar     of string
    | `PatTup     of 'a pat list
] and 'a pat = ('a * ('a s_pat) Codemap.spanned)

type 'a s_ast = [
    | `Unit
    | `Cst     of int
    | `Var     of string
    | `Tuple   of 'a ast list
    | `If      of 'a ast * 'a ast * 'a ast
    | `Fun     of 'a pat * 'a ast
    | `Let     of 'a pat * 'a ast * 'a ast
    | `Match   of 'a ast * ('a pat * 'a ast) list
    | `Apply   of 'a ast * 'a ast
    | `BinOp   of Ast.bin_op * 'a ast * 'a ast
] and 'a ast  = ('a * ('a s_ast) Codemap.spanned)

type input_pat = [ `Unty ] pat
type input_ast = [ `Unty ] ast

(*
 * Those functions try to convert the AST into a more restricted type
 * and report an error if they find a construction that is not
 * allowed in the corresponding language level
 *)

let rec from_pat on_error ((ty, (sp, pat)): Ast.input_pat): input_pat Maybe.t =
    let open Utils.Maybe in
    let pat = match pat with
        | (`PatUnit     as a)
        | (`PatWildcard as a)
        | (`PatCst _    as a)
        | (`PatVar _    as a) -> return a

        | `PatTup lst         ->
            map_m (from_pat on_error) lst >>= (fun l -> return (`PatTup l))

        | _                   ->
            on_error sp "unsupported construction" ;
            None
    in match ty with
        | `Annot _ ->
            on_error sp "this typer does not allow explicit type annotations" ;
            None
        | `Unty -> pat >>= (fun p -> return (`Unty, (sp, p)))

let rec from_ast on_error ((ty, (sp, ast)): Ast.input_ast): input_ast Maybe.t =
    let open Utils.Maybe in
    let from_ast = from_ast on_error in
    let from_pat = from_pat on_error in
    let ast = match ast with
        | (`Unit  as a)
        | (`Cst _ as a)
        | (`Var _ as a)          -> return a

        | `Tuple lst             ->
            map_m from_ast lst >>= (fun l -> return (`Tuple l))

        | `If (ec, et, ef)       ->
            bind3 (from_ast ec) (from_ast et) (from_ast ef)
                  (fun ec et ef -> return (`If (ec, et, ef)))

        | `Fun (pat, expr)       ->
            bind2 (from_pat pat) (from_ast expr)
                  (fun pat expr -> return (`Fun (pat, expr)))

        | `Let (pat, expr, body) ->
            bind3 (from_pat pat) (from_ast expr) (from_ast body)
                  (fun pat expr body -> return (`Let (pat, expr, body)))

        | `Match (expr, arms)    ->
            bind2 (from_ast expr)
                  (map_m (fun (p, e) ->
                              bind2 (from_pat p) (from_ast e)
                                    (fun p e -> return (p, e)))
                         arms)
                  (fun expr arms -> return (`Match (expr, arms)))

        | `Apply (fn, arg)       ->
            bind2 (from_ast fn) (from_ast arg)
                  (fun fn arg -> return (`Apply (fn, arg)))

        | `BinOp (op, opl, opr)  ->
            bind2 (from_ast opl) (from_ast opr)
                  (fun opl opr -> return (`BinOp (op, opl, opr)))

        |  _                     ->
            on_error sp "unsupported construction" ;
            None
    in match ty with
        | `Annot _ ->
            on_error sp "this typer does not allow explicit type annotations" ;
            None
        | `Unty -> ast >>= (fun a -> return (`Unty, (sp, a)))
