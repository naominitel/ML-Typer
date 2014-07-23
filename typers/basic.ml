open Codemap

(*
 * Provides common definitions used by all `basic' typers (currently `simple',
 * `core' and `poly'). Those typers use a subset of the AST that allow only
 * simple constructions (e.g. no sum types).
 * This module also provides functions to convert from a complete AST to a
 * basic AST, reporting unsupported constructions as errors, if any.
 *)

type 'a pat = ('a, [
    | `PatUnit
    | `PatWildcard
    | `PatCst     of int
    | `PatVar     of string
    | `PatTup     of 'a pat list
]) Codemap.lr_spanned

type 'a ast  = ('a, [
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
]) Codemap.lr_spanned

type input_pat = [ `Unty ] pat
type input_ast = [ `Unty ] ast

(*
 * Those functions try to convert the AST into a more restricted type
 * and report an error if they find a construction that is not
 * allowed in the corresponding language level
 *)

let rec from_pat (s_pat: Ast.input_pat) : input_pat Errors.t =
    let open Errors in
    let pat = match s_pat.r with
        | (`PatUnit     as a)
        | (`PatWildcard as a)
        | (`PatCst _    as a)
        | (`PatVar _    as a) -> Ok a

        | `PatTup lst ->
            map (fun l -> `PatTup l) (sequence (List.map from_pat lst))

        | _ -> new_err s_pat.sp "unsupported construction"
    in match s_pat.l with
        | `Annot _ ->
            new_err s_pat.sp "this typer does not allow explicit type annotations"
        | `Unty -> map (fun p -> { l = `Unty ; sp = s_pat.sp ; r = p }) pat

let rec from_ast (s_ast: Ast.input_ast): input_ast Errors.t =
    let open Errors in
    let ast = match s_ast.r with
        | (`Unit  as a)
        | (`Cst _ as a)
        | (`Var _ as a) -> Ok a

        | `Tuple lst ->
            map (fun l -> `Tuple l) (sequence (List.map from_ast lst))

        | `If (ec, et, ef) ->
            map3 (fun ec et ef -> `If (ec, et, ef))
                 (from_ast ec) (from_ast et) (from_ast ef)

        | `Fun (pat, expr) ->
            map2 (fun pat expr -> `Fun (pat, expr))
                 (from_pat pat) (from_ast expr)

        | `Let (pat, expr, body) ->
            map3 (fun pat expr body -> `Let (pat, expr, body))
                 (from_pat pat) (from_ast expr) (from_ast body)

        | `Match (expr, arms) ->
            map2 (fun expr arms -> `Match (expr, arms))
                 (from_ast expr)
                 (sequence (List.map (fun (p, e) -> map2 (fun p e -> (p, e))
                                                   (from_pat p) (from_ast e))
                                     arms))

        | `Apply (fn, arg) ->
            map2 (fun fn arg -> `Apply (fn, arg))
                 (from_ast fn) (from_ast arg)

        | `BinOp (op, opl, opr) ->
            map2 (fun opl opr -> `BinOp (op, opl, opr))
                 (from_ast opl) (from_ast opr)

        | _ -> new_err s_ast.sp "unsupported construction"
    in match s_ast.l with
        | `Annot _ ->
            new_err s_ast.sp "this typer does not allow explicit type annotations"
        | `Unty -> map (fun a -> { l = `Unty ; sp = s_ast.sp ; r = a }) ast
