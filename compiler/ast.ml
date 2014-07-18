(* common type definitions for the AST *)

(* a binary arithmetic operator *)
type bin_op = [
    | `Plus
    | `Minus
    | `Mult
    | `Div
]

(*
 * pattern of a match arm, or of the left-hand
 * side of a let ... in form, or of a function
 * argument
 *)
type 'a s_pat = [
    | `PatUnit
    | `PatWildcard
    | `PatCst     of int
    | `PatVar     of string
    | `PatCtor    of string * 'a pat
    | `PatTup     of 'a pat list
] and 'a pat = ('a * ('a s_pat) Codemap.spanned)

(* internal representation of the AST *)

type 'a s_ast = [
    | `Unit
    | `Cst     of int
    | `Var     of string
    | `Ctor    of string * 'a ast
    | `Tuple   of 'a ast list
    | `If      of 'a ast * 'a ast * 'a ast
    | `Fun     of 'a pat * 'a ast
    | `Let     of 'a pat * 'a ast * 'a ast
    | `Match   of 'a ast * ('a pat * 'a ast) list
    | `Apply   of 'a ast * 'a ast
    | `BinOp   of bin_op * 'a ast * 'a ast
] and 'a ast  = ('a * ('a s_ast) Codemap.spanned)

type error_pat = [ `Unty | `Annot of Types.ty | `Error of string ] pat
type error_ast = [ `Unty | `Annot of Types.ty | `Error of string ] ast
type input_pat = [ `Unty | `Annot of Types.ty ] pat
type input_ast = [ `Unty | `Annot of Types.ty ] ast
type typed_ast = [ `Ty of Types.ty ] ast

type defs = [
    | `Defs of (error_pat * error_ast) list
    | `Expr of error_ast
] Codemap.spanned

(*
 * run the parser and checks the returned AST from parse errors
 * returns an AST that is guaranteed by typing not to contain
 * any errorneous construction, or None, if parse errors
 * has been found.
 * Calls the given callback on each error encountered, which
 * may be used to just report the error, or do something else,
 * accordingly.
 * TODO: if only we had monads... implement in Utils?
 *)

let rec check_pat err ((t, (sp, pat)): error_pat) : input_pat Utils.Maybe.t =
    let open Utils.Maybe in
    let aux () = match pat with
        | (`PatUnit     as a)
        | (`PatWildcard as a)
        | (`PatCst _    as a)
        | (`PatVar _    as a) -> return a

        | `PatCtor (v, pat) ->
            (check_pat err pat) >>= (fun p -> return (`PatCtor (v, p)))

        | `PatTup pats ->
            map_m (check_pat err) pats >>=
                (fun lst -> return (`PatTup lst))
    in match t with
        | `Error e -> err sp e ; None
        | (`Annot _ | `Unty) as t ->
            aux () >>= (fun p -> return (t, (sp, p)))

let rec check err ((t, (sp, ast)): error_ast) : input_ast option =
    let open Utils.Maybe in
    let aux () = match ast with
        | (`Unit  as a)
        | (`Cst _ as a)
        | (`Var _ as a)          -> return a

        | `Ctor (v, arg)         ->
            (check err arg) >>= (fun a -> return (`Ctor (v, a)))

        | `Tuple asts            ->
            map_m (check err) asts >>= (fun lst -> return (`Tuple lst))

        | `If (ec, et, ef)       ->
            bind3 (check err ec) (check err et) (check err ef)
                  (fun ec et ef -> return (`If (ec, et, ef)))

        | `Fun (pat, expr)       ->
            bind2 (check_pat err pat) (check err expr)
                  (fun p e -> return (`Fun (p, e)))

        | `Let (pat, expr, body) ->
            bind3 (check_pat err pat) (check err expr) (check err body)
                  (fun p e b -> Some (`Let (p, e, b)))

        | `Match (expr, arms)    ->
            bind2 (check err expr)
                  (map_m (fun (p, a) ->
                               bind2 (check_pat err p) (check err a)
                                     (fun p a -> return (p, a)))
                          arms)
                  (fun expr arms -> return (`Match (expr, arms)))

        | `Apply (fn, arg)       ->
            bind2 (check err fn) (check err arg)
                  (fun f a -> return (`Apply (f, a)))

        | `BinOp (op, opl, opr)  ->
            bind2 (check err opl) (check err opr)
                  (fun l r -> return (`BinOp (op, l, r)))
    in match t with
        | `Error e -> err sp e ; None
        | (`Annot _ | `Unty) as t ->
            aux () >>= (fun a -> return (t, (sp, a)))

(* not-so-pretty-printing functions, for debug purposes *)

let binop_to_string op = match op with
    | `Plus  -> "+"
    | `Minus -> "-"
    | `Mult  -> "*"
    | `Div   -> "/"

let sprintf = Printf.sprintf

(* Returns a fully-parenthesized string representation of a pattern *)
let rec pat_to_string pat = match (snd pat) with
    | `PatUnit            -> "(unit)"
    | `PatWildcard        -> "_"
    | `PatCst i           -> sprintf "(%s)" (string_of_int i)
    | `PatVar v           -> sprintf "(%s)" v
    | `PatCtor (str, arg) -> sprintf "(%s %s)" str (pat_to_string arg)
    | `PatTup tup         -> sprintf "(%s)" (Utils.string_of_list tup ~sep:", "
                                                                  pat_to_string)

(* Returns a fully-parenthesized string representation of the AST *)
let rec ast_to_string ast = match (snd ast) with
    | `Unit               -> "(unit)"
    | `Cst i              -> sprintf "(%s)" (string_of_int i)
    | `Var v              -> sprintf "(%s)" v
    | `Ctor (str, ast)    -> sprintf "(%s %s)" str (ast_to_string ast)
    | `Tuple tup          -> sprintf "(%s)" (Utils.string_of_list tup ~sep:", "
                                                                  ast_to_string)

    | `If (e1, e2, e3)    ->
        sprintf "(if %s then %s else %s)"
                (ast_to_string e1) (ast_to_string e2) (ast_to_string e3)

    | `Fun (pat, expr)    ->
        sprintf "(fun %s -> %s)" (pat_to_string pat) (ast_to_string expr)

    | `Let (pat, e1, e2)  ->
        sprintf "(let %s = %s in %s)"
                (pat_to_string pat) (ast_to_string e1) (ast_to_string e2)

    | `Match (expr, arms) ->
        sprintf "(match %s with %s)" (ast_to_string expr)
                (Utils.string_of_list
                    arms ~sep:" | " (fun (pat, ast) ->
                                         (sprintf "%s -> %s"
                                             (pat_to_string pat)
                                             (ast_to_string ast))))

    | `BinOp (op, e1, e2) ->
        sprintf "(%s %s %s)"
                (ast_to_string e1) (binop_to_string op) (ast_to_string e2)

    | `Apply (func, arg)  ->
        sprintf "(%s %s)" (ast_to_string func) (ast_to_string arg)
