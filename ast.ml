(* common type definitions for the AST *)

(* a binary arithmetic operator *)
type bin_op = [
    | `Eq
    | `Cons
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
type s_pattern = [
    | `PatUnit
    | `PatWildcard
    | `PatCst     of int
    | `PatVar     of Ident.t
    | `PatTup     of pattern list
] and pattern = s_pattern Codemap.spanned

type s_basic_pattern = [
    | `PatUnit
    | `PatWildcard
    | `PatCst     of int
    | `PatVar     of Ident.t
    | `PatTup     of basic_pattern list
] and basic_pattern = s_basic_pattern Codemap.spanned

(* internal representation of the AST *)

type s_ast = [
    | `Unit
    | `Cst     of int
    | `Var     of Ident.t
    | `Tuple   of ast list
    | `List    of ast list
    | `If      of ast * ast * ast
    | `Fun     of pattern * ast
    | `Let     of bool * pattern * ast * ast
    | `Match   of ast * (pattern * ast) list
    | `Apply   of ast * ast
    | `BinOp   of bin_op * ast * ast
] and ast  = s_ast Codemap.spanned

let rec expr_of_pat (sp, pat) = (sp, match pat with
    | `PatUnit        -> `Unit
    | `PatWildcard    -> failwith "fixme: this shouldn't exist"
    | `PatCst c       -> `Cst c
    | `PatVar v       -> `Var v
    | `PatTup t       -> `Tuple (List.map expr_of_pat t))

(* a possibly errorenous AST *)
(* TODO: way too much ASTs *)

type s_err_pattern = [
    | `PatUnit
    | `PatWildcard
    | `PatCst     of int
    | `PatVar     of Ident.t
    | `PatTup     of err_pattern list
    | `ParseError of string
] and err_pattern = s_err_pattern Codemap.spanned

type s_err_ast = [
    | `Unit
    | `Cst        of int
    | `Var        of Ident.t
    | `Tuple      of err_ast list
    | `List       of err_ast list
    | `If         of err_ast * err_ast * err_ast
    | `Fun        of err_pattern * err_ast
    | `Let        of bool * err_pattern * err_ast * err_ast
    | `Match      of err_ast * (err_pattern * err_ast) list
    | `Apply      of err_ast * err_ast
    | `BinOp      of bin_op * err_ast * err_ast
    | `ParseError of string
] and err_ast  = s_err_ast Codemap.spanned

type defs = [
    | `Defs of (err_pattern * err_ast) list
    | `Expr of err_ast
    | `ParseError of string
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

let rec check_pat err ((sp, pat): err_pattern) : pattern Utils.Maybe.t =
  let open Utils.Maybe in
  match pat with
    | (`PatUnit     as a)
    | (`PatWildcard as a)
    | (`PatCst _    as a)
    | (`PatVar _    as a) -> return (sp, a)

    | `PatTup pats        ->
        map_m (check_pat err) pats >>= (fun lst -> return (sp, `PatTup lst))

    | `ParseError e       -> err sp e ; None

let rec check err ((sp, ast): err_ast) : ast option =
    let open Utils.Maybe in match ast with
        | (`Unit  as a)
        | (`Cst _ as a)
        | (`Var _ as a)          -> return (sp, a)

        | `Tuple asts            ->
            map_m (check err) asts >>= (fun lst -> return (sp, `Tuple lst))

        | `List asts             ->
            map_m (check err) asts >>= (fun lst -> return (sp, `List lst))

        | `If (ec, et, ef)       ->
            bind3 (check err ec) (check err et) (check err ef)
                  (fun ec et ef -> return (sp, `If (ec, et, ef)))

        | `Fun (pat, expr)       ->
            bind2 (check_pat err pat) (check err expr)
                  (fun p e -> return (sp, `Fun (p, e)))

        | `Let (isrec, pat, expr, body) ->
            bind3 (check_pat err pat) (check err expr) (check err body)
                  (fun p e b -> Some (sp, `Let (isrec, p, e, b)))

        | `Match (expr, arms)    ->
            bind2 (check err expr)
                  (map_m (fun (p, a) ->
                              bind2 (check_pat err p) (check err a)
                                    (fun p a -> return (p, a)))
                         arms)
                  (fun expr arms -> return (sp, `Match (expr, arms)))

        | `Apply (fn, arg)       ->
            bind2 (check err fn) (check err arg)
                  (fun f a -> return (sp, `Apply (f, a)))

        | `BinOp (op, opl, opr)  ->
            bind2 (check err opl) (check err opr)
                  (fun l r -> return (sp, `BinOp (op, l, r)))

        | `ParseError e          -> err sp e ; None

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
    | `PatVar v           -> sprintf "(%s)" (Ident.show v)
    | `PatTup tup         -> sprintf "(%s)" (Utils.string_of_list tup ~sep:", "
                                                                  pat_to_string)

(* Returns a fully-parenthesized string representation of the AST *)
let rec ast_to_string ast = match (snd ast) with
    | `Unit               -> "(unit)"
    | `Cst i              -> sprintf "(%s)" (string_of_int i)
    | `Var v              -> sprintf "(%s)" (Ident.show v)
    | `Tuple tup          -> sprintf "(%s)" (Utils.string_of_list tup ~sep:", "
                                                                  ast_to_string)
    | `List  lst          -> sprintf "[%s]" (Utils.string_of_list lst ~sep:"; "
                                                                  ast_to_string)

    | `If (e1, e2, e3)    ->
        sprintf "(if %s then %s else %s)"
                (ast_to_string e1) (ast_to_string e2) (ast_to_string e3)

    | `Fun (pat, expr)    ->
        sprintf "(fun %s -> %s)" (pat_to_string pat) (ast_to_string expr)

    | `Let (isrec, pat, e1, e2)  ->
        sprintf "(let %s%s = %s in %s)"
                (if isrec then "rec " else "")
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
