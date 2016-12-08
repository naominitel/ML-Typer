(* W Algorithm for the Hindley-Milner type system *)

type kind = Concrete | Constructor

module TyPrim = struct
    open Codemap

    (* primitive types and type contructors *)
    let arrow = dummy_spanned @@ Ast.Var (Ident.intern "->")
    let int_  = dummy_spanned @@ Ast.Var (Ident.intern "int")
    let bool_ = dummy_spanned @@ Ast.Var (Ident.intern "bool")

    let (-->) ty_arg ty_body = Ast.curry arrow [ty_arg ; ty_body]

    let initial_ty_env = [
        (arrow, Constructor) ;
        (int_,  Concrete) ;
        (bool_, Concrete)
    ]
end

let free_ty_var =
    let c = ref 0 in
    fun () ->
        let name = Printf.sprintf "a%d" !c in
        incr c ; Codemap.dummy_spanned @@ Ast.Var (Ident.make name)

open Ast
open Codemap
open TyPrim

(*
 * a bunch of primitives that need to be hard-typed because proper typing
 * would require polymorphic types...
 *)

let if_ = Ident.intern "if"         (* all a. bool -> a -> a -> a *)

type ty = (string, unit) Ast.ast

let rec (|-) env expr : (ty * 'a list) =
    match expr.d with
        | Const v -> failwith "unimpl"
        | Var v ->
            (* constants and variables *)
            begin try
                let _ = int_of_string (Ident.show v) in (int_, [])
            with Failure _ ->
                try (List.assoc v env, [])
                with Not_found ->
                    failwith (Printf.sprintf "unknown variable %s" (Ident.show v))
            end
        | App ({ d = Abs (arg, annot, body) ; _ }, expr) ->
            (* ß-redex, let, generalize *)
            let (ty_expr, s0) = env |- expr in
            let (ty_body, s1) = ((arg.d, ty_expr) :: env) |- body in
            (ty_body, s0 @ s1)
        | App ({ d = Ast.Var func ; _ }, expr) when Ident.eq func if_ ->
            (* if *)
            let (ty_arg, s1) = env |- expr in
            let ty = free_ty_var () in
            (ty --> (ty --> ty), (ty_arg, bool_) :: s1)
        | App (func, expr) ->
            (* standard application *)
            let (ty_fun, s0) = env |- func in
            let (ty_arg, s1) = env |- expr in
            let ty_ret = free_ty_var () in
            (ty_ret, (ty_fun, ty_arg --> ty_ret) :: s0 @ s1)
        | Abs (arg, annot, body) ->
            let ty_arg = free_ty_var () in
            let (ty_body, sys) = ((arg.d, ty_arg) :: env) |- body in
            (ty_arg --> ty_body, sys)

let init_prog_env = [
    (Ident.intern "true", bool_) ;
    (Ident.intern "false", bool_) ;
    (Ident.intern "+", int_ --> int_) ;
    (Ident.intern "-", int_ --> int_) ;
    (Ident.intern "=", int_ --> (int_ --> bool_))
]

let rec from_ast ast =
    { ast with d = match ast.d with
        | Var v -> Var v
        | Const c -> Const c
        | App (func, arg) -> App (from_ast func, from_ast arg)
        | Abs (arg, None, expr) -> Abs (arg, None, from_ast expr)
        | Abs (arg, Some _, expr) ->
            failwith "higher-kinded types are not supported here"
    }

let rec show t = match t.d with
    | Const c -> c
    | Var v -> Ident.show v
    | App (e1, e2) -> Printf.sprintf "(%s %s)" (show e1) (show e2)
    | Abs (v, _, body) -> Printf.sprintf "(\\ %s : * . %s)" (Ident.show v.d) (show body)

type exn += TypeError of string

let type_err err =
    let msg = match err with
        | `Occur (t1, t2) ->
            let t1 = Ident.show t1 in
            let t2 = show t2 in
            Printf.sprintf "an expression has type %s but an expression was
                            expected of type %s.\n
                            %s occurs in %s"
                t2 t1 t1 t2
        | `Mismatch (t1, t2) ->
            let (t1, t2) = (show t1, show t2) in
            Printf.sprintf "an expression has type %s but an expression was
                            expected of type %s."
                t1 t2
        | `Undef v -> Printf.sprintf "unbound variable %s" @@ Ident.show v
    in raise (TypeError msg)

let apply_subst subst sys =
    let rec apply ((var, term) as s) t = Codemap.dummy_spanned @@ match t.d with
        | Var v when v = var -> term
        | App (f, a) -> App (apply s f, apply s a)
        | Abs (v, _, a) -> Abs (v, None, apply s a)
        | Var _ | Const _ -> t.d
    in List.map (fun (t1, t2) -> (apply subst t1, apply subst t2)) sys

let rec occur_check var term = match term.d with
    | Var v when v = var -> true
    | Var _ | Const _  -> false
    | Abs (_, _, a) -> occur_check var a
    | App (f, a) -> occur_check var f || occur_check var a

let rec unify equ_sys = match equ_sys with
    | [] -> []
    | ((t1, t2) :: sys) ->
        begin match (t1.d, t2.d) with
            | (x, y) when x = y -> unify sys
            | (Var x, y)
            | (y, Var x) ->
                if occur_check x t2 then 
                    type_err (`Occur (x, y))
                else
                    let subst = (x, y) in
                    let sys = apply_subst subst sys in
                    subst :: unify sys
            | (App (f1, a1), App (f2, a2)) -> unify @@ (f1, f2) :: (a1, a2) :: equ_sys
            | (x, y) -> type_err (`Mismatch (x,  y))
        end

let primitives = []

let run_typer expr =
    let env = init_prog_env in
    let (ty, constrs) = env |- expr in

    Printf.printf "constraints:\n" ;
    List.iter
        (fun (t1, t2) -> Printf.printf "    %s = %s\n" (show t1) (show t2))
        constrs ;

    (* now run constraint solving *)
    ty
