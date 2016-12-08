(* W Algorithm for the Hindley-Milner type system *)

type kind = Concrete | Constructor

module TyPrim = struct
    (* primitive types and type contructors *)
    let arrow = Ast.Var (Ident.intern "->")
    let int_  = Ast.Var (Ident.intern "int")
    let bool_ = Ast.Var (Ident.intern "bool")

    let (-->) ty_arg ty_body = Ast.curry (arrow, [ty_arg ; ty_body])

    let initial_ty_env = [
        (arrow, Constructor) ;
        (int_,  Concrete) ;
        (bool_, Concrete)
    ]
end

type basic_ty = unit Ast.ast

type ty_scheme =
    | Basic of basic_ty
    | Forall of Ident.t * basic_ty

let free_ty_var = Ident.gensym

open TyPrim

let (|-) env (expr, ty) =
    match expr with
        | Var v -> Inst (x, ty)
        | App (Abs (arg, annot, body), expr) ->
            (* ß-redex, let, generalize *)
            let x = fresh_ty_var () in
            Let (z, Forall (x, infer expr x, Ast.Var x), infer body ty)
        | App (func, expr) ->
            (* standard application *)
            let x2 = fresh_ty_var () in
            Exists (x2, (infer func (x2 --> ty)) ^ (infer t2 x2))
        | Abs (arg, annot, body) ->
            let ty_arg = Var (fresh_ty_var ()) in
            let ty_body = infer_constrants ((arg, ty_arg) :: env) body
            in exists [x1 ; x2]
                Let (arg, x1, (infer body ty) ^ (SubTy (ty_arg --> ty_body) ty))

let run_typer expr =
    let env = build_init_env () in
    
    (*
     * t is well typed in env iff for any type variable X that does not
     * appear free in env, the constraint [[env |- t : X]] is satisfiable.
     * such a constraint is determined with the algorithm above.
     * t is thus the result of the type inference and the constraint
     * contains information about it.
     *)
    let ty = free_ty_var () in
    let constraints = env |- (expr, ty) in

    (* now run constraint solving *)
