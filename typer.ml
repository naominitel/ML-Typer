open Hmx_types

let tuple = Hmx.tuple_type 2

(*
 * FIXME: should be linked to codegen somehow. and should probably be
 * elsewhere
 *)
let init_ty_env = [
    (Uid.intern "hd",
     let v = Hmx_types.fresh_ty_var () in
     let lst = Hmx_types.TApp (Hmx_types.t_list, [TVar v]) in
     Hmx.Forall ([v], Hmx.CBool true, Hmx_types.function_type lst (Hmx_types.TVar v))) ;

    (Uid.intern "tl",
     let v = Hmx_types.fresh_ty_var () in
     let lst = Hmx_types.TApp (Hmx_types.t_list, [TVar v]) in
     Hmx.Forall ([v], Hmx.CBool true, Hmx_types.function_type lst lst)) ;

    (Uid.intern "fst",
     let x = Hmx_types.fresh_ty_var () in
     let y = Hmx_types.fresh_ty_var () in
     let tup = Hmx_types.TApp (tuple, [TVar x ; TVar y]) in
     Hmx.Forall ([x ; y], Hmx.CBool true, Hmx_types.function_type tup (Hmx_types.TVar x))) ;

    (Uid.intern "snd",
     let x = Hmx_types.fresh_ty_var () in
     let y = Hmx_types.fresh_ty_var () in
     let tup = Hmx_types.TApp (tuple, [TVar x ; TVar y]) in
     Hmx.Forall ([x ; y], Hmx.CBool true, Hmx_types.function_type tup (Hmx_types.TVar y))) ;

    (Uid.intern "print_int",
     Hmx.sch @@ Hmx_types.function_type Hmx_types.t_int Hmx_types.t_unit) ;

    (Uid.intern "[]",
     let x = Hmx_types.fresh_ty_var () in
     Hmx.Forall ([x], Hmx.CBool true, (TApp (Hmx_types.t_list, [TVar x])))) ;

    (Uid.intern "==",
     let x = Hmx_types.fresh_ty_var () in
     Hmx.Forall ([x], Hmx.CBool true,
                 Hmx_types.function_type
                     (Hmx_types.TVar x)
                     (Hmx_types.function_type
                          (Hmx_types.TVar x)
                          Hmx_types.t_bool))) ;
]

type env = (Uid.t * Hmx.ty_sch) list

let infer (env : env) ast =
    let tbl = Arity.run ast in
    let ty = TVar (Hmx_types.fresh_ty_var ()) in
    let constr = Hmx.infer tbl ast ty in
    ignore @@ Solver.run env constr ;
    ty

let def_infer isrec env vbs =
    let tbl = match vbs with
        | [] -> assert false
        | [b] -> Arity.run b.Parsetree.pvb_expr
        | b :: _ -> raise @@ Hmx.Unimpl (b.Parsetree.pvb_loc, "let and")
    in
    let constr = Hmx.infer_def tbl isrec vbs in
    let env = Solver.run env constr in
    env
