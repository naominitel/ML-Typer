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
     Hmx.sch @@ Hmx_types.function_type Hmx_types.t_int Hmx_types.t_unit)
]

type env = (Uid.t * Hmx.ty_sch) list

let infer (env : env) ast =
    let ty = TVar (Hmx_types.fresh_ty_var ()) in
    let constr = Hmx.infer ast ty in
    ignore @@ Solver.run env constr ;
    ty

let def_infer isrec env vbs =
    let constr = Hmx.infer_def isrec vbs in
    let env = Solver.run env constr in
    env
