open Types

let tuple = Hmx.tuple_type 2

(*
 * FIXME: should be linked to codegen somehow. and should probably be
 * elsewhere
 *)
let init_ty_env = [
    (Ident.intern "hd",
     let v = Types.fresh_ty_var () in
     let lst = Types.TApp (Types.t_list, TVar v) in
     Hmx.Forall ([v], Hmx.CBool true, Types.function_type lst (Types.TVar v))) ;

    (Ident.intern "tl",
     let v = Types.fresh_ty_var () in
     let lst = Types.TApp (Types.t_list, TVar v) in
     Hmx.Forall ([v], Hmx.CBool true, Types.function_type lst lst)) ;

    (Ident.intern "fst",
     let x = Types.fresh_ty_var () in
     let y = Types.fresh_ty_var () in
     let tup = Hmx.curry tuple [TVar x ; TVar y] in
     Hmx.Forall ([x ; y], Hmx.CBool true, Types.function_type tup (Types.TVar x))) ;

    (Ident.intern "snd",
     let x = Types.fresh_ty_var () in
     let y = Types.fresh_ty_var () in
     let tup = Hmx.curry tuple [TVar x ; TVar y] in
     Hmx.Forall ([x ; y], Hmx.CBool true, Types.function_type tup (Types.TVar y))) ;

    (Ident.intern "print_int",
     Hmx.sch @@ Types.function_type Types.t_int Types.t_unit)
]

let infer sess env ast =
    let ty = TVar (Types.fresh_ty_var ()) in
    let constr = Hmx.infer ast ty in
    ignore @@ Solver.run sess env constr ;
    ty

let def_infer sess env pat expr =
    let constr = Hmx.infer_def pat expr in
    let env = Solver.run sess env constr in
    env
