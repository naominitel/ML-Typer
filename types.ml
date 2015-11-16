(* Internal representation of types *)

type ty =
    | TConst of Ident.t
    | TVar of var
    | TApp of ty * ty

and var = var_descr Union_find.point

and var_descr = {
    mutable structure: ty option ;
    mutable rank: int ;
    name: int ;
    mutable mark: unit ref
}

let arrow = TConst (Ident.intern "->")

let function_type t1 t2 =
    TApp ((TApp (arrow, t1)), t2)

let next = ref 0
let next_mark () = ref ()

let fresh_ty_var () =
    Union_find.fresh {
        structure = None ;
        rank = 0 ;
        name = (incr next ; !next) ;
        mark = next_mark ()
    }

let t_int = TConst (Ident.intern "int")
let t_bool = TConst (Ident.intern "bool")
let t_unit = TConst (Ident.intern "unit")
let t_list = TConst (Ident.intern "list")
