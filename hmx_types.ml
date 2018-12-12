(* Internal representation of types *)

type ty =
    | TConst of string
    | TVar of var
    | TApp of ty * ty list

and var = var_descr Union_find.point

and var_descr = {
    mutable structure: ty option ;
    mutable rank: int ;
    name: int ;
    mutable mark: unit ref
}

let arrow = TConst (Uid.intern "->")

let function_type t1 t2 =
    TApp (arrow, [t1 ; t2])

let next = ref 0
let next_mark () = ref ()

let fresh_ty_var () =
    Union_find.fresh {
        structure = None ;
        rank = 0 ;
        name = (incr next ; !next) ;
        mark = next_mark ()
    }

let t_int    = TConst (Uid.intern "int")
let t_char   = TConst (Uid.intern "char")
let t_string = TConst (Uid.intern "string")
let t_float  = TConst (Uid.intern "float")
let t_bool   = TConst (Uid.intern "bool")
let t_unit   = TConst (Uid.intern "unit")
let t_list   = TConst (Uid.intern "list")
