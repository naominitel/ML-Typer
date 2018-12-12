type ty_sch =
    | Forall of Hmx_types.var list * constr * Hmx_types.ty

and constr =
    | CBool of bool
    | CApp of Uid.t * Hmx_types.ty list
    | CAnd of constr * constr
    | CExists of Hmx_types.var list * constr
    | CDef of Uid.t * ty_sch * constr
    | CInstance of Uid.t * Location.t * Hmx_types.ty
    | CDump

let sch ty =
    Forall ([], CBool true, ty)

let is_subtype = Uid.intern "="

let is_instance sch ty =
    let Forall(v, c, t) = sch in
    CExists (v, CAnd (c, CApp (is_subtype, [t ; ty])))

let has_instance sch =
    let Forall(v, c, _) = sch in
    CExists (v, c)

let letin var sch constr =
    CDef (var, sch, constr)

let curry f args =
    List.fold_left (fun acc t -> Hmx_types.TApp (acc, t)) f args

let tuple_type =
    let tuple_constructors = Hashtbl.create 100 in
    fun n ->
        try Hashtbl.find tuple_constructors n
        with Not_found -> begin
            let id = Hmx_types.TConst (Uid.intern @@ Printf.sprintf "tuple%d" n) in
            Hashtbl.add tuple_constructors n id ; id
        end

(* TODO: handle patterns. there are two ways to do it, one that implies making the
   constraints terms accept some kind of pattern somehow, on the implies desugaring
   them during typing *)

open Parsetree
exception Unimpl of Location.t * string

let type_of_const cst = match cst with
    | Pconst_integer _ -> Hmx_types.t_int
    | Pconst_char    _ -> Hmx_types.t_char
    | Pconst_string  _ -> Hmx_types.t_string
    | Pconst_float   _ -> Hmx_types.t_float

let infer_lid lid ty = match lid.Location.txt with
    | Longident.Lident id -> CInstance (id, lid.Location.loc, ty)
    | _ -> raise @@ Unimpl (lid.loc, "unsupported: module lookups")

let rec infer term ty = match term.pexp_desc with
    | Pexp_constant cst -> CApp (is_subtype, [type_of_const cst ; ty])
    | Pexp_ident lid -> infer_lid lid ty
    | Pexp_tuple [] -> CApp (is_subtype, [Hmx_types.t_unit ; ty])
    | Pexp_tuple tup ->
        let (vars, types, constr, len) =
            List.fold_right
                (fun expr (vars, types, constr, i) ->
                     let x = Hmx_types.fresh_ty_var () in
                     (x :: vars, (Hmx_types.TVar x) :: types,
                      CAnd (constr, infer expr (Hmx_types.TVar x)), i + 1))
                tup ([], [], CBool true, 0)
        in

        let ctor = tuple_type len in
        CExists (vars, CAnd (constr, CApp (is_subtype, [ty ; TApp (ctor, types)])))
    | Pexp_ifthenelse (ec, et, Some ef) ->
        CAnd (infer ec (Hmx_types.t_bool), CAnd (infer et ty, infer ef ty))
    | Pexp_ifthenelse (ec, et, None) ->
        CAnd (infer ec (Hmx_types.t_bool), infer et ty)
    | Pexp_fun (Asttypes.Nolabel, default, pattern, body) ->
        let pvar = match pattern.ppat_desc with
            | Ppat_var v -> v.Location.txt
            | _ -> failwith "patterns are not supported by HM(X)"
        in
        let x1 = Hmx_types.fresh_ty_var () in
        let x2 = Hmx_types.fresh_ty_var () in
        let constr_body = infer body (Hmx_types.TVar x2) in
        CExists ([x1 ; x2], CAnd (CDef (pvar, sch (Hmx_types.TVar x1), constr_body),
                                  CApp (is_subtype,
                                        [Hmx_types.function_type
                                             (Hmx_types.TVar x1)
                                             (Hmx_types.TVar x2) ;
                                         ty])))
    | Pexp_fun (_, _, _, _) -> raise @@ Unimpl (term.pexp_loc, "labeled args")
    | Pexp_let (isrec, vbs, body) -> infer_binding isrec vbs (infer body ty)
    | Pexp_apply (f, a) ->
        let (evars, ftype, arg_constrs) =
        List.fold_left
            (fun (evars, ftype, arg_constrs) arg ->
                 let arg_expr = match arg with
                     | (Asttypes.Nolabel, arg) -> arg
                     | _ -> raise @@ Unimpl (term.pexp_loc, "labeled args")
                 in
                let x2 = Hmx_types.fresh_ty_var () in
                (x2 :: evars,
                 Hmx_types.function_type (Hmx_types.TVar x2) ftype,
                 CAnd (infer arg_expr (Hmx_types.TVar x2), arg_constrs)))
            ([], ty, CBool true) a
        in CExists (evars, CAnd (infer f ftype, arg_constrs))
    | _ -> raise @@ Unimpl (term.pexp_loc, "expr kind")

and infer_binding isrec bindings constr =
    let binding = match bindings with
        | [] -> assert false
        | [b] -> b
        | b :: _ -> raise @@ Unimpl (b.pvb_loc, "let and")
    in

    let var = match binding.pvb_pat.ppat_desc with
        | Ppat_var v -> v.Location.txt
        | _ -> raise @@ Unimpl (binding.pvb_pat.ppat_loc, "patterns")
    in

    let x = Hmx_types.fresh_ty_var () in

    let inner = match isrec with
        | Recursive ->
            letin var (Forall ([], CBool true, Hmx_types.TVar x))
                  (infer binding.pvb_expr (Hmx_types.TVar x))
        | Nonrecursive -> infer binding.pvb_expr (Hmx_types.TVar x)
    in letin var (Forall ([x], inner, Hmx_types.TVar x)) constr

let infer_def isrec vbs =
    infer_binding isrec vbs CDump
