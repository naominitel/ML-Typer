type ty_sch =
    | Forall of Types.var list * constr * Types.ty

and constr =
    | CBool of bool
    | CApp of Ident.t * Types.ty list
    | CAnd of constr * constr
    | CExists of Types.var list * constr
    | CDef of Ident.t * ty_sch * constr
    | CInstance of Ident.t * Codemap.span * Types.ty
    | CDump

let sch ty =
    Forall ([], CBool true, ty)

let is_subtype = Ident.intern "="

let is_instance sch ty =
    let Forall(v, c, t) = sch in
    CExists (v, CAnd (c, CApp (is_subtype, [t ; ty])))

let has_instance sch =
    let Forall(v, c, _) = sch in
    CExists (v, c)

let letin var sch constr =
    CDef (var, sch, constr)

let curry f args =
    List.fold_left (fun acc t -> Types.TApp (acc, t)) f args

let tuple_type =
    let tuple_constructors = Hashtbl.create 100 in
    fun n ->
        try Hashtbl.find tuple_constructors n
        with Not_found -> begin
            let id = Types.TConst (Ident.intern @@ Printf.sprintf "tuple%d" n) in
            Hashtbl.add tuple_constructors n id ; id
        end

(* TODO: handle patterns. there are two ways to do it, one that implies making the
   constraints terms accept some kind of pattern somehow, on the implies desugaring
   them during typing *)

let rec infer term ty = match snd term with
    | `Unit -> CApp (is_subtype, [Types.t_unit ; ty])
    | `Cst _ -> CApp (is_subtype, [Types.t_int ; ty])
    | `Var v -> CInstance (v, fst term, ty)
    | `Tuple t ->
        let (vars, types, constr, len) =
            List.fold_right
                (fun e (vars, types, constr, i) ->
                     let x = Types.fresh_ty_var () in
                     (x :: vars, (Types.TVar x) :: types,
                      CAnd (constr, infer e (Types.TVar x)), i + 1))
                t ([], [], CBool true, 0)
        in

        let ctor = tuple_type len in
        CExists (vars, CAnd (constr, CApp (is_subtype, [ty ; curry ctor types])))
    | `List l ->
        let x = Types.fresh_ty_var () in
        let constr =
            List.fold_left
                (fun constr e -> CAnd (constr, infer e (Types.TVar x)))
                (CBool true) l
        in

        CExists ([x], CAnd (constr,
                          CApp (is_subtype, [ty ; Types.TApp (Types.t_list, Types.TVar x)])))
    | `If (ec, et, ef) ->
        CAnd (infer ec (Types.t_bool), CAnd (infer et ty, infer ef ty))
    | `Fun (x, t) ->
        let x = match snd x with
            | `PatVar v -> v
            | _ -> failwith "patterns are not supported by HM(X)"
        in
        let x1 = Types.fresh_ty_var () in
        let x2 = Types.fresh_ty_var () in
        let constr_body = infer t (Types.TVar x2) in
        CExists ([x1 ; x2], CAnd (CDef (x, sch (Types.TVar x1), constr_body),
                                  CApp (is_subtype, [Types.function_type (Types.TVar x1) (Types.TVar x2) ; ty])))
    | `Let (isrec, z, e, t) -> infer_binding isrec z e (infer t ty)
    | `Apply (f, a) ->
        let x2 = Types.fresh_ty_var () in
        CExists ([x2], CAnd (infer f (Types.function_type (Types.TVar x2) ty),
                             infer a (Types.TVar x2)))
    | `BinOp (op, e1, e2) ->
        begin match op with
            | `Plus | `Minus | `Mult | `Div ->
                CAnd (CAnd (infer e1 Types.t_int, infer e2 Types.t_int),
                      CApp (is_subtype, [ty ; Types.t_int]))
            | `Eq ->
                let x = Types.fresh_ty_var () in
                CAnd (CAnd (infer e1 (Types.TVar x), infer e2 (Types.TVar x)),
                      CApp (is_subtype, [ty ; Types.t_bool]))
            | `Cons ->
                let x = Types.fresh_ty_var () in
                let t = Types.TApp (Types.t_list, Types.TVar x) in
                CExists ([x], CAnd (CAnd (infer e1 (Types.TVar x), infer e2 t),
                      CApp (is_subtype, [t ; ty])))
        end
    | `Match _ -> failwith "patterns are not supported by HM(X)"

and infer_binding isrec pat expr constr =
    let var = match snd pat with
        | `PatVar v -> v
        | _ -> failwith "patterns are not supported by HM(X)"
    in

    let x = Types.fresh_ty_var () in

    let inner = if isrec then
        letin var (Forall ([], CBool true, Types.TVar x)) (infer expr (Types.TVar x))
     else infer expr (Types.TVar x)
    in letin var (Forall ([x], inner, Types.TVar x)) constr

let infer_def p e = infer_binding true p e CDump
