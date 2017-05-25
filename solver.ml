open Hmx
open Types
open Printer

let extend env var sch = (var, sch) :: env
let lookup env var = List.assoc var env

type pool = {
    mutable vars: var list ;
    mutable rank: int
}

let chop pool term =
    let var = fresh_ty_var () in
    (Union_find.find var).rank <- pool.rank ;
    (Union_find.find var).structure <- Some term ;
    var

let alpha_conv term r =
    let mark = Types.next_mark () in
    let rec iter term = match term with (*  *)
        | TApp (t1, t2) -> TApp (iter t1, iter t2)
        | TVar var ->
            let var = Union_find.find var in
            if var.mark == mark then
                try TVar (List.assq var r)
                with Not_found -> term
            else begin
                var.mark <- mark ;
                try
                    let r = List.assq var r in
                    let s = match var.structure with
                        | Some s -> Some (iter s)
                        | None -> None
                    in
                    (Union_find.find r).structure <- s ;
                    TVar r
                with Not_found -> begin match var.structure with
                    | Some s -> iter s
                    | None -> term
                end
            end
        | TConst _ -> term
    in iter term

let instance pool (Forall (vars, c, term)) =
    let replace = List.map
        (fun v ->
             let new_var = fresh_ty_var () in
             (Union_find.find new_var).rank <- pool ;
             (Union_find.find v, new_var))
        vars
    in
    alpha_conv term replace

(* FIXME FIXME FIXME FIXME *)
let dummy_pos = Lexing.({ dummy_pos with pos_fname = "<stdin>" ; pos_lnum = 1 ; pos_cnum = 1 })
let dummy_sp = (dummy_pos, { dummy_pos with Lexing.pos_cnum = 2 })

let rec unify pool sess t1 t2 =
    let open Union_find in
    if (find t1 == find t2) then ()
    else match ((find t1).structure, (find t2).structure) with
        | None, None ->
            union (fun x y -> if x.Types.rank < y.Types.rank then x else y) t1 t2
        | (Some r1, None)    ->
            union (fun x y -> if x.Types.rank < y.Types.rank then x else {y with structure = x.structure}) t1 t2
        | (None, Some r2)    ->
           union (fun x y -> if x.Types.rank < y.Types.rank then {x with structure = y.structure} else y) t1 t2
        | (Some r1, Some r2) ->
            union (fun x y -> if x.Types.rank < y.Types.rank then x else y) t1 t2 ; unify_terms pool sess r1 r2

and unify_terms pool sess t1 t2 =
    match (t1, t2) with
        | (TVar v1, TVar v2) -> unify pool sess v1 v2
        | (TVar v, t)
        | (t, TVar v) -> unify pool sess v (chop pool t)
        | (TApp (t1, v1), TApp(t2, v2)) -> unify_terms pool sess t1 t2 ; unify_terms pool sess v1 v2
        | (TConst x, TConst y) when x = y -> ()
        | _ -> Errors.span_fatal sess dummy_sp
                   (Format.asprintf "impossible to unify %a with %a"
                        Printer.sch (Hmx.sch t1) Printer.sch (Hmx.sch t2))

let rec var_member v lst = match lst with
    | [] -> false
    | (x :: t) ->
        if Union_find.find v == Union_find.find x
            then true
            else var_member v t

let rec union l1 l2 = match l1 with
    | [] -> l2
    | (x :: t) ->
        let ret = union t l2 in
        if var_member x l2 then ret else (x :: ret)

let free_vars_of term =
    let mark = Types.next_mark () in
    let rec iter = function
        | TConst _ -> []
        | TVar v ->
            let var = Union_find.find v in
            if var.mark == mark then [v]
            else begin
                var.mark <- mark ;
                match var.structure with
                    | Some s -> iter s
                    | None -> [v]
            end
        | TApp (t1, t2) -> union (iter t1) (iter t2)
    in iter term

let rec solve sess constr pool env =
    match constr with
        | CDump -> env
        | CBool true -> []
        | CBool false -> failwith "false"
        | CApp (pred, [t1 ; t2]) when pred = is_subtype ->
            unify pool sess (chop pool t1) (chop pool t2) ; []
        | CApp _ -> failwith "bad predicate application"
        | CAnd (c1, c2) -> ignore @@ solve sess c1 pool env ; solve sess c2 pool env
        | CExists (vars, constr) ->
            List.iter (fun var -> (Union_find.find var).Types.rank <- pool.rank) vars ;
            pool.vars <- vars @ pool.vars ;
            solve sess constr pool env
        | CDef (var, sch, constr) ->
            let (Forall (vars, c, t)) = sch in
            let v = chop pool t in

            let pool' = { rank = pool.rank + 1 ; vars = [] } in
            List.iter (fun var -> (Union_find.find var).Types.rank <- pool'.rank) vars ;
            pool.vars <- vars @ pool'.vars ;

            ignore @@ solve sess c pool' env ;

            let t = match (Union_find.find v).structure with
                | Some s -> s
                | None -> TVar v
            in

            (* we only generalize up to the rank of this pool... *)
            let vars =
                List.filter
                    (fun v -> (Union_find.find v).Types.rank > pool.rank)
                    (free_vars_of t)
            in

            solve sess constr pool (extend env var (Forall (vars, CBool true, t)))
        | CInstance (var, pos, ty) ->
            try unify_terms pool sess ty @@ instance pool.rank (lookup env var) ; []
            with Not_found ->
                Errors.span_fatal sess pos @@
                    Printf.sprintf "unbound variable: %s" @@ Ident.show var

let run sess env const = solve sess const { rank = 0 ; vars = [] } env
