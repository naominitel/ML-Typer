open Hmx
open Hmx_types

module VMap =
    Map.Make (struct
        type t = Hmx_types.var_descr
        let compare x y = if x == y then 0 else -1 (* FIXME *)
    end)

module VSet =
    Set.Make (struct
        type t = Hmx_types.var_descr
        let compare x y = if x == y then 0 else -1 (* FIXME *)
    end)

let show_var (vars, next) var =
    (* Printf.sprintf "%d" var.name *)
    try VMap.find var !vars
    with Not_found ->
        let name = Printf.sprintf "α%d" !next in
        incr next ; vars := VMap.add var name !vars ;
        name

let rec show_ty var_printer vl = function
    | TConst s -> Uid.show s
    | TVar s -> show_tvar var_printer vl s
    | TApp (TApp (s1, s2), s3) when s1 = arrow ->
        let s2 = show_ty var_printer vl s2 in
        let s3 = show_ty var_printer vl s3 in
        Printf.sprintf "(%s -> %s)" s2 s3
    | TApp (s1, s2) ->
        let s1 = show_ty var_printer vl s1 in
        let s2 = show_ty var_printer vl s2 in
        Printf.sprintf "(%s %s)" s1 s2

and show_tvar var_printer vl var =
    let v = Union_find.find var in
    begin match v.structure with
        | Some s ->
            let mark = Hmx_types.next_mark () in
            v.mark <- mark ;
            if VSet.mem v !vl then
                show_var var_printer v
            else begin
                vl := VSet.add v !vl ;
                let t = (show_ty var_printer vl s) in
                if v.mark == mark then t
                else Printf.sprintf "(%s as %s)" t (show_var var_printer v)
            end
        | None -> show_var var_printer v
    end

let rec show_constr vp vl const = match const with
    | CDump -> "Dump"
    | CBool true -> "True"
    | CBool false -> "False"
    | CApp (pred, args) when pred = Hmx.is_subtype ->
        Printf.sprintf "%s = %s" (show_ty vp vl @@ List.hd args) (show_ty vp vl @@ List.hd @@ List.tl args)
    | CApp (pred, args) ->
        Printf.sprintf "%s %s"
            (Uid.show pred) (String.concat ", " @@ List.map (show_ty vp vl) args)
    | CAnd (c1, c2) ->
        Printf.sprintf "(%s) ∧ (%s)" (show_constr vp vl c1) (show_constr vp vl c2)
    | CExists (vars, constr) ->
        Printf.sprintf"∃ %s : %s"
            (String.concat ", " (List.map (fun v -> show_var vp (Union_find.find v)) vars))
            (show_constr vp vl constr)
    | CDef (var, sch, constr) ->
        Printf.sprintf "def %s: %s in %s"
            (Uid.show var) (show_sch_ vp vl sch) (show_constr vp vl constr)
    | CInstance (var, _, ty) ->
        Printf.sprintf "%s < %s"
            (Uid.show var) (show_ty vp vl ty)

and show_sch_ var_printer vl (Forall (vars, c, ty)) =
    (* force printing of the type first so that variables
       are named after their order of occurence in the
       type term rather than in the quantifier list *)
    let ty = (show_ty var_printer vl ty) in
    if vars == [] then ty
    else match c with
        | CBool true ->
            Printf.sprintf "∀ %s: %s"
                (String.concat ", "
                     (List.map
                          (fun v -> show_var var_printer (Union_find.find v))
                          vars))
                ty
        | _ ->
            Printf.sprintf "∀ %s[%s]: %s"
                (String.concat ", "
                     (List.map
                          (fun v -> show_var var_printer (Union_find.find v))
                          vars))
                (show_constr var_printer vl c)
                ty

let show_sch sch =
    let var_printer = (ref VMap.empty, ref 0) in
    let vl = ref VSet.empty in
    show_sch_ var_printer vl sch

let show_constr constr =
    let var_printer = (ref VMap.empty, ref 0) in
    let vl = ref VSet.empty in
    show_constr var_printer vl constr
