open Hmx
open Types
open Format

module VMap =
    Map.Make (struct
        type t = Types.var_descr
        let compare x y = if x == y then 0 else -1 (* FIXME *)
    end)

module VSet =
    Set.Make (struct
        type t = Types.var_descr
        let compare x y = if x == y then 0 else -1 (* FIXME *)
    end)

let var (vars, next) ff var =
    (* Printf.sprintf "%d" var.name *)
    let id = try VMap.find var !vars with Not_found ->
        let name = !next in
        incr next ; vars := VMap.add var name !vars ;
        name
    in fprintf ff "α%d" id

let rec ty var_printer vl ff = function
    | TApp (TApp (s1, s2), s3) when s1 = arrow ->
        fprintf ff "%a -> %a"
            (ty1 var_printer vl) s2
            (ty  var_printer vl) s3
    | t -> ty1 var_printer vl ff t

and ty1 var_printer vl ff = function
    | TApp (s1, s2) ->
        fprintf ff "%a %a"
            (ty1 var_printer vl) s1
            (ty2 var_printer vl) s2
    | t -> ty2 var_printer vl ff t

and ty2 var_printer vl ff = function
    | TConst s -> fprintf ff "%s" @@ Ident.show s
    | TVar s -> tvar var_printer vl ff s
    | s -> fprintf ff "(%a)" (ty var_printer vl) s

and tvar var_printer vl ff v =
    let v = Union_find.find v in
    begin match v.structure with
        | Some s ->
            let mark = Types.next_mark () in
            v.mark <- mark ;
            if VSet.mem v !vl then
                var var_printer ff v
            else begin
                vl := VSet.add v !vl ;
                let t = asprintf "%a" (ty var_printer vl) s in
                if v.mark == mark then fprintf ff "%s" t
                else fprintf ff "(%s as %a)" t (var var_printer) v
            end
        | None -> var var_printer ff v
    end

let rec list sep pp ff = function
    | []         -> ()
    | [hd]       -> pp ff hd
    | (hd :: tl) -> fprintf ff "%a%s%a" pp hd sep (list sep pp) tl

let rec constr vp vl ff const = match const with
    | CDump -> fprintf ff "Dump"
    | CBool true -> fprintf ff "True"
    | CBool false -> fprintf ff "False"
    | CApp (pred, args) when pred = Hmx.is_subtype ->
        fprintf ff "%a = %a" (ty vp vl) (List.hd args) (ty vp vl) (List.hd @@ List.tl args)
    | CApp (pred, args) ->
        fprintf ff "%s %a" (Ident.show pred) (list ", " @@ ty vp vl) args
    | CAnd (c1, c2) ->
        fprintf ff "(%a) ∧ (%a)" (constr vp vl) c1 (constr vp vl) c2
    | CExists (vars, cstr) ->
        fprintf ff "∃ %a : %a"
            (list ", " (fun ff v -> var vp ff (Union_find.find v))) vars
            (constr vp vl) cstr
    | CDef (var, sch_, cstr) ->
        fprintf ff "def %s: %a in %a"
            (Ident.show var) (sch vp vl) sch_ (constr vp vl) cstr
    | CInstance (var, _, t) ->
        fprintf ff "%s < %a" (Ident.show var) (ty vp vl) t

and sch var_printer vl ff (Forall (vars, c, t)) =
    (* force printing of the type first so that variables
       are named after their order of occurence in the
       type term rather than in the quantifier list *)
    if vars == [] then ty var_printer vl ff t
    else match c with
        | CBool true ->
            fprintf ff "∀ %a: %a"
                (list ", " (fun ff v -> var var_printer ff (Union_find.find v))) vars
                (ty var_printer vl) t
        | _ ->
            fprintf ff "∀ %a[%a]: %a"
                (list ", " (fun ff v -> var var_printer ff (Union_find.find v))) vars
                (constr var_printer vl) c
                (ty var_printer vl) t

let sch ff sch_ =
    let var_printer = (ref VMap.empty, ref 0) in
    let vl = ref VSet.empty in
    sch var_printer vl ff sch_

let constr ff cstr =
    let var_printer = (ref VMap.empty, ref 0) in
    let vl = ref VSet.empty in
    constr var_printer vl ff cstr
