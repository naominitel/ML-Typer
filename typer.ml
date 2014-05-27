(* typer for a simple ML-like pure functional language *)

open Ast

(* a type variable *)
type tvar = int

(* generates a unique type var *)
let next_var =
    let n = ref 0 in
    (fun () ->
        let ret = !n in
        n := !n + 1 ;
        ret)

(* 
 * the type of an expression may be either
 * a type variable or a concrete type
 *)
type ty =
  | TUnit
  | TInt
  | TVar   of tvar
  | TTuple of ty list
  | TFunc  of ty * ty
  | TSum   of ctor list
and ctor = (string * ty)

(*
 * the typing environment that remembers
 * lexical bindings and their types
 *)
type type_env = (string * ty) list

(*
 * equation system that stores all the constraints
 * encountered during type inference
 *)
type equ = (ty * ty)
type equ_sys = equ list

(*
 * Type the pattern of a let form or a function argument
 * Returns:
 *  - the type of the whole pattern
 *  - a type environment for bindings defined in the pattern
 *)
let rec pat_typer pat = match pat with
  | PatUnit  -> (TUnit, [])
  | PatCst _ -> (TInt, [])
  | PatVar v ->
      let ty = TVar (next_var ()) in
      (ty, [(v, ty)])
  | PatCtor (name, pat) ->
      let (pat_ty, pat_env) = pat_typer pat in
      (TSum [(name, pat_ty)], pat_env)
  | PatTup pats ->
      let (types, env_list) = List.split (List.map pat_typer pats) in
      (TTuple types, List.flatten env_list)

(*
 * Infers the type of an expression
 * Returns:
 *  - the inferred type, that may be a type variable
 *  - an equation system with constraints on the inferred types
 *)
let rec typer env ast = match ast with
  | Unit  -> (TUnit, [])
  | Cst _ -> (TInt, [])
  | Var v ->
      (* typing of a variable (lookup in the type environment) *)
      (try (List.assoc v env, [])
       with Not_found -> failwith "unbound variable")
  | Let (pat, expr, body) ->
      (*
       * typing of `let x = e1 in e2' :
       *  - type x in tx
       *  - type e1 in t1
       *  - type e2 in t2, with the pattern in the environment
       *  - return t2, add t1 = tpat as constraint
       *)
      let (ty_pat, nenv)      = pat_typer pat  in
      let (ty_expr, sys)      = typer env expr in
      let (ty_body, sys_body) = typer (nenv @ env) body in
      (ty_body, (ty_expr, ty_pat) :: sys @ sys_body)
  | Fun (pat, body) ->
      (*
       * typing of `fun x -> y' :
       *  - type x in tx
       *  - type y in ty
       * Constraints will be added on apply
       *)
      let (ty_pat, nenv) = pat_typer pat in
      let (ty_body, sys) = typer (nenv @ env) body in
      (TFunc (ty_pat, ty_body), sys)
  | If (econd, etrue, efalse) ->
      (*
       * typing of ifz e1 then e2 else e3
       *  - type of the expression is t2
       *  - add constraint t2 = t3
       *  - add constraint t1 = int
       *)
      let (ty_econd, sys_cond)   = typer env econd  in
      let (ty_etrue, sys_true)   = typer env etrue  in
      let (ty_efalse, sys_false) = typer env efalse in
      let sys = sys_cond @ sys_true @ sys_false in
      (ty_etrue, (ty_econd, TInt) :: (ty_etrue, ty_efalse) :: sys)
  | Tuple lst ->
      (*
       * typing of a tuple
       * just collects the types and constraints of sub-expressions
       *)
      let (types, sys_list) = List.split (List.map (typer env) lst) in
      (TTuple types, List.flatten sys_list)
  | BinOp (_, opl, opr) ->
      (*
       * typing of a binary operator
       *  - type of the expression is x
       *  - add the constraint that the types of operands must be int
       *)
      let (ty_opl, sys_l) = typer env opl in
      let (ty_opr, sys_r) = typer env opr in
      (TInt, (ty_opl, TInt) :: (ty_opr, TInt) :: sys_l @ sys_r)
  | Match (expr, (car :: cdr)) ->
      (*
       * typing of a match expr with ...
       *  - type expr into ty_expr
       *  - type each arm t into ty_t with the pattern in the environment
       *  - add the constraint that ty_0 = ty_1 ... = ty_n
       *  - add the constraint that each pattern has the same type
       *  - add the constraint that ty_expr = type of the patterns
       *  - the final type is ty_0
       *)
      let type_arm (pat, ast) =
          let (ty_pat, nenv)  = pat_typer pat          in
          let (ty_arm, sys)   = typer (nenv @ env) ast in
          (ty_pat, ty_arm, sys) in
      let (ty_expr, sys_expr) = typer env expr in
      let (ty_pat_car, ty_arm_car, sys_car) = type_arm car in
      let sys = List.flatten (List.map (fun arm ->
          let (ty_pat, ty_arm, sys) = type_arm arm in
          (ty_pat_car, ty_pat) :: (ty_arm_car, ty_arm) :: sys) cdr) in
      (ty_arm_car, (ty_pat_car, ty_expr) :: sys)
  | Apply (func, arg) ->
      (*
       * typing of a function application (f e)
       *  - type f in tf
       *  - type e in te
       *  - introduce a type tr for the result
       *  - add constraint tf = te -> tr
       *)
      let (ty_func, sys_func) = typer env func in
      let (ty_arg, sys_arg)   = typer env arg  in
      let ty_ret = TVar (next_var ()) in
      (ty_ret, (TFunc (ty_arg, ty_ret), ty_func) :: sys_func @ sys_arg)
  | _ ->
      (* TODO: implement match and sum types *)
      failwith "unimplemented"

let rec ty_to_string ty = match ty with
  | TUnit                    -> "*unit*" ;
  | TInt                     -> "int" ;
  | TVar v                   -> Printf.sprintf "a%d" v
  | TTuple []                -> failwith "empty tuple"
  | TTuple (car :: cdr)      -> Printf.sprintf
                                    "(%s)"
                                    (List.fold_left
                                        (fun res ty -> (Printf.sprintf
                                                           "%s, %s" res
                                                           (ty_to_string ty)))
                                        (ty_to_string car) cdr)
  | TFunc (arg, res)         -> Printf.sprintf "%s -> %s" (ty_to_string arg)
                                                  (ty_to_string res)
  | TSum []                  -> failwith "empty sum type"
  | TSum ((ctor, ty) :: cdr) -> Printf.sprintf
                                    "(%s)"
                                    (List.fold_left
                                        (fun res (ctor, ty) ->
                                            (Printf.sprintf "%s | Ctor%s %s"
                                                            res ctor
                                                            (ty_to_string ty)))
                                        (Printf.sprintf "Ctor%s %s" ctor
                                                        (ty_to_string ty)) cdr)
