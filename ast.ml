(* common type definitions for the AST *)

module Var : sig
  type t
  val to_string : t -> string
end = struct
  type t = string
  let to_string x = x
end

type 'a ast = {
    sp: Codemap.span ;
    d: 'a ast_s
  }
and 'a ast_s =
  | Var of Var.t
  | App of 'a ast * 'a ast
  | Lam of (Var.t * Codemap.span) * ('a * Codemap.span) option * 'a ast
  (*                 var span        type  type span                  *)
(* let x = u in v <=> (\x . v) u *)
(* (u:t) <=> (\(x : t) . x) u *)

(* /!\ need -rectypes *)
type parser_ast = parser_ast ast

module type TyperSig = sig
  type ty
  val ty_to_string : ty -> string
end

module Make (T : TyperSig) = struct

  type t = T.ty ast

  (* not-so-pretty-printing functions, for debug purposes *)

  (* Returns a fully-parenthesized string representation of the AST *)
  let rec ast_to_string ast =
    let sprint = Printf.sprintf in
    match ast.d with
      | Var v -> Var.to_string v
      | App (e1, e2) -> sprint "(%s %s)" (ast_to_string e1) (ast_to_string e2)
      | Lam ((v, _), None, body) -> sprint "(\ %s . %s)" (Var.to_string v) (ast_to_string body)
      | Lam ((v, _), Some (ty, _), body) -> sprint "(\ %s : %s . %s)" (Var.to_string v) (T.ty_to_string ty)
                                           (ast_to_string body)
end
