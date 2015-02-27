(* common type definitions for the AST *)

type var

type ty
(* type ty should be a parameter => functor ? *)
(* what about primitives (like type constructions...) ? *)

type ast = {
    sp: Codemap.span ;
    d: ast_s
}
and ast_s =
  | AstVar of var
  | AstApp of ast * ast
  | AstLam of var * Codemap.span * ty * Codemap.span * ast
(* let x = u in v <=> (\x . v) u *)
(* (u:t) <=> (\(x : t) . x) u *)

(* not-so-pretty-printing functions, for debug purposes *)

(* Returns a fully-parenthesized string representation of the AST *)
let rec ast_to_string ast =
  let sprint = Printf.sprintf in
  match ast.d with
  | Var v -> failwith "TODO"
  | App (e1, e2) -> sprint "(%s %s)" (ast_to_string e1) (ast_to_string e2)
  | Lam (v, _, ty, _, body) -> failwith "TODO"
