(* common type definitions for the AST *)

type 'ty ast = ('ty ast_node) Codemap.spanned
and 'ty ast_node =
    | Var of Ident.t
    | App of 'ty ast * 'ty ast
    | Abs of Ident.t Codemap.spanned * ('ty) option * 'ty ast
