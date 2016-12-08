(* common type definitions for the AST *)

type ('cst, 'ty) ast = (('cst, 'ty) ast_node) Codemap.spanned
and ('cst, 'ty) ast_node =
    | Var of Ident.t
    | App of ('cst, 'ty) ast * ('cst, 'ty) ast
    | Abs of Ident.t Codemap.spanned * ('ty) option * ('cst, 'ty) ast
    | Const of 'cst

(* curryfied application of a type constructor to a list of types *)
let curry acc args =
    List.fold_left
        (fun acc arg ->
            Codemap.dummy_spanned @@ App (acc, arg))
        acc args
