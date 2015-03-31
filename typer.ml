module type S = sig
    type ty

    val show : ty -> string

    val from_ast : (('ast Ast.ast) as 'ast) -> ty
end

module MakeAst(Typer: S) = struct
    open Ast
    open Codemap

    type t = Typer.ty ast
    
    let rec from_frontend_ast ast = {
        ast with d = match ast.d with
            | Var v -> Var v
            | App (e1, e2) -> App (from_frontend_ast e1, from_frontend_ast e2)
            | Abs (v, None, body) -> Abs (v, None, from_frontend_ast body)
            | Abs (v, Some ty, body) ->
                Abs (v, Some (Typer.from_ast ty), from_frontend_ast body)
        }

    (* not-so-pretty-printing functions, for debug purposes *)

    (* Returns a fully-parenthesized string representation of the AST *)
    let rec show ast =
        let open Printf in
        match ast.d with
            | Var v -> Ident.show v
            | App (e1, e2) ->
                 sprintf "(%s %s)" (show e1) (show e2)
            | Abs (v, None, body) ->
                 sprintf "(\\ %s . %s)" (Ident.show v.d) (show body)
            | Abs (v, Some ty, body) ->
                 sprintf "(\\ %s : %s . %s)"
                         (Ident.show v.d) (Typer.show ty) (show body)
end
