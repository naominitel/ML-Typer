(*
 * a re-expanded ast that contains enough information for codegen
 * - functions are uncurryed
 * - applications are n-ary
 * - beta-redexes are identified
 *)

type ast =
    | Var of Ident.t
    | Closure of Ident.t list * ast
    | Call of ast * ast list
    | Let of bool * Ident.t * ast * ast

exception Codegen_error of string

let rec from_ast ast = match ast.Codemap.d with
    | Ast.Var id -> Var id
    | Ast.Abs (arg, _, expr) ->
        begin match from_ast expr with
            | Closure (args, expr) -> Closure (arg.d :: args, expr)
            | expr -> Closure ([arg.d], expr)
        end
    | Ast.App (f, arg) ->
        begin match from_ast f with
            | Call (f, args) -> Call (f, args @ [from_ast arg])
            (* TODO: handle mutual recursion *)
            | Closure ([var], expr) ->
                (* identify a ß-redex. the "fix" primitive denotes a let rec *)
                let (isrec, expr) = match expr with
                    | Call (Var id, [arg]) when Ident.eq (Ident.intern "fix") id ->
                        (true, arg)
                    | _ -> (false, expr)
                in Let (isrec, var, from_ast arg, expr)
            | expr -> Call (expr, [from_ast arg])
        end

type context = {
    llctxt: Llvm.llcontext ;
    builder: Llvm.llbuilder
}

type primitive =
    | Codegen of Ident.t * (context -> ast list -> Llvm.llvalue)
    | Runtime of Ident.t * int
