(* Specification of typers module and adaptation of the different typers *)
open Types

module type GenTyper = sig
    type ty
    type input_ast
    type input_pat

    val from_ast     : Ast.input_ast -> input_ast Errors.t
    val from_pat     : Ast.input_pat -> input_pat Errors.t
    val infer        : (module Unificator with type t = 'a) -> 'a -> Session.session -> ty Types.type_env -> input_ast -> ty
    val def_infer    : (module Unificator) -> Session.session -> ty Types.type_env -> input_pat -> input_ast -> ty Types.type_env
    val ty_to_string : ty -> string
  end

module Generic: GenTyper = struct
    type ty      = Types.ty
    let ty_to_string = Types.ty_to_string

    include Basic
    include Generic
end

module Poly: GenTyper = struct
    type ty      = Types.ty_sch
    let ty_to_string = Types.sty_to_string

    include Basic
    
    let infer (type s) (module M: Unificator with type t = s) (_: s) sess env ast = Poly.infer sess env ast
    let def_infer (module M: Unificator) sess env pat ast = Poly.def_infer sess env pat ast
end
