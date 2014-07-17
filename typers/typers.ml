(* Specification of typers module and adaptation of the different typers *)

module type GenTyper = sig
    type ty
    type ast
    type pattern

    val from_ast     : Errors.callback -> Ast.input_ast -> ast     Utils.Maybe.t
    val from_pat     : Errors.callback -> Ast.input_pat -> pattern Utils.Maybe.t
    val infer        : Session.session -> ty Types.type_env -> ast     -> ty
    val def_infer    : Session.session -> ty Types.type_env -> pattern -> ast -> ty Types.type_env
    val ty_to_string : ty -> string
  end

module BasicTyper = struct
    type ty      = Types.ty
    type ast     = Ast.basic_input_ast
    type pattern = Ast.basic_input_pat

    let from_ast     = Ast.simple_ast
    let from_pat     = Ast.simple_pat
    let ty_to_string = Types.ty_to_string
end

module Simple: GenTyper = struct
    include BasicTyper
    include Simple
end

module Core: GenTyper = struct
    include BasicTyper
    include Core
end

module Poly: GenTyper = struct
    type ty      = Types.ty_sch
    type ast     = Ast.basic_input_ast
    type pattern = Ast.basic_input_pat
    include Poly

    let from_ast     = Ast.simple_ast
    let from_pat     = Ast.simple_pat
    let ty_to_string = Types.sty_to_string
end
