(* Specification of typers module and adaptation of the different typers *)

module type GenTyper = sig
    type ty
    type input_ast
    type input_pat

    val from_ast     : Ast.input_ast -> input_ast Errors.t
    val from_pat     : Ast.input_pat -> input_pat Errors.t
    val infer        : Session.session -> ty Types.type_env -> input_ast -> ty
    val def_infer    : Session.session -> ty Types.type_env -> input_pat -> input_ast -> ty Types.type_env
    val ty_to_string : ty -> string
  end

module Simple: GenTyper = struct
    type ty = Types.ty
    let ty_to_string = Types.ty_to_string

    include Basic
    include Simple
end

module Core: GenTyper = struct
    type ty = Types.ty
    let ty_to_string = Types.ty_to_string

    include Basic
    include Core
end

module Poly: GenTyper = struct
    type ty      = Types.ty_sch
    let ty_to_string = Types.sty_to_string

    include Basic
    include Poly
end
