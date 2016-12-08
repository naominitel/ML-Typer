type ast

type context = {
    llctxt: Llvm.llcontext ;
    builder: Llvm.llbuilder
}

exception Codegen_error of string

type primitive =
    | Codegen of Ident.t * (context -> ast list -> Llvm.llvalue)
    | Runtime of Ident.t * int
