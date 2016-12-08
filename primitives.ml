let codegen_if cx args =
    let open Codegen in
    let (cond, branch_true, branch_false) = match args with
        | [c; t; f] -> (c, t, f)
        | _ -> raise (Codegen_error "invalid number of arguments for if")
    in

    let cur_block = Llvm.insertion_block cx.builder in
    let func = Llvm.block_parent cur_block in

    let then_blk = Llvm.append_block cx.llctxt "then" func in
    let else_blk = Llvm.append_block cx.llctxt "else" func in
    let cont_blk = Llvm.append_block cx.llctxt "cont" func in

    (* codegen branch *)

    let eval_cond = Llvm.build_ptrtoint
        (codegen_expr cx cond)
        (Llvm.i1_type cx.llctxt) "" cx.builder
    in

    ignore @@ Llvm.build_cond_br eval_cond then_blk else_blk cx.builder ;

    (* codegen true *)

    Llvm.position_at_end then_blk cx.builder ;
    let eval_then = codegen_expr cx branch_true in
    ignore @@ Llvm.build_br cont_blk cx.builder ;
    let then_blk = Llvm.insertion_block cx.builder in

    (* codegen false *)

    Llvm.position_at_end else_blk cx.builder ;
    let eval_else = codegen_expr cx branch_false in
    ignore @@ Llvm.build_br cont_blk cx.builder ;
    let else_blk = Llvm.insertion_block cx.builder in

    Llvm.position_at_end cont_blk cx.builder ;
    Llvm.build_phi [(eval_then, then_blk); (eval_else, else_blk)] "" cx.builder

let codegen_int args = failwith "unimplemented: codegen_int"
let codegen_true args = failwith "unimplemented: codegen_true"
let codegen_false args = failwith "unimplemented: codegen_false"

let int_ = Codegen.Codegen (Ident.intern "int", codegen_int)
let true_ = Codegen.Codegen (Ident.intern "true", codegen_true)
let false_ = Codegen.Codegen (Ident.intern "false", codegen_false)

let basic_constants = [int_ ; true_ ; false_] ;;

let if_ = Codegen.Codegen (Ident.intern "if", codegen_if)

let runtime_primitives = [
    Codegen.Runtime (Ident.intern "ml_cons", 2) ;
    Codegen.Runtime (Ident.intern "ml_car",  1) ;
    Codegen.Runtime (Ident.intern "ml_cdr",  1) ;
    Codegen.Runtime (Ident.intern "ml_add",  2) ;
]
