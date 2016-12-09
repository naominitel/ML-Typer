(*
 * a re-expanded ast that contains enough information for codegen
 * - functions are uncurryed
 * - applications are n-ary
 * - beta-redexes are identified
 *)

type cst =
    | Unit
    | Null
    | Int of int
    | Bool of bool
    | Float of float

type ast =
    | Cst of cst
    | Var of Ident.t
    | Closure of Ident.t list * ast
    | Call of ast * ast list
    | Let of bool * Ident.t * ast * ast

(*
 * builtin symbols and primitives. will be inserted in a specialthe environment
 * along with functions telling how to compile their invocations (for codegen
 * primitives such as if) or just pointers to external functions (for runtime
 * functions like plus or alloc)
 *)

let if_       = Ident.gensym ()
let make_tup  = Ident.gensym ()
let make_list = Ident.gensym ()
let eq        = Ident.gensym ()
let cons      = Ident.gensym ()
let plus      = Ident.gensym ()
let minus     = Ident.gensym ()
let mult      = Ident.gensym ()
let div       = Ident.gensym ()

exception Codegen_error of string
let error str = raise (Codegen_error str)

(* convert from the original AST to the re-expanded AST *)

let from_pat (_, pat) = match pat with
    | `PatVar x -> x
    | `PatWildcard -> Ident.gensym ()
    | _ -> error "pattern binding is not implemented in codegen"

let from_op = function
    | `Eq -> eq
    | `Cons -> cons
    | `Plus -> plus
    | `Minus -> minus
    | `Mult -> mult
    | `Div -> div

let rec from_ast (_, ast) = match ast with
    | `Unit -> Cst Unit
    | `Cst i -> Cst (Int i)
    | `Var id -> Var id
    | `Tuple args -> Call (Var make_tup, (List.map from_ast args))
    | `List args -> Call (Var make_list, (List.map from_ast args))
    | `If (cond, etrue, efalse) ->
        Call (Var if_, [from_ast cond ; from_ast etrue ; from_ast efalse])
    | `Fun (arg, expr) ->
        let arg = from_pat arg in
        begin match from_ast expr with
            | Closure (args, expr) -> Closure (arg :: args, expr)
            | expr -> Closure ([arg], expr)
        end
    | `Let (isrec, pat, expr, body) ->
        Let (isrec, from_pat pat, from_ast expr, from_ast body)
    | `Match _ -> error "pattern matching is not implemented in codegen"
    | `Apply (f, arg) ->
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
    | `BinOp (op, l, r) -> Call (Var (from_op op), [from_ast l ; from_ast r])

(* actual codegen code *)

let global_cx = Llvm.global_context ()

(*
 * the only static information that is used by this codegen module is arity.
 * one must differentiate between two things:
 * - typing arity is the arity seen from the typing PoV: it exactly corresponds
 *   to how many arguments can be applied according to typing rules. for example,
 *   an expression which has type a -> b -> c always have a typing arity of 2.
 * - actual arity is how many arguments can be "sent" at once when compiled.
 *   it differs from the typing arity because of curryfying and partial
 *   application. for example, (fun x -> let y = 0 in fun z -> x + y + z)
 *   will have an actual arity of 1 because it will be compiled down to an
 *   LLVM function taking a single argument and returing a function taking a
 *   single argument, whereas (fun x -> fun y -> x + y) will be packed into
 *   a single LLVM function taking 2 parameters at once.
 *
 * The actual arity is determined once before starting the code generation by
 * packing chains of function expressions into a single Closure expression
 * that stores the arity. It's then kept in an environment to be used to
 * generate the most efficient code as possible when compiling function calls.
 *
 * In some cases, however, the information is lost. map, for example. cannot
 * know wether its function argument is a single-argument function, or a
 * multiple-argument function that will be partially applied. In those cases,
 * we call a primitive ml_apply_N, where N is number of arguments provided.
 * This primitive will dynamically lookup arity information and make the call.
 *
 * This is represented by the following type. Fun (x, sub) denotes an expression
 * that will be compiled down to a function taking x parameters and returning
 * something that has arity sub, and Ty either denotes an expression which is
 * not a function, or an expression whose actual arity is unknown (like a parameter)
 *)

type arity =
    | Unknown
    | Fun of int * arity

(*
 * the codegen context. contains information that are gathered during code
 * generation:
 * - arity_env keeps track of the actual arity of the functions (see below)
 * - tr_env keeps track of compiled symbols from the environment
 * - prim_env contains the caml functions to compile calls tp builtin primitives
 * - llctxt is the Llvm context
 * - llmod is the current Llvm module
 * - llbuilder is the current Llvm builder
 *)

type 'a env = (Ident.t * 'a) list

type variable = {
    arity: arity ;
    value: [
        | `Value of Llvm.llvalue
        | `External of Llvm.llvalue
        | `SpecialForm of (trans_context -> ast list -> Llvm.llvalue)
    ]
}

and trans_context = {
    tr_env      : variable env ;
    prim_env    : (trans_context -> ast list -> Llvm.llvalue) env ;
    llctxt      : Llvm.llcontext ;
    llmod       : Llvm.llmodule ;
    llbuilder   : Llvm.llbuilder
}

(* lookup in the arity environment *)

let rec arity expr cx = match expr with
    | Var v -> (List.assq v cx.tr_env).arity
    | Closure (args, e) -> Fun (List.length args, arity e cx)
    | _ -> Unknown

let value_of id cx = match (List.assq id cx.tr_env).value with
    | `Value v -> v
    | `External v -> v
    | `SpecialForm _ -> failwith "attempting to take value of special form"

(* the (LLVM) type of an ML value ( basically void* ) *)

let mlval_ty = Llvm.pointer_type (Llvm.i8_type global_cx)

(* codegen of a constant expression *)

let rec codegen_const cx = function
    | Int i      -> Llvm.const_int (Llvm.integer_type cx.llctxt 64) i
    | Bool true  -> Llvm.const_int (Llvm.integer_type cx.llctxt 8) 1
    | Bool false -> Llvm.const_int (Llvm.integer_type cx.llctxt 8) 0
    | Null       -> Llvm.const_null mlval_ty
    | Unit       -> codegen_const cx (Int 1)
    | Float f    -> error "floats are not implemented in codegen"

(*
 * generates the LLVM type for a function that takes argc arguments of type
 * mlval_ty and returns a value of type mlval_ty. this is used to ensure calls
 * typecheck in LLVM.
 *)

let make_fn_ty argc = Llvm.function_type mlval_ty @@ Array.make argc mlval_ty

(*
 * generates a function definition.
 * FIXME: the name of the function is currently arbitrarly generated. this
 * makes the generated code unreadable. one should somehow keep the name of
 * where the function was defined, when it's not a lambda
 *)

let gen_fun_name =
    let name = ref 0 in
    fun () ->
        let ret = !name in
        name := !name + 1 ;
        Printf.sprintf "ml_fun_%d" ret

let make_function md arity =
    Llvm.declare_function (gen_fun_name ()) (make_fn_ty (arity + 1)) md

(* generates a declaration for an external function *)

let make_ext_function md name arity =
    Llvm.declare_function name (make_fn_ty arity) md

(*
 * creates a ml_apply_N (see above about arity
 * FIXME: currently this only creates an external function definition so that
 * LLVM is happy and can generate calls to it. the actual code of the function
 * for small values of N will be written in C and linked at runtime. but it
 * would be better it this function could actually generate the code itself.
 *)

let make_ml_apply_fun cx argc =
    let fun_name = Printf.sprintf "ml_apply_%d" argc in
    match Llvm.lookup_function fun_name cx.llmod with
        | Some f -> f
        | None -> make_ext_function cx.llmod fun_name (argc + 1)

(*
 * generates code for an allocation of an ML block of the given size.
 * FIXME: we should use the environment rather than using Llvm to retreive
 * a pointer to the ml_alloc function
 *)

let make_ml_alloc cx size =
    match Llvm.lookup_function "ml_alloc" cx.llmod with
        | Some f ->
            let size = Llvm.const_inttoptr (codegen_const cx (Int size)) mlval_ty in
            Llvm.build_call f [|size|] "" cx.llbuilder
        | None -> error "missing ml_alloc"

(*
 * generates code to call the ml_blcopy function that copies all fields
 * from the src ML block to the dst ML block. all blocks should be of the
 * same size len.
 * FIXME: same as above, ml_blcopy should be in the environment
 * FIXME: THIS. SHOULD. BE. INLINED. which won't be possible while ml_blcopy
 * will be an external function implemented in C as it is now. this makes
 * creating recursive closures really inefficient.
 *)

let make_ml_blcopy cx dst src len =
    match Llvm.lookup_function "ml_blcopy" cx.llmod with
        | Some f ->
            let len = Llvm.const_inttoptr (codegen_const cx (Int len)) mlval_ty in
            Llvm.build_call f [|dst; src; len|] "" cx.llbuilder
        | None -> error "missing ml_blcopy"

(* generates code for reading/writing in a field of an ML block *)

let make_ml_fetch cx blk idx =
    let blk = Llvm.build_pointercast blk (Llvm.pointer_type mlval_ty) "" cx.llbuilder
    in
    let field = Llvm.build_gep blk [|Llvm.const_int (Llvm.i32_type cx.llctxt) idx|]
                               "" cx.llbuilder
    in Llvm.build_load field "" cx.llbuilder

let make_ml_store cx blk idx data =
    let blk = Llvm.build_pointercast blk (Llvm.pointer_type mlval_ty)
             "" cx.llbuilder
    in
    let field = Llvm.build_gep blk [|Llvm.const_int (Llvm.i32_type cx.llctxt) idx|]
                               "" cx.llbuilder
    in
    let data = Llvm.build_bitcast data mlval_ty "" cx.llbuilder in
    ignore @@ Llvm.build_store data field cx.llbuilder

(* find the free variables of the given expression *)

let rec find_free_vars env prims expr =
    let open Batteries in
    let rec iter env = function
        | Cst _ -> []
        | Var v -> if List.memq v env || List.mem_assq v prims then [] else [v]
        | Closure (ids, expr) -> iter (ids @ env) expr
        | Call (f, args) ->
            (iter env f) @
            (List.fold_left (fun acc e -> acc @ iter env e) [] args)
        | Let (isrec, id, expr, body) ->
            iter (if isrec then id :: env else env) expr
            @ iter (id :: env) body
    in List.sort_unique compare (iter env expr)

(* codegen for function application *)

let rec codegen_apply cx f args =
    let arity = arity f cx in
    let argc = List.length args in

    (* compile functional expr *)
    let eval_fun = codegen_expr cx f in

    (* compile arguments *)
    let eval_args = Array.of_list @@ List.map (codegen_expr cx) args in

    let rec iter clos arity first_arg argc = match arity with
        | Unknown ->
            (*
             * stop. at this point we don't know anymore how many
             * args we can give at a time, so we just use ml_apply
             *)
            if argc == 0 then failwith "eh" ;
            let ml_apply = make_ml_apply_fun cx argc in
            let args = Array.append (Array.sub eval_args first_arg argc)
                                    [|clos|]
            in Llvm.build_call ml_apply args "" cx.llbuilder
        | Fun (arity, next) ->
            if argc < arity then (
                (* partial application. allocate a closure and stop here *)
                let closure = make_ml_alloc cx @@ argc + 1 in

                (* store the function arguments in the closure's envrironment *)
                make_ml_store cx closure 1 eval_fun ;

                for i = 0 to argc - 1 do
                    make_ml_store cx closure (i + 2) eval_args.(first_arg + i)
                done ;

                (* code for the closure itself *)
                let code_ptr = make_function cx.llmod arity in
                let block = Llvm.append_block cx.llctxt "" code_ptr in
                let more_args = Llvm.params code_ptr in

                make_ml_store cx closure 0 code_ptr ;

                (*
                 * inside the closure: fetch args
                 * and function from the environment
                 *)
                let blk = Llvm.insertion_block cx.llbuilder in
                Llvm.position_at_end block cx.llbuilder ;

                let args_array =
                    Array.init
                        arity
                        (fun i ->
                             if i < argc then make_ml_fetch cx closure (i + 2)
                             else more_args.(i))
                in

                let fetch_fun = make_ml_fetch cx closure 1 in
                let func =
                    Llvm.build_bitcast fetch_fun (make_fn_ty arity) "" cx.llbuilder
                in
                ignore @@ Llvm.build_call func args_array "" cx.llbuilder ;
                Llvm.position_at_end blk cx.llbuilder ;
                closure
            ) else (
                (* full application. *)
                let pc = make_ml_fetch cx clos 0 in
                let func =
                    Llvm.build_bitcast
                         pc (Llvm.pointer_type (make_fn_ty (arity + 1)))
                         "" cx.llbuilder
                in
                let next_function =
                    Llvm.build_call
                        func (Array.append [|clos|]
                                (Array.sub eval_args first_arg arity))
                        "" cx.llbuilder
                in

                let args_left = (argc - arity) in
                if args_left > 0
                    then iter next_function next (first_arg + arity) args_left
                    else next_function
            )
    in iter eval_fun arity 0 argc

and codegen_decl cx (isrec, id, expr) =
    let rec expr_block_size env = function
        | Cst _ -> None
        | Var _ -> (try List.assq id env with Not_found -> None)
        | Closure (args, expr) ->
            let free = find_free_vars args cx.prim_env expr in
            Some (1 + List.length free)
        | Call _ -> None
        | Let (_, id, expr, body) ->
            let size = expr_block_size env expr in
            expr_block_size ((id, size) :: env) body
    in

    let arity = arity expr cx in
    let blsize = expr_block_size [] expr in

    let eval_expr = match blsize with
        | None -> codegen_expr cx expr
        | Some sz ->
            let value = make_ml_alloc cx sz in

            let dummy_val = {
                arity = arity ;
                value = `Value value ;
            } in

            let eval_expr =
                codegen_expr
                    (if isrec then
                        { cx with tr_env = (id, dummy_val) :: cx.tr_env }
                     else cx) expr
            in

            ignore @@ make_ml_blcopy cx value eval_expr sz ;
            value
    in

    { cx with
          tr_env = (id, { arity = arity ; value = `Value eval_expr }) :: cx.tr_env
    }

and codegen_expr cx = function
    | Cst c ->
        let const = codegen_const cx c in
        Llvm.const_inttoptr const mlval_ty
    | Var id -> value_of id cx
    | Closure (ids, expr) ->
        (* generate function *)
        let arity = List.length ids in
        let fd = make_function cx.llmod (arity + 1) in
        let blk = Llvm.append_block cx.llctxt "entry" fd in

        (* update env *)
        let args = Llvm.params fd in
        let new_env =
            List.mapi
                (fun i id -> (id, { value = `Value (args.(i + 1)) ;
                                    arity = Unknown })) ids
        in

        let free = find_free_vars ids cx.prim_env expr in

        (*
         * all closures are recursive.
         * closure[0] = closure
         * closure[1] = PC
         * closure[2..N] = free variables
         *)
        let closure = make_ml_alloc cx (List.length free + 1) in
        List.iteri
            (fun i id ->
                try ignore @@ make_ml_store cx closure (i + 1) (value_of id cx)
                (* probably a primitive or a global. don't capture *)
                with Not_found -> ())
            free ;
        ignore @@ make_ml_store cx closure 0 fd ;

        (* generate body *)
        let prev_blk = Llvm.insertion_block cx.llbuilder in
        Llvm.position_at_end blk cx.llbuilder ;

        let getenv = args.(0) in
        let (_, new_env) =
            List.fold_left
                (fun (i, env) id ->
                    try
                        ignore @@ List.assq id cx.tr_env ;
                        let v = {
                            arity = (List.assq id cx.tr_env).arity ;
                            value = `Value (make_ml_fetch cx getenv (i + 1))
                        } in (i + 1, (id, v) :: env)
                    with Not_found -> (i + 1, env))
                (0, new_env) free ;
        in

        let ret = codegen_expr { cx with tr_env = new_env @ cx.tr_env } expr in

        ignore @@ Llvm.build_ret ret cx.llbuilder ;
        ignore @@ Llvm.position_at_end prev_blk cx.llbuilder ;

        closure
    | Call (f, args) ->
        begin match f with
            | Var id ->
                let prim = try Some (List.assq id cx.prim_env)
                           with Not_found -> None
                in begin match prim with
                    | Some prim -> prim cx args
                    | None -> codegen_apply cx f args
                end
            | _ -> codegen_apply cx f args
        end
    | Let (isrec, id, expr, body) ->
        codegen_expr (codegen_decl cx (isrec, id, expr)) body

let codegen_ffi_call func args = failwith "unimpl"
let make_foreign_function name arity = failwith "unimpl"

module Primitives = struct
    module CG = struct
        let codegen_if cx args =
            let (cond, branch_true, branch_false) = match args with
                | [c; bt; bf] -> (c, bt, bf)
                | _ -> error "bad arguments for if"
            in

            let cur_block = Llvm.insertion_block cx.llbuilder in
            let func = Llvm.block_parent cur_block in

            let then_blk = Llvm.append_block cx.llctxt "then" func in
            let else_blk = Llvm.append_block cx.llctxt "else" func in
            let cont_blk = Llvm.append_block cx.llctxt "cont" func in

            (* codegen branch *)
            let eval_cond = Llvm.build_ptrtoint (codegen_expr cx cond) (Llvm.i1_type cx.llctxt) "" cx.llbuilder in
            ignore @@ Llvm.build_cond_br eval_cond then_blk else_blk cx.llbuilder ;

            (* codegen true *)
            Llvm.position_at_end then_blk cx.llbuilder ;
            let eval_then = codegen_expr cx branch_true in
            ignore @@ Llvm.build_br cont_blk cx.llbuilder ;
            let then_blk = Llvm.insertion_block cx.llbuilder in

            (* codegen false *)
            Llvm.position_at_end else_blk cx.llbuilder ;
            let eval_else = codegen_expr cx branch_false in
            ignore @@ Llvm.build_br cont_blk cx.llbuilder ;
            let else_blk = Llvm.insertion_block cx.llbuilder in

            Llvm.position_at_end cont_blk cx.llbuilder ;
            Llvm.build_phi [(eval_then, then_blk); (eval_else, else_blk)]
            "" cx.llbuilder

        let codegen_make_tup cx args =
            let nargs = List.length args in
            let tup = make_ml_alloc cx nargs in
            List.iteri (make_ml_store cx tup) (List.map (codegen_expr cx) args) ;
            tup

        let rec codegen_make_list cx args =
            match args with
                | [] -> codegen_const cx Null
                | (x :: t) ->
                    let blk = make_ml_alloc cx 2 in
                    ignore @@ make_ml_store cx blk 0 (codegen_expr cx x) ;
                    ignore @@ make_ml_store cx blk 1 (codegen_make_list cx t) ;
                    blk

        let init_env = [
            (if_,       codegen_if) ;
            (make_tup,  codegen_make_tup) ;
            (make_list, codegen_make_list) ;
        ]
    end

    module RT = struct
        let codegen_prim f cx args =
            Llvm.build_call f (Array.of_list (List.map (codegen_expr cx) args))
                            "" cx.llbuilder

        let intern = Ident.intern
        let gensym = Ident.gensym

        let make_primitives md = [
            (eq,     codegen_prim (make_ext_function md "ml_eq"    2)) ;
            (plus,   codegen_prim (make_ext_function md "ml_add"   2)) ;
            (minus,  codegen_prim (make_ext_function md "ml_minus" 2)) ;
            (mult,   codegen_prim (make_ext_function md "ml_mult"  2)) ;
            (div,    codegen_prim (make_ext_function md "ml_div"   2)) ;
            (cons,   codegen_prim (make_ext_function md "ml_cons"  2)) ;

            (*
             * FIXME: for now those are here, but they should be in the standard
             * environment as they use user-visible symbols. but unfortunately
             * codegen currently assumes a function in the standard environment
             * is an ML closure
             *)
            (intern "fst",       codegen_prim (make_ext_function md "ml_fst" 1)) ;
            (intern "snd",       codegen_prim (make_ext_function md "ml_snd" 1)) ;
            (intern "print_int", codegen_prim (make_ext_function md "ml_print_int" 1)) ;

            (gensym (), codegen_prim (make_ext_function md "ml_alloc"  1)) ;
            (gensym (), codegen_prim (make_ext_function md "ml_blcopy" 3))
        ]
    end
end

(* entry point for code generation *)

let codegen_module name defs =
    let md = Llvm.create_module global_cx name in

    let init_env =
        Primitives.CG.init_env @
        Primitives.RT.make_primitives md
    in

    (* create context *)
    let cx = {
        tr_env = [] ;
        prim_env = init_env ;
        llmod = md ;
        llctxt = global_cx ;
        llbuilder = Llvm.builder global_cx
    } in

    (* create the main function *)
    let ty = Llvm.function_type (Llvm.void_type cx.llctxt) [||] in
    let entry = Llvm.declare_function "ml_main" ty cx.llmod in
    let block = Llvm.append_block cx.llctxt "entry" entry in
    Llvm.position_at_end block cx.llbuilder ;

    (* codegen definitions *)
    ignore @@ List.fold_left codegen_decl cx defs ;
    ignore @@ Llvm.build_ret_void cx.llbuilder ;
    md
