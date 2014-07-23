/*
 * Parser for a simple ML-like language
 * supported features:
 *   - basic integer arithmetics
 *   - tuples
 *   - sum types
 *   - conditional expressions
 *   - unary functions
 *   - let ... in form
 *   - pattern matching
 */

%{
    open Ast
    open Codemap
    open Errors
    open Lexing

    let range s e =
        let sp = rhs_start_pos s in
        let ep = rhs_end_pos e in
        let sp = {
            fileno = 0 ;
            line = sp.pos_lnum ;
            col = sp.pos_cnum - sp.pos_bol + 1
        } in
        let ep = {
            fileno = 0 ;
            line = ep.pos_lnum ;
            col = ep.pos_cnum - ep.pos_bol + 1
        } in
        (sp, ep)

    let span r s e = { sp = range s e ; l = `Unty ; r = r }
%}

%start main
%token EOF
%token DEF
%token OP CL
%token EQ COMMA PIPE ARR USCO
%token IF THEN ELSE FUN LET IN MATCH WITH
%token PLUS MINUS MULT DIV
%token <char> UNKNOWN
%token <int> INT
%token <string> ID
%token <string> CTOR
%type  <[ `Unty | `Annot of Types.ty ] Ast.defs Errors.t> main

%%

main:
    | expr EOF  { map (fun x -> { sp = range 1 1 ; d = `Expr x }) $1 }
    | defs EOF  { map (fun x -> { sp = range 1 1 ; d = `Defs x }) (sequence $1) }
    | EOF       { Ok { sp = range 1 1 ; d = `Defs [] } }
    | error EOF { new_err (range 1 1) "unexpected token" }
    ;

defs:
    | DEF pattern EQ expr      { [map2 (fun p e -> (p, e)) $2 $4] }
    | DEF pattern EQ expr defs { (map2 (fun p e -> (p, e)) $2 $4) :: $5 }
    ;

expr:
    | arith      { $1 }
    | expr arith { map2 (fun e a -> span (`Apply (e, a)) 1 2) $1 $2 }
    ;

arith:
    | arith PLUS term  { map2 (fun a t -> span (`BinOp (`Plus,  a, t)) 1 3) $1 $3 }
    | arith MINUS term { map2 (fun a t -> span (`BinOp (`Minus, a, t)) 1 3) $1 $3 }
    | term             { $1 }
    ;

term:
    | term MULT factor { map2 (fun t f -> span (`BinOp (`Mult, t, f)) 1 3) $1 $3 }
    | term DIV factor  { map2 (fun t f -> span (`BinOp (`Div,  t, f)) 1 3) $1 $3 }
    | factor           { $1 }
    ;

factor:
    | if_expr    { $1 }
    | fun_expr   { $1 }
    | let_expr   { $1 }
    | match_expr { $1 }
    | entity     { $1 }
    ;

if_expr:
    | IF expr THEN expr ELSE expr {
        map3 (fun e1 e2 e3 -> span (`If (e1, e2, e3)) 1 6) $2 $4 $6
    }
    ;

fun_expr:
    | FUN pattern ARR expr {
        map2 (fun p e -> span (`Fun (p, e)) 1 4) $2 $4
    }
    ;

let_expr:
    | LET pattern EQ expr IN expr {
        map3 (fun p e1 e2 -> span (`Let (p, e1, e2)) 1 6) $2 $4 $6
    }
    ;

match_expr:
    | MATCH expr WITH arm_list {
        map2 (fun e a -> span (`Match (e, a)) 1 4) $2 (sequence $4)
    }
    | MATCH expr WITH PIPE arm_list {
        map2 (fun e a -> span (`Match (e, a)) 1 5) $2 (sequence $5)
    }
    ;

arm_list:
    | arm               { [$1] }
    | arm PIPE arm_list { $1 :: $3 }
    ;

arm:
    | pattern ARR expr { map2 (fun p e -> (p, e)) $1 $3 }
    ;

pattern:
    | CTOR pattern    { map (fun p -> span (`PatCtor ($1, p)) 1 2) $2 }
    | OP tuple_pat CL { map (fun t -> span (`PatTup t) 1 3) (sequence $2) }
    | OP error CL     { new_err (range 2 2) "syntax error" }
    | OP pattern CL   { $2 }
    | OP CL           { Ok (span `PatUnit 1 2) }
    | USCO            { Ok (span `PatWildcard 1 1) }
    | ID              { Ok (span (`PatVar $1) 1 1) }
    | INT             { Ok (span (`PatCst $1) 1 1) }
    ;

tuple_pat:
    | pattern COMMA pattern   { [$1; $3] }
    | pattern COMMA tuple_pat { $1 :: $3 }
    ;

entity:
    | CTOR expr   { map (fun e -> span (`Ctor ($1, e)) 1 2) $2 }
    | OP tuple CL { map (fun t -> span (`Tuple t) 1 3) (sequence $2) }
    | OP error CL { new_err (range 2 2) "syntax error" }
    | OP expr CL  { $2 }
    | OP CL       { Ok (span `Unit 1 2) }
    | ID          { Ok (span (`Var $1) 1 1) }
    | INT         { Ok (span (`Cst $1) 1 1) }
    ;

tuple:
    | expr COMMA expr  { [$1; $3] }
    | expr COMMA tuple { $1 :: $3 }
    ;
