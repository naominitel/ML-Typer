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
    open Lexing

    let sp s e expr = { sp = Some (s, e) ; d = expr }

    let eq    = Ident.intern "="
    let cons  = Ident.intern "::"
    let plus  = Ident.intern "+"
    let minus = Ident.intern "-"
    let mult  = Ident.intern "*"
    let div   = Ident.intern "/"

    let binop f a1 a2 =
        Ast.App (
            Codemap.dummy_spanned @@ Ast.App (
                Codemap.dummy_spanned @@ Ast.Var f,
                a1
            ),
            a2
        )
%}

%start main
%token EOF
%token DEF
%token OP CL BROP BRCL
%token EQ COMMA PIPE ARR USCO SEMI
%token IF THEN ELSE FUN LET REC IN MATCH WITH
%token PLUS MINUS MULT DIV CONS
%token <char> UNKNOWN
%token <int> INT
%token <Ident.t> ID
%token <Ident.t> CTOR
%type  <(('b, 'a) Ast.ast as 'a) option> main

%%

main:
    | expr EOF                      { Some ($1) }
    | error EOF                     { None }
    | EOF                           { None }
    ;

expr:
    | expr EQ cons                  { sp $startpos $endpos @@ binop eq $1 $3 }
    | cons                          { $1 }
    ;

cons:
    | arith CONS cons               { sp $startpos $endpos @@ binop cons $1 $3 }
    | arith                         { $1 }
    ;

arith:
    | arith PLUS term               { sp $startpos $endpos @@ binop plus $1 $3 }
    | arith MINUS term              { sp $startpos $endpos @@ binop minus $1 $3 }
    | term                          { $1 }
    ;

term:
    | term MULT factor              { sp $startpos $endpos @@ binop mult $1 $3 }
    | term DIV factor               { sp $startpos $endpos @@ binop div $1 $3 }
    | apply                         { $1 }
    ;

apply:
    | expr factor                   { sp $startpos $endpos @@ Ast.App ($1, $2) }
    | factor                        { $1 }
    ;

factor:
    (* | if_expr                       { $1 } *)
    (* | let_expr                      { $1 } *)
    (* | match_expr                    { $1 } *)
    | fun_expr                      { $1 }
    | entity                        { $1 }
    ;

    (*
if_expr:
    | IF expr THEN expr ELSE expr   { sp $startpos $endpos @@ `If ($2, $4, $6) }
    ;
    *)

fun_expr:
    | FUN pattern ARR expr          { sp $startpos $endpos @@ Ast.Abs ($2, None, $4) }
    ;

    (*
let_expr:
    | LET pattern EQ expr IN expr       { sp $startpos $endpos @@ `Let (false, $2, $4, $6) }
    | LET REC pattern EQ expr IN expr   { sp $startpos $endpos @@ `Let (true,  $3, $5, $7) }
    ;

match_expr:
    | MATCH expr WITH arm_list      { sp $startpos $endpos @@ `Match ($2, snd $4) }
    | MATCH expr WITH PIPE arm_list { sp $startpos $endpos @@ `Match ($2, snd $5) }
    ;

arm_list:
    | arm                           { sp $startpos $endpos @@ [snd $1] }
    | arm PIPE arm_list             { sp $startpos $endpos @@ snd $1 :: snd $3 }
    ;

arm:
    | pattern ARR expr              { sp $startpos $endpos @@ ($1, $3) }
    ;
    *)

pattern:
    (*
    | USCO                          { sp $startpos $endpos @@ `PatWildcard }
    | ID                            { sp $startpos $endpos @@ `PatVar $1 }
    | OP tuple_pat CL               { sp $startpos $endpos @@ `PatTup $2 }
    | INT                           { sp $startpos $endpos @@ `PatCst $1 }
    | OP CL                         { sp $startpos $endpos @@ `PatUnit }
    | OP error CL                   { sp $startpos $endpos @@ `ParseError "" }
    | OP pattern CL                 { $2 }
    *)
    | ID                            { sp $startpos $endpos $1 }
    ;

    (*
tuple_pat:
    | pattern COMMA pattern         { [$1; $3] }
    | pattern COMMA tuple_pat       { $1 :: $3 }
    ;
    *)

entity:
    (* | OP tuple CL                   { sp $startpos $endpos @@ `Tuple $2 } *)
    (* | OP CL                         { sp $startpos $endpos @@ `Unit } *)
    (* | BROP list BRCL                { sp $startpos $endpos @@ `List $2 } *)
    (* | BROP BRCL                     { sp $startpos $endpos @@ `List [] } *)
    (* | INT                           { sp $startpos $endpos @@ `Cst $1 } *)
    | OP expr CL                    { $2 }
    | ID                            { sp $startpos $endpos @@ Ast.Var $1 }
    ;

    (*
list:
    | expr                          { [$1] }
    | expr SEMI list                { $1 :: $3  }
    ;

tuple:
    | expr COMMA expr               { [$1; $3] }
    | expr COMMA tuple              { $1 :: $3 }
    ;
    *)
