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
%type  <Ast.defs> main

%%

main:
    | expr EOF                      { (range 1 1, `Expr $1) }
    | defs EOF                      { (range 1 1, `Defs $1) }
    | error EOF                     { (range 1 1, `ParseError "syntax error") }
    | EOF                           { (range 1 1, `Defs []) }
    ;

defs:
    | DEF pattern EQ expr           { [($2, $4)] }
    | DEF pattern EQ expr defs      { ($2, $4) :: $5 }
    ;

expr:
    | cons                          { $1 }
    | expr EQ cons                  { (range 1 3, `BinOp (`Eq, $1, $3)) }
    ;

cons:
    | arith CONS cons               { (range 1 3, `BinOp (`Cons, $1, $3)) }
    | arith                         { $1 }
    ;

arith:
    | arith PLUS term               { (range 1 3, `BinOp (`Plus, $1, $3)) }
    | arith MINUS term              { (range 1 3, `BinOp (`Minus, $1, $3)) }
    | term                          { $1 }
    ;

term:
    | term MULT factor              { (range 1 3, `BinOp (`Mult, $1, $3)) }
    | term DIV factor               { (range 1 3, `BinOp (`Div, $1, $3)) }
    | apply                         { $1 }
    ;

apply:
    | expr factor                   { (range 1 2, `Apply ($1, $2)) }
    | factor                        { $1 }
    ;

factor:
    | if_expr                       { $1 }
    | fun_expr                      { $1 }
    | let_expr                      { $1 }
    | match_expr                    { $1 }
    | entity                        { $1 }
    ;

if_expr:
    | IF expr THEN expr ELSE expr   { (range 1 6, `If ($2, $4, $6)) }
    ;

fun_expr:
    | FUN pattern ARR expr          { (range 1 4, `Fun ($2, $4)) }
    ;

let_expr:
    | LET pattern EQ expr IN expr       { (range 1 6, `Let (false, $2, $4, $6)) }
    | LET REC pattern EQ expr IN expr   { (range 1 6, `Let (true,  $3, $5, $7)) }
    ;

match_expr:
    | MATCH expr WITH arm_list      { (range 1 4, `Match ($2, snd $4)) }
    | MATCH expr WITH PIPE arm_list { (range 1 5, `Match ($2, snd $5)) }
    ;

arm_list:
    | arm                           { (range 1 1, [snd $1]) }
    | arm PIPE arm_list             { (range 1 3, snd $1 :: snd $3) }
    ;

arm:
    | pattern ARR expr              { (range 1 3, ($1, $3)) }
    ;

pattern:
    | USCO                          { (range 1 1, `PatWildcard) }
    | ID                            { (range 1 1, `PatVar $1) }
    | OP tuple_pat CL               { (range 1 3, `PatTup (snd $2)) }
    | INT                           { (range 1 1, `PatCst $1) }
    | OP CL                         { (range 1 2, `PatUnit) }
    | OP error CL                   { (range 2 2, `ParseError "") }
    | OP pattern CL                 { $2 }
    ;

tuple_pat:
    | pattern COMMA pattern         { (range 1 3, [$1; $3]) }
    | pattern COMMA tuple_pat       { (range 1 3, $1 :: snd $3) }
    ;

entity:
    | OP expr CL                    { $2 }
    | OP error CL                   { (range 2 2, `ParseError "") }
    | OP tuple CL                   { (range 1 3, `Tuple (snd $2)) }
    | OP CL                         { (range 1 2, `Unit) }
    | BROP list BRCL                { (range 1 3, `List $2) }
    | BROP BRCL                     { (range 1 2, `List []) }
    | INT                           { (range 1 1, `Cst $1) }
    | ID                            { (range 1 1, `Var $1) }
    ;

list:
    | expr                          { [$1] }
    | expr SEMI list                { $1 :: $3  }
    ;

tuple:
    | expr COMMA expr               { (range 1 3, [$1; $3]) }
    | expr COMMA tuple              { (range 1 3, $1 :: snd $3) }
    ;
