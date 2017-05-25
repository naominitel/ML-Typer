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
    | expr EOF                      { (($startpos, $endpos), `Expr $1) }
    | defs EOF                      { (($startpos, $endpos), `Defs $1) }
    | error EOF                     { (($startpos, $endpos), `ParseError "syntax error") }
    | EOF                           { (($startpos, $endpos), `Defs []) }
    ;

defs:
    | DEF pattern EQ expr           { [($2, $4)] }
    | DEF pattern EQ expr defs      { ($2, $4) :: $5 }
    ;

expr:
    | cons                          { $1 }
    | expr EQ cons                  { (($startpos, $endpos), `BinOp (`Eq, $1, $3)) }
    ;

cons:
    | arith CONS cons               { (($startpos, $endpos), `BinOp (`Cons, $1, $3)) }
    | arith                         { $1 }
    ;

arith:
    | arith PLUS term               { (($startpos, $endpos), `BinOp (`Plus, $1, $3)) }
    | arith MINUS term              { (($startpos, $endpos), `BinOp (`Minus, $1, $3)) }
    | term                          { $1 }
    ;

term:
    | term MULT apply               { (($startpos, $endpos), `BinOp (`Mult, $1, $3)) }
    | term DIV apply                { (($startpos, $endpos), `BinOp (`Div, $1, $3)) }
    | apply                         { $1 }
    ;

apply:
    | apply factor                  { (($startpos, $endpos), `Apply ($1, $2)) }
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
    | IF expr THEN expr ELSE expr   { (($startpos, $endpos), `If ($2, $4, $6)) }
    ;

fun_expr:
    | FUN pattern ARR expr          { (($startpos, $endpos), `Fun ($2, $4)) }
    ;

let_expr:
    | LET pattern EQ expr IN expr       { (($startpos, $endpos), `Let (false, $2, $4, $6)) }
    | LET REC pattern EQ expr IN expr   { (($startpos, $endpos), `Let (true,  $3, $5, $7)) }
    ;

match_expr:
    | MATCH expr WITH arm_list      { (($startpos, $endpos), `Match ($2, snd $4)) }
    | MATCH expr WITH PIPE arm_list { (($startpos, $endpos), `Match ($2, snd $5)) }
    ;

arm_list:
    | arm                           { (($startpos, $endpos), [snd $1]) }
    | arm PIPE arm_list             { (($startpos, $endpos), snd $1 :: snd $3) }
    ;

arm:
    | pattern ARR expr              { (($startpos, $endpos), ($1, $3)) }
    ;

pattern:
    | USCO                          { (($startpos, $endpos), `PatWildcard) }
    | ID                            { (($startpos, $endpos), `PatVar $1) }
    | OP tuple_pat CL               { (($startpos, $endpos), `PatTup (snd $2)) }
    | INT                           { (($startpos, $endpos), `PatCst $1) }
    | OP CL                         { (($startpos, $endpos), `PatUnit) }
    | OP error CL                   { (($startpos, $endpos), `ParseError "") }
    | OP pattern CL                 { $2 }
    ;

tuple_pat:
    | pattern COMMA pattern         { (($startpos, $endpos), [$1; $3]) }
    | pattern COMMA tuple_pat       { (($startpos, $endpos), $1 :: snd $3) }
    ;

entity:
    | OP expr CL                    { $2 }
    | OP error CL                   { (($startpos, $endpos), `ParseError "") }
    | OP tuple CL                   { (($startpos, $endpos), `Tuple (snd $2)) }
    | OP CL                         { (($startpos, $endpos), `Unit) }
    | BROP list BRCL                { (($startpos, $endpos), `List $2) }
    | BROP BRCL                     { (($startpos, $endpos), `List []) }
    | INT                           { (($startpos, $endpos), `Cst $1) }
    | ID                            { (($startpos, $endpos), `Var $1) }
    ;

list:
    | expr                          { [$1] }
    | expr SEMI list                { $1 :: $3  }
    ;

tuple:
    | expr COMMA expr               { (($startpos, $endpos), [$1; $3]) }
    | expr COMMA tuple              { (($startpos, $endpos), $1 :: snd $3) }
    ;
