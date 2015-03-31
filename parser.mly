%{
    open Codemap
%}

%token <Ident.t> ID
%token <char> UNKNOWN
%token LAMBDA DOT COL
%token OP CL
%token EOF

%start <(('ast Ast.ast) as 'ast) option> main

%%

main:
    | e = expr EOF { Some e }
    | EOF { None }
    ;

expr:
    | f = expr arg = simple
        {{ sp = ($startpos(f), $endpos(arg)) ; d = Ast.App (f, arg) }}
    | s = simple { s }
    ;

simple:
    | var = ID
        {{ sp = ($startpos(var), $endpos(var)) ; d = Ast.Var var }}
    | LAMBDA v = ID DOT e = expr
        {{
           sp = ($startpos(v), $endpos(e)) ;
           d = Ast.Abs ({ sp = ($startpos(e), $endpos(e)) ; d = v }, None, e)
        }}
    | LAMBDA v = ID COL ty = expr DOT e = expr
        {{
           sp = ($startpos(v), $endpos(e)) ;
           d = Ast.Abs ({ sp = ($startpos(e), $endpos(e)) ; d = v }, Some ty, e)
        }}
    | OP e = expr CL { e }
