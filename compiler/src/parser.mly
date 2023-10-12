%{
    open Location
    open Langsyntax

    let mk_location l_start l_end = { l_start; l_end }
%}

%token EOF
%token COLON
%token COMMA
%token LPAREN RPAREN
%token LCURLY RCURLY
%token EQ
%token DOT
%token CALL
%token COMPONENT
%token ENTITY
%token FUN
%token <string> IDENT
%token INT
%token <int> NUMBER
%token PERIODIC
%token SEMICOLON
%token SYSTEM
%token TRIGGER
%token UNIT
%token VAR

%start file
%type <Langsyntax.file> file

%%

file:
| component EOF { Component $1 }
| system EOF { System $1 }
;

component: COMPONENT IDENT list(fundecl)
    { { c_funs = $3; c_loc = mk_location $startpos $endpos } }
;

system:
| SYSTEM IDENT COLON list(trigger)
    { { s_name = $2;
        s_triggers = $4;
        s_loc = mk_location $startpos $endpos } }


fundecl: FUN IDENT LPAREN separated_list(COMMA, typ) RPAREN COLON typ
    { { fd_name = $2;
        fd_args = $4;
        fd_ty = $7;
        fd_loc = mk_location $startpos $endpos } }
;

exp:
| IDENT { { e_desc = Var $1; e_loc = mk_location $startpos $endpos } }

stmt:
| IDENT EQ exp
  { { st_desc = Equation ($1, $3);
      st_loc = mk_location $startpos $endpos } }
| CALL IDENT DOT IDENT LPAREN separated_list(COMMA, exp) RPAREN
  { { st_desc = Call ($2, $4, $6);
      st_loc = mk_location $startpos $endpos } }

stmts: separated_list(SEMICOLON, stmt)
    { $1 }

block: LCURLY stmts RCURLY
    { { blk_stmts = $2;
        blk_loc = mk_location $startpos $endpos } }

trigger: TRIGGER trigger_cond COLON block
    { { tr_cond = $2;
        tr_action = $4;
        tr_loc = mk_location $startpos $endpos } }

trigger_cond:
| PERIODIC NUMBER { Periodic $2 }

typ:
| UNIT { Unit }
| INT { Int }
;
