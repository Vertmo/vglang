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
%token AND OR
%token GE GT LE LT
%token PLUS MINUS STAR SLASH
%token COMPONENT
%token ELSE
%token ENTITY
%token FOREACH
%token FUN
%token <string> IDENT
%token IF
%token INT
%token LAST
%token <int> NUMBER
%token PERIODIC
%token SEMICOLON
%token SYSTEM
%token THEN
%token TRIGGER
%token UNIT
%token VAR

%left AND OR
%left EQ GE GT LE LT
%left PLUS MINUS
%left STAR SLASH

%start file
%type <Langsyntax.file> file

%%

file:
| entity EOF { Entity $1 }
| component EOF { Component $1 }
| system EOF { System $1 }

entity: ENTITY IDENT LPAREN vardecls RPAREN VAR vardecls list(component_impl)
    { { e_name = $2; e_params = $4; e_vars = $7; e_comps = $8;
        e_loc = mk_location $startpos $endpos } }

component: COMPONENT IDENT list(fundecl)
    { { c_name = $2; c_funs = $3; c_loc = mk_location $startpos $endpos } }

system: SYSTEM IDENT COLON list(trigger)
    { { s_name = $2;
        s_triggers = $4;
        s_loc = mk_location $startpos $endpos } }

component_impl: COMPONENT IDENT list(func)
    { { ci_name = $2; ci_funs = $3; ci_loc = mk_location $startpos $endpos } }

func:
| FUN IDENT LPAREN vardecls RPAREN block
    { { f_name = $2; f_ins = $4; f_outs = []; f_body = $6; f_loc = mk_location $startpos $endpos } }
(* TODO version with return *)

fundecl: FUN IDENT LPAREN vardecls RPAREN COLON typ
    { { fd_name = $2;
        fd_args = $4;
        fd_ty = $7;
        fd_loc = mk_location $startpos $endpos } }

vardecls: separated_list(SEMICOLON, vardecl)
    { List.concat $1 }

vardecl: separated_nonempty_list(COMMA, IDENT) COLON typ
    { List.map (fun x -> { vd_name = x; vd_ty = $3; vd_loc = mk_location $startpos $endpos }) $1 }

exp:
| IDENT { { e_desc = Var $1; e_loc = mk_location $startpos $endpos } }
| LAST IDENT { { e_desc = Last $2; e_loc = mk_location $startpos $endpos } }
| NUMBER { { e_desc = ConstInt $1; e_loc = mk_location $startpos $endpos } }
| exp AND exp { { e_desc = BinOp (And, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp OR exp { { e_desc = BinOp (Or, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp EQ exp { { e_desc = BinOp (Eq, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp LE exp { { e_desc = BinOp (Le, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp LT exp { { e_desc = BinOp (Lt, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp GE exp { { e_desc = BinOp (Ge, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp GT exp { { e_desc = BinOp (Gt, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp PLUS exp { { e_desc = BinOp (Plus, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp MINUS exp { { e_desc = BinOp (Minus, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp STAR exp { { e_desc = BinOp (Mult, $1, $3); e_loc = mk_location $startpos $endpos }}
| exp SLASH exp { { e_desc = BinOp (Div, $1, $3); e_loc = mk_location $startpos $endpos }}
| MINUS exp { { e_desc = UnOp(Minus, $2); e_loc = mk_location $startpos $endpos } }

pat:
| LPAREN separated_list(COMMA, IDENT) RPAREN
  { $2 }

stmt:
| IDENT EQ exp
  { { st_desc = Equation ($1, $3);
      st_loc = mk_location $startpos $endpos } }
| pat EQ IDENT DOT IDENT LPAREN separated_list(COMMA, exp) RPAREN
  { { st_desc = Call ($1, $3, $5, $7);
      st_loc = mk_location $startpos $endpos } }
| IF LPAREN exp RPAREN THEN block
  { { st_desc = IfThenElse($3, $6, { blk_stmts = []; blk_loc = mk_location $startpos $endpos });
      st_loc = mk_location $startpos $endpos } }
| IF LPAREN exp RPAREN THEN block ELSE block
  { { st_desc = IfThenElse($3, $6, $8);
      st_loc = mk_location $startpos $endpos } }
| FOREACH IDENT COLON IDENT block
  { { st_desc = Foreach ($2, $4, $5);
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
