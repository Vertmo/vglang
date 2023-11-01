(** Syntax of the source language *)
(** There are three types of compilation units: *)
(** - components *)
(** - entities *)
(** - systems *)

open Location

type ident = string
[@@deriving show]

type typ =
  | Unit
  | Int
[@@deriving show]

type unop =
  | Minus
  | Not
[@@deriving show]

type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | And
  | Or
  | Eq
  | Le
  | Lt
  | Ge
  | Gt
[@@deriving show]

type exp = {
  e_desc : exp_desc;
  e_loc  : location;
} [@@deriving show]

and exp_desc =
  | ConstInt of int
  | Var of ident
  | Last of ident
  | UnOp of unop * exp
  | BinOp of binop * exp * exp

type stmt = {
  st_desc : stmt_desc;
  st_loc  : location
} 

and stmt_desc =
  | Equation of ident * exp
  | Call of ident list * ident * ident * exp list
  | IfThenElse of exp * block * block
  | Foreach of ident * ident * block

and block = {
  blk_stmts : stmt list;
  blk_loc : location
} [@@deriving show]

type trigger_cond =
  | Periodic of int
[@@deriving show]

type trigger = {
  tr_cond : trigger_cond;
  tr_action : block;
  tr_loc  : location
} [@@deriving show]

type vardecl = {
  vd_name : ident;
  vd_ty   : typ;
  vd_loc  : location
} [@@deriving show]

type fundecl = {
  fd_name : ident;
  fd_args : vardecl list;
  fd_ty   : typ;
  fd_loc  : location
} [@@deriving show]

type func = {
  f_name : ident;
  f_ins  : vardecl list;
  f_outs : vardecl list;
  f_body : block;
  f_loc  : location
} [@@deriving show]

type component_impl = {
  ci_name : ident;
  ci_funs : func list;
  ci_loc  : location
} [@@deriving show]

type entity = {
  e_name   : ident;
  e_params : vardecl list;
  e_vars   : vardecl list;
  e_comps  : component_impl list;
  e_loc    : location
} [@@deriving show]

type component = {
  c_name : ident;
  c_funs : fundecl list;
  c_loc  : location
} [@@deriving show]

type system = {
  s_name : ident;
  s_triggers : trigger list;
  s_loc  : location
} [@@deriving show]

(** Compilation unit *)
type file =
  | Entity of entity
  | Component of component
  | System of system
[@@deriving show]
