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

type exp = {
  e_desc : exp_desc;
  e_loc  : location;
}
and exp_desc =
  | ConstInt of int
  | Var of ident
[@@deriving show]

type stmt = {
  st_desc : stmt_desc;
  st_loc  : location
}

and stmt_desc =
  | Equation of ident * exp
  | Call of ident list * ident * ident * exp list
  | Foreach of ident * ident * block
[@@deriving show]

and block = {
  blk_stmts : stmt list;
  blk_loc : location
}
[@@deriving show]

type trigger_cond =
  | Periodic of int
[@@deriving show]

type trigger = {
  tr_cond : trigger_cond;
  tr_action : block;
  tr_loc  : location
} [@@deriving show]

type fundecl = {
  fd_name : ident;
  fd_args : typ list;
  fd_ty   : typ;
  fd_loc  : location
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
  | Component of component
  | System of system
[@@deriving show]
