(** Definition of a system *)

open Utils
open Location
open Proglang

type system = {
  s_name : ident;
  s_triggers : trigger list;
  s_loc  : location
} [@@deriving show]
