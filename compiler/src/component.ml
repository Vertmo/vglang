(* Definition of a component *)

open Utils
open Location
open Proglang

type component = {
  c_name : ident;
  c_funs : fundecl list;
  c_loc  : location
} [@@deriving show]
