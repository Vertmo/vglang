(* Definition of an entity *)

open Utils
open Location
open Proglang

type component_impl = {
  ci_name : ident;
  ci_funs : func list;
  ci_loc  : location
} [@@deriving show]

(* Graphics *)

type gfx = {
  gfx_tileset : filename; (* source tileset (also indicates the palette) *)
  gfx_tile_width : int;
  gfx_tile_height : int;
  gfx_width : int;
  gfx_height : int;
  gfx_init_offset : int;
} [@@deriving show]

(** Number of tiles used by graphics [gfx] *)
let nb_tiles gfx = gfx.gfx_width * gfx.gfx_height

type behavior = {
  e_vars   : vardecl list;
  e_lasts  : (ident * exp) list;
  e_comps  : component_impl list;
} [@@deriving show]

type entity = {
  e_name   : ident;
  e_params : vardecl list;
  e_gfx    : gfx;
  e_behav  : behavior;
  e_loc    : location;
} [@@deriving show]
