(* Syntax of scene files *)
(* A scene contains a list of entities *)

open Utils
open Proglang

type scene = {
  scn_name : ident;
  scn_init_entities : (ident * constant list) list;
  scn_spawn_entities : ident list;
}
