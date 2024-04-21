(* Generating C code for the Nintendo DS *)

(* There are two concurrent scenes : one for each screen *)

open Utils
open Entity
open Scene
open Clang
open Generator

(* Helpers *)

let ty_u8 = Tident "u8"
let ty_u16 = Tident "u16"
let ty_oamstate = Tident "OamState"
let ty_entity = Tident "entity_t"
let ty_entity_list = Tident "entity_list_t"

(** Generate a call to oamAllocateGfx for entity [ent], and place result in [v] *)
let mk_oam_allocate_gfx x i oam (ent: entity) =
  Call (Some (Index (x, i)), "oamAllocateGfx",
        [Ident oam;
         Ident (Printf.sprintf "SpriteSize_%dx%d" ent.e_gfx.gfx_tile_width ent.e_gfx.gfx_tile_height);
         Ident "SpriteColorFormat_256Color"])

(** Generate a call to dmaCopy *)
let mk_dma_cpy src dest len =
  Call (None, "dmaCopy", [src; dest; len])

let mk_dma_cpy_palette ent =
  mk_dma_cpy
    (Ident (ent.e_gfx.gfx_tileset^"Pal"))
    (Index (Index (Ident "VRAM_G_EXT_PALETTE", Const (Int 0)), Ident "paletteId"))
    (Ident (ent.e_gfx.gfx_tileset^"PalLen"))

(* Translating an entity *)

let entity_init_graphics_fun ent = {
  fun_name = (Printf.sprintf "%s_init_gfx" ent.e_name);
  fun_args = [("oam", Tptr ty_oamstate);
              ("ent", Tptr ty_entity);
              ("paletteId", ty_u8)];
  fun_ret = Tvoid;
  fun_body = None;
}

(** Initialize graphics for an entity [ent] *)
let entity_init_graphics (ent: entity) =
  let nbTiles = nb_tiles ent.e_gfx in
  (* TODO with a loop? *)
  let allocates = List.init nbTiles (fun i -> mk_oam_allocate_gfx "ent->gfx" i "oam" ent) in
  let cpyPalette = mk_dma_cpy_palette ent in
  { (entity_init_graphics_fun ent) with fun_body = Some (allocates@[cpyPalette]) }

(** Call the runtime function [entity_draw_gfx] for an entity [ent] *)
let entity_draw_graphics (_ent: entity) =
  Call (None, "entity_draw_gfx", [])
  (* { fun_name = (Printf.sprintf "%s_draw_gfx" ent.e_name); *)
  (*   fun_args entity_draw_graphics_params; *)
  (*   fun_ret = Tvoid; *)
  (*   fun_body = Some [call] } *)

(** Header for an entity [ent] *)
let entity_header (ent: entity) =
  let includes = [mk_include "runtime.h"] in
  let init_gfx = entity_init_graphics_fun ent in
  { file_name = ent.e_name ^ ".h";
    globals = includes@[Fun init_gfx] }

(** Source for an entity [ent] *)
let entity_source (ent: entity) =
  let includes = [mk_include "nds.h";
                  mk_include "runtime.h";
                  mk_include (ent.e_name ^ ".h");
                  mk_include ((Filename.remove_extension ent.e_gfx.gfx_tileset) ^ ".h")] in
  let funs = [entity_init_graphics ent] in
  { file_name = ent.e_name ^ ".c";
    globals = includes@(List.map (fun fd -> Fun fd) funs) }

let entity ent = [entity_header ent; entity_source ent]

(* Translating a scene *)

(** Generate a limited number of ids *)
type 'a generator = {
  gen_limit  : int;
  gen_next   : int;
  gen_ids    : int IdMap.t;
  gen_errmsg : string;
}

(** Initialize a generator *)
let init_generator gen_limit gen_errmsg =
  { gen_limit; gen_next = 0; gen_ids = IdMap.empty; gen_errmsg }

(** Generate a new id, if possible *)
let generate v = fun st ->
  if st.gen_next < st.gen_limit then
    st.gen_next,
    { st with
      gen_next = st.gen_next + 1;
      gen_ids = IdMap.add v st.gen_next st.gen_ids }
  else failwith st.gen_errmsg

(** Find the id associated with [name], or generate it if necessary *)
let find_or_generate name = fun st ->
  match IdMap.find_opt name st.gen_ids with
  | Some id -> id, st
  | None -> generate name st

(** State for collecting and generating scene info *)
type scene_st = {
  (* gfx_entries : string generator; *)
  palettes : string generator;
}

(** Initialize the scene generation state *)
let init_scene_st scn_name = {
  (* gfx_entries = init_generator 128 (Printf.sprintf "Too many entries in scene '%s'" scn_name); *)
  palettes = init_generator 16 (Printf.sprintf "Too many palettes in scene '%s'" scn_name);
}

(* (\** Add or find a gfx entry *\) *)
(* let gfx_entry name n = fun st -> *)
(*   let name = Printf.sprintf "ENTRY_%s_%d" (String.uppercase_ascii name) n in *)
(*   let (id, gfx_entries) = find_or_generate name st.gfx_entries in *)
(*   id, { st with gfx_entries } *)

let palette_name ent_name =
  Printf.sprintf "PALETTE_%s" (String.uppercase_ascii ent_name)

(** Add or find a gfx entry *)
let palette name = fun st ->
  let (id, palettes) = find_or_generate (palette_name name) st.palettes in
  id, { palettes }

(** Get gfx and palettes for an entity *)
let collect_entity_gfx entity =
  (* let name = entity.e_name in *)
  (* gfx *)
  (* let rec loop n = *)
  (*   if n = 0 then ret () *)
  (*   else let* _ = gfx_entry name n in loop (n - 1) *)
  (* in *)
  (* let* _ = loop (entity.e_gfx.gfx_width * entity.e_gfx.gfx_height) in *)
  palette entity.e_name

(** Get gfx and palettes for a scene *)
let collect_scene_gfx entities scn =
  let find = find_with_msg "entity" entities in
  let* _ = omap (fun (x, _) -> collect_entity_gfx (find x)) scn.scn_init_entities in
  let* _ = omap (fun x -> collect_entity_gfx (find x)) scn.scn_spawn_entities in
  ret ()

let scene_init_fun scn = {
  fun_name = scn.scn_name^"_init";
  fun_args = [("oam", Tptr ty_oamstate)];
  fun_ret = Tvoid;
  fun_body = None;
}

let scene_draw_fun scn = {
  fun_name = scn.scn_name^"_draw";
  fun_args = [("oam", Tptr ty_oamstate)];
  fun_ret = Tvoid;
  fun_body = None;
}

(** Generate header for a scene *)
let scene_header entities (scn : scene) =
  let ent_names = List.map fst scn.scn_init_entities@scn.scn_spawn_entities in
  let includes = (mk_include "nds.h")::(List.map (fun name -> mk_include (name^".h")) ent_names) in
  let* () = collect_scene_gfx entities scn in
  fun st ->
    let mk_defines gen =
      List.map (fun (s, i) -> mk_define s (Int i)) (idmap_to_list gen.gen_ids)
    in
    { file_name = scn.scn_name ^ ".h";
      globals = includes@(mk_defines st.palettes)
                @[Fun (scene_init_fun scn); Fun (scene_draw_fun scn)] },
    st

(** Initialize a scene [scn] *)
let scene_init entities scn =
  let body = List.concat_map (fun (name, _params) ->
      let ent = find_with_msg "entity" entities name in
      let init_ent = Call (Some (DeclIdent (Tptr ty_entity, "ent")),
                           "entity_create",
                           [Ref (Ident (Printf.sprintf "%s_entries" scn.scn_name));
                            Const (Int (nb_tiles ent.e_gfx));
                            Const (Int 0); Const (Int 0)]) in (* TODO *)
      let add_ent = Call (None, "add_entity",
                          [Ident "ent"; Ref (Ident (Printf.sprintf "%s_entities" scn.scn_name))]) in
      let init_gfx = Call (None, Printf.sprintf "%s_init_gfx" name,
                           [Ident "oam"; Ident "ent"; Ident (palette_name name)]) in
      [init_ent; add_ent; init_gfx]
    ) scn.scn_init_entities in
  ret { (scene_init_fun scn) with fun_body = Some body }

(** Draw a scene [scn] *)
let scene_draw _entities scn =
  let body = [] in
  ret { (scene_draw_fun scn) with fun_body = Some body }

(** Generate the source for a scene [scn] *)
let scene_source entities scn =
  let includes = [mk_include (scn.scn_name ^ ".h")] in
  let entriesDecl = Var (Tint, Printf.sprintf "%s_entries" scn.scn_name, Some (Int 0)) in
  let entitiesDecl = Var (Tptr ty_entity_list, Printf.sprintf "%s_entities" scn.scn_name, None) in
  let* init = scene_init entities scn in
  let* draw = scene_draw entities scn in
  ret
    { file_name = scn.scn_name ^ ".c";
      globals = includes@[entriesDecl; entitiesDecl; Fun init; Fun draw] }

(** Generate all code for a scene [scn] *)
let scene entities (scn : scene) =
  let (header, source) = run
      (let* header = scene_header entities scn in
       let* source = scene_source entities scn in
       ret (header, source))
      (init_scene_st scn.scn_name) in
  [header;source]
