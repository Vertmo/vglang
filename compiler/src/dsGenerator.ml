(* Generating C code for the Nintendo DS *)

(* There are two concurrent scenes : one for each screen *)

open Utils
open Entity
open Scene
open Generator

open GoblintCil

(* Helpers *)

let ty_u8 = Cil.(TNamed ({ tname = "u8"; ttype = TVoid []; treferenced = false }, []))
let ty_u16 = Cil.(TNamed ({ tname = "u16"; ttype = TVoid []; treferenced = false }, []))

(** Generate a call to oamAllocateGfx for entity [ent], and place result in [v] *)
let mk_oam_allocate_gfx x oam i (ent: entity) =
  mk_call (Some (mk_index_access x i))
    "oamAllocateGfx"
    [mk_var oam;
     mk_var (Printf.sprintf "SpriteSize_%dx%d" ent.e_gfx.gfx_tile_width ent.e_gfx.gfx_tile_height);
     mk_var "SpriteColorFormat_256Color"]

(* Translating an entity *)

let entity_init_graphics_params =
  [("oam", TPtr (TVoid [] (* TODO *), []), []);
   ("entries", TArray (ty_u8, None, []), []);
   ("gfx", TArray (ty_u16, None, []), [])]

(** Initialize graphics for an entity [ent] *)
let entity_init_graphics (ent: entity) =
  let nbTiles = nb_tiles ent.e_gfx in
  (* TODO with a loop? *)
  let allocates = List.init nbTiles (fun i -> mk_oam_allocate_gfx "oam" "entries" i ent) in
  Cil.(mk_function
         (Printf.sprintf "%s_init_gfx" ent.e_name)
         entity_init_graphics_params
         (TVoid [])
         (mkBlock [mkStmt (Instr allocates)]))

(** Header for an entity [ent] *)
let entity_header (ent: entity) =
  let init_gfx = mk_fun_decl (Printf.sprintf "%s_init_gfx" ent.e_name) entity_init_graphics_params (TVoid []) in
  Cil.({ fileName = ent.e_name ^ ".h";
         globals = [init_gfx];
         globinit = None;
         globinitcalled = false; })

(** Source for an entity [ent] *)
let entity_source (ent: entity) =
  let includes = [mk_include "nds.h";
                  mk_include (ent.e_name ^ ".h")] in
  let funs = [entity_init_graphics ent] in
  Cil.({ fileName = ent.e_name ^ ".c";
         globals = includes@(List.map (fun fd -> GFun (fd, locUnknown)) funs);
         globinit = None;
         globinitcalled = false; })

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
  gfx_entries : string generator;
  palettes : string generator;
}

(** Initialize the scene generation state *)
let init_scene_st scn_name = {
  gfx_entries = init_generator 128 (Printf.sprintf "Too many entries in scene '%s'" scn_name);
  palettes = init_generator 16 (Printf.sprintf "Too many palettes in scene '%s'" scn_name);
}

(** Add or find a gfx entry *)
let gfx_entry name n = fun st ->
  let name = Printf.sprintf "ENTRY_%s_%d" (String.uppercase_ascii name) n in
  let (id, gfx_entries) = find_or_generate name st.gfx_entries in
  id, { st with gfx_entries }

(** Add or find a gfx entry *)
let palette name = fun st ->
  let name = Printf.sprintf "PALETTE_%s" (String.uppercase_ascii name) in
  let (id, palettes) = find_or_generate name st.palettes in
  id, { st with palettes }

(** Get gfx and palettes for an entity *)
let collect_entity_gfx entity =
  let name = entity.e_name in
  (* gfx *)
  let rec loop n =
    if n = 0 then ret ()
    else let* _ = gfx_entry name n in loop (n - 1)
  in
  let* _ = loop (entity.e_gfx.gfx_width * entity.e_gfx.gfx_height) in
  palette entity.e_name

(** Get gfx and palettes for a scene *)
let collect_scene_gfx entities scn =
  let find = find_with_msg "entity" entities in
  let* _ = omap (fun (x, _) -> collect_entity_gfx (find x)) scn.scn_init_entities in
  let* _ = omap (fun x -> collect_entity_gfx (find x)) scn.scn_spawn_entities in
  ret ()

(** Generate header for a scene *)
let scene_header entities (scn : scene) =
  let* () = collect_scene_gfx entities scn in
  fun st ->
    let mk_defines gen =
      List.map (fun (s, i) -> mk_define s i) (idmap_to_list gen.gen_ids)
    in
    Cil.({
        fileName = scn.scn_name ^ ".h";
        globals = (mk_defines st.gfx_entries)@(mk_defines st.palettes);
        globinit = None;
        globinitcalled = false;
      }),
    st

(** Initialize graphics for a scene [scn] *)
let scene_init_graphics _scn = ret () (* TODO *)

(** Generate the source for a scene [scn] *)
let scene_source scn =
  let includes = [mk_include (scn.scn_name ^ ".h")] in
  ret
    Cil.({
      fileName = scn.scn_name ^ ".c";
      globals = includes;
      globinit = None;
      globinitcalled = false;
    })

(** Generate all code for a scene [scn] *)
let scene entities (scn : scene) =
  let (header, source) = run
      (let* header = scene_header entities scn in
       let* source = scene_source scn in
       ret (header, source))
      (init_scene_st scn.scn_name) in
  [header;source]
