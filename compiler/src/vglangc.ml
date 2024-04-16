(* Drive *)
open Utils

let lexbuf_from_file file_name =
  let ic = open_in file_name in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file_name };
  ic, lexbuf

let syntax_error loc =
  Format.eprintf "%aSyntax error.@." Location.print_location loc;
  raise Errors.Error

let parse filename =
  let (_ic, lexbuf) = lexbuf_from_file filename in
  try
    Parser.file Lexer.token lexbuf
  with
  | Lexer.Lexical_error(err, l) ->
    Lexer.lexical_error err l
  | Parser.Error ->
    let l_start = Lexing.lexeme_start_p lexbuf
    and l_end   = Lexing.lexeme_end_p lexbuf in
    syntax_error { l_start; l_end }

open GoblintCil

let output = ref None

let spec = [
  ("-o", Arg.String (fun s -> output := Some s), ": set output file for c code")
]

type game = {
  mutable entities : Entity.entity IdMap.t;
  mutable scenes : Scene.scene IdMap.t;
}

let game = {
  entities = IdMap.empty;
  scenes = IdMap.empty;
}

let add_file filename =
  let add_if_nmem name v map =
    if IdMap.mem name map then failwith "Duplicated _ name"
    else IdMap.add name v map
  in
  match parse filename with
  | Entity e -> game.entities <- add_if_nmem e.e_name e game.entities
  | Scene s -> game.scenes <- add_if_nmem s.scn_name s game.scenes
  | _ -> failwith "TODO add_file"

let compile_for_ds game =
  let scenes = List.concat_map (fun (_, sc) -> DsGenerator.scene game.entities sc) (idmap_to_list game.scenes) in
  let entities = List.concat_map (fun (_, e) -> DsGenerator.entity e) (idmap_to_list game.entities) in
  scenes@entities

let _ =
  Arg.parse spec add_file ("usage: "^Sys.argv.(0)^" [-o outname] <files>");
  let files = compile_for_ds game in
  List.iter (fun f ->
      let oc = open_out f.fileName in
      Cil.dumpFile Cil.defaultCilPrinter oc "meh" f;
      close_out oc) files
(* let decls = List.concat_map Generator.from_file (List.rev !files) in *)
  (* let cfile = Generator.mk_file "game" [] in *)
  (* match !output with *)
  (* | None -> Cil.dumpFile Cil.defaultCilPrinter stdout "meh" cfile *)
  (* | Some filename -> *)
  (*   let chan = open_out filename in *)
  (*   Cil.dumpFile Cil.defaultCilPrinter chan "" cfile; *)
  (*   close_out chan *)
