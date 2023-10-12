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
    let prog = Parser.file Lexer.token lexbuf in
    print_endline (Langsyntax.show_file prog)
  with
  | Lexer.Lexical_error(err, l) ->
    Lexer.lexical_error err l
  | Parser.Error ->
    let l_start = Lexing.lexeme_start_p lexbuf
    and l_end   = Lexing.lexeme_end_p lexbuf in
    syntax_error { l_start; l_end }


let main filename =
  parse filename

let _ =
  Arg.parse [] main "usage: ./vglanc <files>"
