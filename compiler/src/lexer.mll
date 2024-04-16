{
open Lexing
open Location
open Parser

type lexical_error =
  | Illegal_character
  | Unterminated_comment
  | Bad_char_constant
  | Unterminated_string

let lexical_error err loc =
  Format.eprintf (match err with
    | Illegal_character -> Stdlib.format_of_string "%aIllegal character.@."
    | Unterminated_comment -> "%aUnterminated comment.@."
    | Bad_char_constant -> "%aBad char constant.@."
    | Unterminated_string -> "%aUnterminated string.@."
     ) print_location loc;
  raise Errors.Error

exception Lexical_error of lexical_error * location;;

(* To buffer string literals *)

let string_buffer = Buffer.create 256

let reset_string_buffer () =
  Buffer.reset string_buffer

let store_string_char c =
  Buffer.add_char string_buffer c

let get_stored_string () =
  Buffer.contents string_buffer

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c =
    100 * (int_of_char(Lexing.lexeme_char lexbuf i) - 48) +
     10 * (int_of_char(Lexing.lexeme_char lexbuf (i+1)) - 48) +
          (int_of_char(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  char_of_int(c land 0xFF)
}

let newline = '\n' | '\r' '\n'

rule token = parse
  | newline          { new_line lexbuf; token lexbuf }
  | [' ' '\t'] +     { token lexbuf }
  | "("              { LPAREN }
  | ")"              { RPAREN }
  | "{"              { LCURLY }
  | "}"              { RCURLY }
  | ":"              { COLON }
  | ";"              { SEMICOLON }
  | ","              { COMMA }
  | "."              { DOT }
  | "="              { EQ }
  | "+"              { PLUS }
  | "-"              { MINUS }
  | "*"              { STAR }
  | "/"              { SLASH }
  | "and"            { AND }
  | "or"             { OR }
  | ">="             { GE }
  | ">"              { GT }
  | "<="             { LE }
  | "<"              { LT }
  | "behavior"       { BEHAVIOR }
  | "bool"           { BOOL }
  | "component"      { COMPONENT }
  | "else"           { ELSE }
  | "entity"         { ENTITY }
  | "entities"       { ENTITIES }
  | "foreach"        { FOREACH }
  | "fun"            { FUN }
  | "graphics"       { GRAPHICS }
  | "height"         { HEIGHT }
  | "if"             { IF }
  | "initial"        { INITIAL }
  | "int"            { INT }
  | "last"           { LAST }
  | "offset"         { OFFSET }
  | "periodic"       { PERIODIC }
  | "scene"          { SCENE }
  | "spawn"          { SPAWN }
  | "system"         { SYSTEM }
  | "then"           { THEN }
  | "tileset"        { TILESET }
  | "trigger"        { TRIGGER }
  | "unit"           { UNIT }
  | "var"            { VAR }
  | "width"          { WIDTH }
  | eof              { EOF }
  | ['0'-'9']+
  | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
  | '0' ['o' 'O'] ['0'-'7']+
  | '0' ['b' 'B'] ['0'-'1']+
      { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | (['a'-'z']('_' ? ['A'-'Z' 'a'-'z' ''' '0'-'9']) * as id)
    { IDENT id }
  | (['A'-'Z']('_' ? ['A'-'Z']) * as id)
    { BIGIDENT id }
  | "\""
      { reset_string_buffer();
        string lexbuf;
        STRING (get_stored_string()) }
  | _
  { raise (Lexical_error (Illegal_character,
             { l_start = Lexing.lexeme_start_p lexbuf;
               l_end   = Lexing.lexeme_end_p lexbuf })) }

and string = parse
  | newline         { new_line lexbuf; string lexbuf }
  | '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"'  'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Lexical_error(Unterminated_string,
                { l_start = dummy_pos;
                  l_end   = Lexing.lexeme_start_p lexbuf } )) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
