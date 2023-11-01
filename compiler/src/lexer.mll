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
  | "component"      { COMPONENT }
  | "else"           { ELSE }
  | "entity"         { ENTITY }
  | "foreach"        { FOREACH }
  | "fun"            { FUN }
  | "if"             { IF }
  | "int"            { INT }
  | "last"           { LAST }
  | "periodic"       { PERIODIC }
  | "then"           { THEN }
  | "trigger"        { TRIGGER }
  | "unit"           { UNIT }
  | "system"         { SYSTEM }
  | "var"            { VAR }
  | eof              { EOF }
  | ['0'-'9']+
  | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
  | '0' ['o' 'O'] ['0'-'7']+
  | '0' ['b' 'B'] ['0'-'1']+
      { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | (['a'-'z']('_' ? ['A'-'Z' 'a'-'z' ''' '0'-'9']) * as id)
    { IDENT id }
  | _
  { raise (Lexical_error (Illegal_character,
             { l_start = Lexing.lexeme_start_p lexbuf;
               l_end   = Lexing.lexeme_end_p lexbuf })) }
