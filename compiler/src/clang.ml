(** Representation of a subset of the C language, used for code generation *)

open Utils

type ty =
  | Tvoid
  | Tint | Tfloat
  | Tident of ident
  | Tenum of ident
  | Tptr of ty

let rec string_of_ty = function
  | Tvoid -> "void"
  | Tint -> "int" | Tfloat -> "float"
  | Tident id -> id
  | Tenum id -> Printf.sprintf "enum %s" id
  | Tptr ty -> Printf.sprintf "%s*" (string_of_ty ty)

type const =
  | Int of int | Float of float
  | EnumField of string

let string_of_const = function
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | EnumField s -> s

type structdef = {
  struct_name : ident;
  struct_fields : (ident * ty) list;
}

let string_of_structdef (stdef : structdef) =
  Printf.sprintf "typedef struct {\n\
                 \t%s\n\
                 } %s;\n"
    (String.concat "\n\t"
       (List.map (fun (id, ty) -> (string_of_ty ty)^" "^id^";")
          stdef.struct_fields))
    stdef.struct_name

type lhs =
  | Ident of ident
  | PField of ident * ident
  | Index of ident * int

let string_of_lhs = function
  | Ident id -> id
  | PField (st, id) -> Printf.sprintf "%s->%s" st id
  | Index (id, i) -> Printf.sprintf "%s[%d]" id i

type op =
  | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod
  | Op_not
  | Op_and | Op_or | Op_xor

let string_of_op = function
  | Op_eq -> "==" | Op_neq -> "!=" | Op_lt -> "<" | Op_le -> "<="
  | Op_gt -> ">" | Op_ge -> ">="
  | Op_add -> "+" | Op_sub -> "-" | Op_mul -> "*" | Op_div -> "/" | Op_mod -> "%"
  | Op_not -> "~" | Op_and -> "&" | Op_or -> "|" | Op_xor -> "^"

type expr =
  | Const of const
  | Ident of ident
  | Ref of expr
  | Field of expr * ident
  | PField of ident * ident
  | Index of expr * expr
  | BinOp of expr * op * expr
  | UnOp of op * expr

let rec string_of_expr = function
  | Const c -> string_of_const c
  | Ident id -> id
  | Ref e -> "&"^(string_of_expr e)
  | Field (e, field) ->
    Printf.sprintf "%s.%s" (string_of_expr e) field
  | PField (id, field) -> id^"->"^field
  | Index (e, i) ->
    Printf.sprintf "%s[%s]" (string_of_expr e) (string_of_expr i)
  | BinOp (e1, op, e2) ->
    Printf.sprintf "(%s %s %s)" (string_of_expr e1)
      (string_of_op op) (string_of_expr e2)
  | UnOp (op, e) ->
    Printf.sprintf "(%s %s)" (string_of_op op) (string_of_expr e)

type instr =
  | Assign of lhs * expr
  | If of expr * instr list * instr list
  | Call of lhs option * ident * expr list
  | VarDec of ty * ident
  | SwitchCase of (expr * (ident * instr list) list)
  (* Only used for AVR main loop, dont worry ! *)
  | While of (expr * instr list)

let rec string_of_instr (level : int) (i : instr) =
  let indent = String.make (level*2) ' ' in
  match i with
  | Assign (l, e) ->
    Printf.sprintf "%s%s = %s;" indent (string_of_lhs l) (string_of_expr e)
  | If (c, t, e) ->
    Printf.sprintf "%sif (%s) {\n\
                    %s\n\
                    %s} else {\n\
                    %s\n\
                    %s}"
      indent (string_of_expr c)
      (String.concat "\n" (List.map (string_of_instr (level + 1)) t))
      indent
      (String.concat "\n" (List.map (string_of_instr (level + 1)) e))
      indent
  | Call (None, fid, es) ->
    Printf.sprintf "%s%s(%s);" indent fid
      (String.concat "," (List.map string_of_expr es))
  | Call (Some lhs, fid, es) ->
    Printf.sprintf "%s%s = %s(%s);" indent
      (string_of_lhs lhs) fid
      (String.concat "," (List.map string_of_expr es))
  | VarDec (ty, id) ->
    Printf.sprintf "%s%s %s;" indent (string_of_ty ty) id
  | SwitchCase (e, cases) ->
    Printf.sprintf "%sswitch(%s) {\n%s\n%s}\n" indent (string_of_expr e) indent
      (String.concat "\n" (List.map (fun (c, instr) ->
           Printf.sprintf "%s\tcase %s:\n%s\n%s\t\tbreak;"
             indent c
             (String.concat "\n"
                (string_of_instrs (level + 2) instr))
             indent) cases))
  | While (e, instrs) ->
    Printf.sprintf "%swhile(%s) {\n%s\n%s}" indent
      (string_of_expr e)
      (String.concat "\n"
         (string_of_instrs (level + 1) instrs)) indent
and string_of_instrs level = List.map (string_of_instr level)

type fundef = {
  fun_name : ident;
  fun_ret : ty;
  fun_args : (ident * ty) list;
  fun_body : instr list option;
}

let string_of_fundef (f : fundef) =
  Printf.sprintf "%s %s(%s)%s\n"
    (string_of_ty f.fun_ret) f.fun_name
    (String.concat ", "
       (List.map (fun (id, ty) -> (string_of_ty ty)^" "^id) f.fun_args))
    (match f.fun_body with
     | None -> ";"
     | Some body ->
       Printf.sprintf "{\n%s\n}"
         (String.concat "\n" (string_of_instrs 1 body)))

type global =
  | Enum of ident * ident list
  | Struct of structdef
  | Fun of fundef
  | Include of ident
  | Define of string * const

let string_of_global = function
  | Enum (id, constrs) ->
    Printf.sprintf "enum %s {%s};"
      id (String.concat "," constrs)
  | Struct std -> string_of_structdef std
  | Fun fd -> string_of_fundef fd
  | Include s -> Printf.sprintf "#include %S\n" s
  | Define (s, c) -> Printf.sprintf "#define %s %s" s (string_of_const c)

type file = {
  file_name : filename;
  globals : global list
}

let string_of_file (f : file) =
  String.concat "\n" (List.map string_of_global f.globals)
