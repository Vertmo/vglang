(** Generate C code *)
(** V0: directly from the syntax *)

open Langsyntax
open GoblintCil

(* Helpers *)

let mk_field_info fname ftype =
  { fcomp = Cil.mkCompInfo false "dummy" (fun _ -> []) [];
    fname;
    ftype;
    fbitfield = None;
    fattr = [];
    floc = locUnknown }

let mk_field_ptr_access x field ty =
  mkMem
    ~addr:(Lval ((Var x), NoOffset))
    ~off:(Field (mk_field_info field ty, NoOffset))

let mk_struct_typ name =
  TComp (Cil.mkCompInfo true name (fun _ -> []) [], [])

let mk_fun_typ args retty =
 TFun (retty, Some args, false, [])

let mk_function name args retty body =
  let typ = mk_fun_typ args retty in
  { svar = Cil.makeGlobalVar name typ;
    sformals = [];
    slocals = [];
    smaxid = 0;
    sbody = body;
    smaxstmtid = None;
    sallstmts = [];
  }

let mk_file name globals =
  Cil.({
      fileName = name ^ ".c";
      globals;
      globinit = None;
      globinitcalled = false;
    })

let tuint = TInt (IUInt, [])
let enull = Const (CInt (Z.zero, IUInt, None))

(* Translate a type *)

let from_typ = function
  | Int -> TInt (IInt, [])
  | Unit -> TVoid []

(* Translate an expression *)

let from_exp (e: Langsyntax.exp) =
  match e.e_desc with
  | ConstInt i -> Const (CInt (Z.of_int i, IInt, None))
  | Var x -> Lval (Var (Cil.makeGlobalVar x (TVoid [])), NoOffset)

(* Translate a statement *)

let rec from_stmt (s: Langsyntax.stmt) =
  match s.st_desc with
  | Call ([], x, f, es) ->
    let x = Cil.makeGlobalVar x (TVoid []) (* TODO *) in
    let fty = TVoid [] in (* TODO *)
    let dataty = TVoid [] in (* TODO *)
    [mkStmt (Instr [Call (None,
                          Lval (mk_field_ptr_access x f fty),
                          (Lval (mk_field_ptr_access x "$entity" dataty))::(List.map from_exp es),
                          locUnknown, locUnknown)])]
  | Equation _ -> failwith "TODO from_stmt Equation"
  | Foreach (x, cclass, body) ->
    let iterty = TPtr (mk_struct_typ cclass, []) (* TODO *) in
    let clist = Cil.makeGlobalVar ("component$"^cclass^"$list") iterty in
    let x = Cil.makeGlobalVar x iterty in
    mkFor
      ~start:[mkStmtOneInstr (VarDecl (x, locUnknown));
              mkStmtOneInstr (Set ((Var x, NoOffset),
                                   (Lval (Var clist, NoOffset)),
                                   locUnknown, locUnknown))]
      ~guard:(Lval (Var x, NoOffset))
      ~next:[mkStmtOneInstr
               (Set ((Var x, NoOffset),
                     Lval (mk_field_ptr_access x "$next" iterty),
                     locUnknown, locUnknown))]
      ~body:(from_stmts body.blk_stmts)
  | _ -> failwith "TODO from_stmt"

and from_stmts stmts = List.concat_map from_stmt stmts

and from_block (b: Langsyntax.block) =
  mkBlock (from_stmts b.blk_stmts)

(* Translate a trigger *)

let mk_modulo_test var m =
  BinOp (Eq,
         BinOp(Mod,
               Lval (Var (Cil.makeGlobalVar var tuint), NoOffset),
               Const (CInt (Z.of_int m, IUInt, None)),
               tuint),
         Const (CInt (Z.zero, IUInt, None)),
         TInt (IBool, []))

let from_trigger (t: trigger) =
  match t.tr_cond with
  | Periodic i ->
    mkStmt (If (mk_modulo_test "$tick" i,
                from_block t.tr_action,
                mkBlock [], locUnknown, locUnknown))

(* Translate a component *)

let from_component_fun (f: fundecl) =
  (f.fd_name,
   TPtr (TFun (from_typ f.fd_ty,
               Some (("$entity", TPtr (TVoid [], []), [])::
                     List.mapi (fun i ty -> (Printf.sprintf "d$%d" i, from_typ ty, [])) f.fd_args),
               false, []), []),
   None, [], locUnknown)

let from_component (c: component) =
  let sty = TPtr (mk_struct_typ c.c_name, []) in
  [GCompTag (Cil.mkCompInfo true c.c_name
             (fun _ -> [("$entity", TPtr (TVoid [], []), None, [], locUnknown);
                        ("$next", sty, None, [], locUnknown)]@
                       (List.map from_component_fun c.c_funs))
             [],
             locUnknown);
  GVar (Cil.makeGlobalVar ("component$"^c.c_name^"$list") sty, { init = None }, locUnknown)]

(* Translate a system *)

let init_system_name s = "system$"^s.s_name^"$init"
let update_system_name s = "system$"^s.s_name^"$update"

let init_from_system (s: system) =
  mk_function (init_system_name s) [] (TVoid []) (mkBlock [])

let update_from_system (s: system) =
  let args = [("$tick", TInt (IUInt, []), [])] in
  mk_function (update_system_name s) args (TVoid [])
    (mkBlock (List.map from_trigger s.s_triggers))

let from_system (s: system) =
  [ GFun (init_from_system s, locUnknown);
    GFun (update_from_system s, locUnknown) ]

let from_file file =
  match file with
  | Component c -> from_component c
  | System s -> from_system s
