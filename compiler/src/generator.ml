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
  | Bool -> TInt (IUChar, [])
  | Unit -> TVoid []

let from_vardecls vds =
  List.map (fun vd -> (vd.vd_name, from_typ vd.vd_ty, None, [], locUnknown)) vds

let params_from_vardecls vds =
  List.map (fun vd -> (vd.vd_name, from_typ vd.vd_ty, [])) vds

(* Translate an expression *)

let from_unop (op: Langsyntax.unop) = match op with
  | Minus -> Neg
  | Not -> LNot

let from_binop (op: Langsyntax.binop) = match op with
  | Plus -> PlusA
  | Minus -> MinusA
  | Mult -> Mult
  | Div -> Div
  | And -> LAnd
  | Or -> LOr
  | Eq -> Eq
  | Le -> Le
  | Lt -> Lt
  | Ge -> Ge
  | Gt -> Gt

let from_constant (c: Langsyntax.constant) =
  match c with
  | CInt i -> CInt (Z.of_int i, IInt, None)
  | CFloat f -> CReal (f, FFloat, None)
  | CBool b -> CInt ((if b then Z.one else Z.zero), IUChar, None)

let rec from_exp (e: Langsyntax.exp) =
  match e.e_desc with
  | Constant c -> Const (from_constant c)
  | Var x -> Lval (Var (Cil.makeGlobalVar x (TVoid [])), NoOffset)
  | Last x -> Lval (Var (Cil.makeGlobalVar x (TVoid [])), NoOffset)
  | BinOp (op, e1, e2) -> BinOp (from_binop op, from_exp e1, from_exp e2, TVoid [])
  | UnOp (op, e1) -> UnOp (from_unop op, from_exp e1, TVoid [])

(* Translate a statement *)

let rec from_stmt (s: Langsyntax.stmt) =
  match s.st_desc with
  | Equation (x, e) ->
    let x = Cil.makeGlobalVar x (TVoid []) (* TODO *) in
    [mkStmtOneInstr (Set ((Var x, NoOffset), from_exp e, locUnknown, locUnknown))]
  | Call ([], x, f, es) ->
    let x = Cil.makeGlobalVar x (TVoid []) (* TODO *) in
    let fty = TVoid [] in (* TODO *)
    let dataty = TVoid [] in (* TODO *)
    [mkStmt (Instr [Call (None,
                          Lval (mk_field_ptr_access x f fty),
                          (Lval (mk_field_ptr_access x "$entity" dataty))::(List.map from_exp es),
                          locUnknown, locUnknown)])]
  | IfThenElse (e, blkt, blke) ->
    [mkStmt (If (from_exp e, from_block blkt, from_block blke, locUnknown, locUnknown))]
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

(* Translate an entity *)

let from_component_impl e _sty (c: component_impl) =
  List.map (fun f ->
      GFun (mk_function (e.e_name^"$"^c.ci_name^"$"^f.f_name)
              (params_from_vardecls f.f_ins)
              (TVoid []) (* TODO return multiple values? *)
              (from_block f.f_body) (* TODO return *),
            locUnknown)) c.ci_funs

let from_entity (e: entity) =
  let params = from_vardecls e.e_params in
  let sty = Cil.mkCompInfo true e.e_name
               (fun _ -> params
                  @List.map (fun vd -> (vd.vd_name, from_typ vd.vd_ty, None, [], locUnknown)) e.e_vars)
               [] in
  let initv = Cil.makeGlobalVar "$init" (TPtr (TComp (sty, []), [])) in
  [ (* Structure *)
    GCompTag (sty, locUnknown);
    (* Constructor *)
    GFun (mk_function (e.e_name^"$init")
            (List.map (fun (x, ty, _, _, _) -> (x, ty, [])) params)
            (TPtr (TComp (sty, []), []))
            (mkBlock [mkStmt (Instr ([VarDecl (initv, locUnknown);
                                      Call (Some (Var initv, NoOffset),
                                            Lval ((Var (Cil.makeGlobalVar "malloc" (TVoid []))), NoOffset),
                                            [SizeOf (TComp (sty, []))],
                                            locUnknown, locUnknown)]@
                                     (List.map (fun (x, ty, _, _, _) ->
                                          Set (mk_field_ptr_access initv x ty,
                                               Lval (Var (Cil.makeGlobalVar x ty), NoOffset),
                                               locUnknown, locUnknown)) params)@
                                    (List.map (fun (x, e) -> Set (mk_field_ptr_access initv x (TVoid []),
                                                                  from_exp e,
                                                                  locUnknown, locUnknown)) e.e_lasts)));
                      mkStmt (Return (Some (Lval (Var initv, NoOffset)), locUnknown))
                     ]),
          locUnknown)
  ]@(List.concat_map (from_component_impl e sty) e.e_comps)

(* Translate a component *)

let from_component_fun (f: fundecl) =
  (f.fd_name,
   TPtr (TFun (from_typ f.fd_ty,
               Some (("$entity", TPtr (TVoid [], []), [])::
                     List.mapi (fun i { vd_ty; _ } -> (Printf.sprintf "d$%d" i, from_typ vd_ty, [])) f.fd_args),
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

let init_system_name s = s.s_name^"$init"
let update_system_name s = s.s_name^"$update"

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
  | Entity e -> from_entity e
  | Component c -> from_component c
  | System s -> from_system s
