



IFDEF BUILD_TUT15 THEN




open Cil
open Pretty
open Tututil

module E = Errormsg
module L = List
module H = Hashtbl

let autotest_str = "autotest"
let isAutotestType   (t : typ) : bool = hasAttribute "autotest"   (typeAttrs t) 
let isInstrumentType (t : typ) : bool = hasAttribute "instrument" (typeAttrs t)

let input_str    = "input"
let inputarr_str = "inputarr"
let inputnt_str  = "inputnt"

let isInputArrType (t : typ) : bool = hasAttribute "inputarr" (typeAttrs t)
let isInputNTType  (t : typ) : bool = hasAttribute "inputnt"  (typeAttrs t)

let isInputType (t : typ) : bool =
  let a = typeAttrs t in
  hasAttribute "input" a    ||
  hasAttribute "inputarr" a ||
  hasAttribute "inputnt" a

let getInputArrLen (t : typ) : int =
  match filterAttributes "inputarr" (typeAttrs t) with
  | [Attr("inputarr", [AInt n])] -> n
  | _ -> E.s(E.error "Malforemd inputarr attribute")

let dummyVar = makeVarinfo false "_tut_foo" voidType

let instCallHash : (string, varinfo) H.t = H.create 32
let uint64_t : typ ref = ref voidType
let finished : varinfo ref = ref dummyVar
let initCalls (f : file) =
  let focf : string -> typ -> varinfo = findOrCreateFunc f in
  uint64_t := findType f.globals "uint64_t";
  finished := findGlobalVar f.globals "autotest_finished";
  let u64v n = (n, !uint64_t, []) in
  let intv n = (n, intType,  []) in
  let strv n = (n, charPtrType, []) in
  let mkft r aa = TFun(r, Some aa, false, []) in
  let void = voidType in
  L.iter (fun (n,rt,atl) -> H.add instCallHash n (focf n (mkft rt atl))) [
("assign",    void, [u64v "lhs"; u64v "op"; intv "opk"; u64v "opv";]);
("assgn_bop", void, [u64v "lhs"; u64v "lhsv"; intv "bop";
                     u64v "op1"; intv "op1k"; u64v "op1v";
                     u64v "op2"; intv "op2k"; u64v "op2v";]);
("assgn_uop", void, [u64v "lhs"; u64v "lhsv"; intv "uop";
                     u64v "op"; intv "opk"; u64v "opv";]);
("cond",      void, [intv "cid"; intv "r";
                     u64v "op"; intv "opk"; u64v "opv";]);
("cond_bop",  void, [intv "cid"; intv "bop"; intv "r";
                     u64v "op1"; intv "op1k"; u64v "op1v";
                     u64v "op2"; intv "op2k"; u64v "op2v";]);
("cond_uop",  void, [intv "cid"; intv "uop"; intv "r";
                     u64v "op"; intv "opk"; u64v "opv";]);
("register_input",     void, [strv "name"; u64v "addr";intv "bits"]);
("register_arr_input", void, [strv "name"; u64v "start"; intv "sz" ;intv "cnt"]);
("register_nt_input",  void, [strv "name"; strv "start";]);
("gen_new_input",      void, []);
("push_val",           void, [u64v "v"]);
("pop_val",            !uint64_t, [strv "name"]);
("pop_array",          void, [strv "name"; strv "base"; intv "cnt"; intv "sz";]);
("pop_nt",             void, [strv "name"; strv "base";]);
("return_push",        void, [u64v "p";u64v "v"]);
("return_pop",         void, [u64v "p";u64v "v"]);
("autotest_reset",     void, []);]

let getIcall (n : string) : exp = n |> H.find instCallHash |> v2e

let int_of_bop (b : binop) : int =
  match b with
  | PlusA         | PlusPI        | IndexPI -> 0
  | MinusA        | MinusPI       | MinusPP -> 1
  | Mult    -> 2  | Div     -> 3  | Mod     -> 4
  | Shiftlt -> 5  | Shiftrt -> 6  | Lt      -> 8
  | Gt      -> 9  | Le      -> 10 | Ge      -> 11
  | Eq      -> 12 | Ne      -> 13 | BAnd    -> 14
  | BXor    -> 15 | BOr     -> 16 | LAnd    -> 17
  | LOr     -> 18

let int_of_uop (u : unop) : int =
  match u with
  | Neg  -> 0 | BNot -> 1 | LNot -> 2

let rec op_of_exp (e : exp) : exp * exp =
  let cast e = CastE(!uint64_t, e) in
  match e with
  | Const _ -> cast e, integer 0
  | AddrOf(Var v, NoOffset) -> cast e, integer 0
  | StartOf(Var v, NoOffset) -> cast e, integer 0
  | Lval(Var v, off) -> cast(AddrOf(Var v, off)), integer 1
  | Lval(Mem e, NoOffset) -> cast e, integer 1
  | CastE(t, e) -> op_of_exp e
  | _ -> E.s(E.bug "Unexpected expression in binop")

let make_assign_call (lhs : lval) (e : exp) (loc : location) : instr list =
  if isScalarType (typeOfLval lhs) then
   let lhsaddr     = CastE(!uint64_t, AddrOf lhs) in
    let eop, eopk = op_of_exp e in
    let ecst = CastE(!uint64_t, e) in
    [Set(lhs, e, loc);
     Call(None, getIcall "assign", [lhsaddr; eop; eopk; ecst], loc);]
  else [Set(lhs, e, loc)]

let make_assgn_bop_call (lhs : lval) (b : binop) (rt : typ) (loc : location)
                        (e1 : exp) (e2 : exp)
                        : instr list
  =
  if isScalarType (typeOfLval lhs) then
    let lhsaddr     = CastE(!uint64_t, AddrOf lhs) in
    let bopint      = int_of_bop b in
    let e1op, e1opk = op_of_exp e1 in
    let e2op, e2opk = op_of_exp e2 in
    let e1cst       = CastE(!uint64_t, e1) in
    let e2cst       = CastE(!uint64_t, e2) in
    [Set(lhs, BinOp(b, e1, e2, rt), loc);
     Call(None, getIcall "assgn_bop", [lhsaddr;Lval lhs;integer bopint;
                                       e1op; e1opk; e1cst; e2op; e2opk; e2cst],
          loc)]
  else [Set(lhs, BinOp(b, e1, e2, rt), loc)]

let make_assgn_uop_call (lhs : lval) (u : unop) (rt : typ) (loc : location)
                        (e : exp)
                        : instr list
  =
  if isScalarType (typeOfLval lhs) then
    let lhsaddr     = CastE(!uint64_t, AddrOf lhs) in
    let uopint      = int_of_uop u in
    let eop, eopk   = op_of_exp e in
    let ecst        = CastE(!uint64_t, e) in
    [Set(lhs, UnOp(u, e, rt), loc);
     Call(None, getIcall "assgn_uop", [lhsaddr;Lval lhs;integer uopint;
                                       eop; eopk; ecst;], loc)]
  else [Set(lhs, UnOp(u, e, rt), loc)]


let make_cond_call (cid : int) (r : int) (loc : location) (e : exp) : instr =
  let eop, eopk = op_of_exp e in
  let ecst      = CastE(!uint64_t, e) in
  Call(None, getIcall "cond", [integer cid; integer r; eop; eopk; ecst;], loc)

let make_cond_bop_call (cid : int) (bop : binop) (r : int) (loc : location)
                       (e1 : exp) (e2 : exp)
                       : instr
  =
  let bopint      = int_of_bop bop in
  let e1op, e1opk = op_of_exp e1 in
  let e2op, e2opk = op_of_exp e2 in
  let e1cst       = CastE(!uint64_t, e1) in
  let e2cst       = CastE(!uint64_t, e2) in
  Call(None, getIcall "cond_bop",
       [integer cid; integer bopint; integer r;
        e1op; e1opk; e1cst; e2op; e2opk; e2cst], loc)

let make_cond_uop_call (cid : int) (uop : unop) (r : int) (loc : location)
                       (e : exp)
                       : instr
  =
  let uopint    = int_of_uop uop in
  let eop, eopk = op_of_exp e in
  let ecst      = CastE(!uint64_t, e) in
  Call(None, getIcall "cond_uop",
       [integer cid; integer uopint; integer r; eop; eopk; ecst;], loc)

let make_input_gen_call (loc : location) : instr =
  Call(None, getIcall "gen_new_input", [], loc)

let make_push_val_call (loc : location) (e : exp) : instr =
  Call(None, getIcall "push_val", [e], loc)

let make_pop_val_call (loc : location) ((rvi, n) : varinfo * string) : instr list =
  match rvi.vtype with
  | TPtr(rt, a) when isInputNTType  rvi.vtype ->
    [Call(None, getIcall "pop_nt", [mkString n; v2e rvi], loc)]
  | TPtr(rt, a) when isInputArrType rvi.vtype ->
    let cnt = getInputArrLen rvi.vtype in
    [Call(None, getIcall "pop_array",
          [mkString n; v2e rvi; integer cnt; SizeOf rt;], loc)]
  | TInt _ -> [Call(Some(var rvi),getIcall "pop_val", [mkString n], loc)]
  | _ -> []

let handle_instr (i : instr) : instr list =
  match i with
  | Set(lhs, UnOp(u, e, rt), loc) ->
    make_assgn_uop_call lhs u rt loc e
  | Set(lhs, BinOp(b, e1, e2, rt), loc) ->
    make_assgn_bop_call lhs b rt loc e1 e2
  | Set(lhs, e, loc) ->
    make_assign_call lhs e loc
  | _ -> [i]

let make_return_pop_call (rlv : lval) (fe : exp) (args : exp list) (loc : location)
                         : instr list
  =
  [Call(Some rlv, fe, args, loc);
   Call(None, getIcall "return_pop", [AddrOf rlv; Lval rlv], loc)]

let make_return_push_call (fd : fundec) (e : exp) (loc : location) : instr list =
  let rvi = makeTempVar fd ~name:"ret" (typeOf e) in
  (handle_instr (Set(var rvi, e, loc)))@
  [Call(None, getIcall "return_push", [AddrOf(var rvi); Lval(var rvi)],loc)]

let cond_id = ref 0
class concolicCalleeVisitor (autotest : bool) (fd : fundec) = object(self)
  inherit nopCilVisitor

  method vinst (i : instr) =
    match i with
    | Set(lhs, UnOp(u, e, rt), loc) ->
      ChangeTo(make_assgn_uop_call lhs u rt loc e)
    | Set(lhs, BinOp(b, e1, e2, rt), loc) ->
      ChangeTo(make_assgn_bop_call lhs b rt loc e1 e2)
    | Set(lhs, e, loc) ->
      ChangeTo(make_assign_call lhs e loc)
    | Call(Some lv, fe, args, loc) ->
      ChangeTo(make_return_pop_call lv fe args loc)
    | _ -> DoChildren

  method vstmt (s : stmt) =
    let action s =
      match s.skind with
      | If(e,tb,fb,loc) -> begin
        let cid = !cond_id in incr cond_id;
        let tinstr, finstr =
          match e with
          | BinOp(b,e1,e2,rt) -> make_cond_bop_call cid b 1 loc e1 e2,
                                 make_cond_bop_call cid b 0 loc e1 e2
          | UnOp(u,e,rt)      -> make_cond_uop_call cid u 1 loc e,
                                 make_cond_uop_call cid u 0 loc e
          | _                 -> make_cond_call cid 1 loc e,
                                 make_cond_call cid 0 loc e
        in
        tb.bstmts <- mkStmt(Instr[tinstr]) :: tb.bstmts;
        fb.bstmts <- mkStmt(Instr[finstr]) :: fb.bstmts;
        s
      end
      | Return(eo,loc) when autotest ->
        mkStmt(Block(mkBlock[mkStmt(Instr[make_input_gen_call loc]);s]))
      | Return(Some e, loc) when not autotest ->
        mkStmt(Block(mkBlock[mkStmt(Instr(make_return_push_call fd e loc));s]))
      | _ -> s
    in
    ChangeDoChildrenPost(s, action)

end

let mk_reset_call (loc : location) : instr =
  Call(None, getIcall "autotest_reset", [], loc)

let makeTestLoop (fd : fundec) (loc : location)
                 (preloop : instr list) (body : instr list)
                 : stmt list
  =
  Formatcil.cStmts "%I:preloop while(%l:finished == 0) { %I:body } %i:reset;"
  (fun n t -> makeTempVar fd ~name:n t) loc
  [("preloop",  FI preloop);
   ("finished", Fl(var !finished));
   ("body",     FI body);
   ("reset",    Fi(mk_reset_call loc));]

let procInstrList (fd : fundec) (il : instr list) : stmt =
  mkStmt(Block(mkBlock
  (L.map (fun i ->
    match i with
    | Call(rvo, fe, args, loc) when fe |> typeOf |> isAutotestType ->
      
      let rt, ats = function_elements fe in
      let init_instrs, nvis =
        ats  |> L.map snd3
             |> L.map (makeTempVar fd ~name:"in")
             |> L.combine args
             |> L.map (fun (a, v) -> Set(var v, a, loc), v)
             |> L.split
      in
      let nargs = L.map v2e nvis in
      let pop_instrs  =
        L.combine nvis (L.map fst3 ats)
             |> L.map (make_pop_val_call loc)
             |> L.concat
      in
      let orig = Call(rvo,fe,nargs,loc) in
      let lbsl = makeTestLoop fd loc init_instrs (orig :: pop_instrs) in
      mkStmt(Block(mkBlock lbsl))
    | _ -> mkStmt(Instr[i])
  ) il)))

class concolicCallerVisitor (fd : fundec) = object(self)
  inherit nopCilVisitor

  method vstmt (s : stmt) =
    match s.skind with
    | Instr il -> ChangeTo(procInstrList fd il)
    | _ -> DoChildren

end

let make_input_reg_call (loc : location) (vi : varinfo) : instr list =
  if not(isInputType vi.vtype) then [] else
  match vi.vtype with
  | TPtr(rt, a) when isInputArrType vi.vtype ->
    let cnt = getInputArrLen vi.vtype in
    [Call(None, getIcall "register_arr_input",
         [mkString vi.vname; v2e vi; SizeOf(rt); integer cnt], loc)]
  | TPtr(rt, a) when isInputNTType  vi.vtype ->
    [Call(None, getIcall "register_nt_input", [mkString vi.vname; v2e vi], loc)]
  | TInt _ ->
    [Call(None, getIcall "register_input",
          [mkString vi.vname; AddrOf(Var vi, NoOffset);
           vi.vtype |> bitsSizeOf |> integer], loc)]
  | _ -> []

let makeAutotestPreamble (fd : fundec) : unit =
  let ircalls =
    fd.sformals |> L.map (make_input_reg_call locUnknown)
                |> L.concat
  in
  let irstmt = mkStmt(Instr ircalls) in
  fd.sbody.bstmts <- irstmt :: fd.sbody.bstmts

let processFunction (fd : fundec) (loc : location) : unit =
  if isAutotestType fd.svar.vtype then
    let vis = new concolicCalleeVisitor true fd in
    ignore(visitCilFunction vis fd);
    makeAutotestPreamble fd
  else if isInstrumentType fd.svar.vtype then
    let vis = new concolicCalleeVisitor false fd in
    ignore(visitCilFunction vis fd)
  else
    let vis = new concolicCallerVisitor fd in
    ignore(visitCilFunction vis fd)

let tut15 (f : file) : unit =
  initCalls f;
  List.iter Simplify.doGlobal f.globals;
  iterGlobals f (onlyFunctions processFunction)

ELSE


let tut15 (f : Cil.file) : unit = ()

END



