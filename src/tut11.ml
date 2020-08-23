



IFDEF BUILD_TUT11 THEN




open Cil
open Tututil

module Em = Errormsg
module L = List


module W = Why3 
module T = W.Term 
module Th = W.Theory 

module E = W.Env
module Wc = W.Whyconf
module D = W.Decl
module P = W.Pretty




type ops = {
  iplus_op  : T.lsymbol; 
  iminus_op : T.lsymbol; 
  itimes_op : T.lsymbol; 
  idiv_op   : T.lsymbol; 
  imod_op   : T.lsymbol; 
  lt_op     : T.lsymbol; 
  
  gt_op     : T.lsymbol;
  lte_op    : T.lsymbol;
  gte_op    : T.lsymbol;
  
  
  get_op    : T.lsymbol; 
  set_op    : T.lsymbol;
}


type wctxt = {

  mutable env    : E.env;
  mutable task   : W.Task.task;
  mutable prover : Wc.config_prover;
  mutable driver : W.Driver.driver;


  mutable ops    : ops;
  mutable memory : T.vsymbol;
  mutable vars   : T.vsymbol SM.t;
}


let initOps (it : Th.theory) (dt : Th.theory) (mt : Th.theory) : ops =
  {iplus_op    = Th.ns_find_ls it.Th.th_export ["infix +"];
   iminus_op   = Th.ns_find_ls it.Th.th_export ["infix -"];


   itimes_op   = Th.ns_find_ls it.Th.th_export ["infix *"];
   idiv_op     = Th.ns_find_ls dt.Th.th_export ["div"];
   imod_op     = Th.ns_find_ls dt.Th.th_export ["mod"];
   lt_op       = Th.ns_find_ls it.Th.th_export ["infix <"];
   gt_op       = Th.ns_find_ls it.Th.th_export ["infix >"];
   lte_op      = Th.ns_find_ls it.Th.th_export ["infix <="];
   gte_op      = Th.ns_find_ls it.Th.th_export ["infix >="];
   get_op      = Th.ns_find_ls mt.Th.th_export ["get"];
   set_op      = Th.ns_find_ls mt.Th.th_export ["set"];

  }


let initWhyCtxt (p : string) (pv : string) : wctxt =

  let config  = Wc.read_config None in
  let main    = Wc.get_main config in
  Wc.load_plugins main;
  let provers = Wc.get_provers config in
  Wc.Mprover.iter
    (fun k a ->
      Em.warn "%s %s (%s)" k.Wc.prover_name k.Wc.prover_version k.Wc.prover_altern
    ) provers;
  let prover_spec = {Wc.prover_name =p; Wc.prover_version=pv; Wc.prover_altern=""} in
  let prover =
    try Wc.Mprover.find prover_spec provers
    with Not_found -> Em.s (Em.error "Prover %s not found." p)
  in
  let env = E.create_env (W.Whyconf.loadpath main) in
  let driver : W.Driver.driver =
    try W.Driver.load_driver env prover.Wc.driver []
    with e -> Em.s (Em.error "Failed to load driver for %s." p)
  in
  let int_theory = E.find_theory env ["int"] "Int" in
  let div_theory = E.find_theory env ["int"] "ComputerDivision" in
  let arr_theory = E.find_theory env ["map"] "Map" in
  let task =
    L.fold_left W.Task.use_export None
      [int_theory; div_theory; arr_theory]
  in
  let arr_ts     = Th.ns_find_ts arr_theory.Th.th_export ["map"] in
  let int_arr_t  = W.Ty.ty_app arr_ts [W.Ty.ty_int; W.Ty.ty_int] in
  {env = env; task = task; prover = prover; driver = driver;
   ops = initOps int_theory div_theory arr_theory;
   memory = T.create_vsymbol (W.Ident.id_fresh "M") int_arr_t;
   vars = SM.empty;}




let invariantAttrStr = "invariant"
let postAttrStr      = "post"
let preAttrStr       = "pre"
let tut11_attrs = [invariantAttrStr; postAttrStr; preAttrStr;]


let term_of_int (i : int) : T.term   = i |> string_of_int   |> T.t_int_const
let term_of_i64 (i : int64) : T.term = i |> Int64.to_string |> T.t_int_const


let make_symbol (s : string) : T.vsymbol =
  T.create_vsymbol (W.Ident.id_fresh s) W.Ty.ty_int

let freshvar_of_ap (ap : attrparam) : string * T.vsymbol =
  match ap with
  | ACons(n, []) -> n, make_symbol n
  | _ -> Em.s(Em.error "Names only")


let rec term_of_attrparam (wc : wctxt) (ap : attrparam) : T.term =
  match ap with
  | AInt i                 -> term_of_int i
  | ACons(n,[])            -> T.t_var (SM.find n wc.vars)
  | ACons("forall",apl)    -> term_of_forall wc apl
  | ACons("implies",[a;c]) -> term_of_implies wc a c
  | AUnOp(uo, ap)          -> term_of_apuop wc uo ap
  | ABinOp(bo, ap1, ap2)   -> term_of_apbop wc bo ap1 ap2
  | AStar ap               -> term_of_star wc ap
  | AIndex(base, index)    -> term_of_index wc base index


  | AStr _ -> Em.s(Em.unimp "AStr -> Term")
  | ASizeOf _
  | ASizeOfE _
  | ASizeOfS _
  | AAlignOf _
  | AAlignOfE _
  | AAlignOfS _  -> Em.s(Em.unimp "A{Size,Align}Of* -> Term")
  | ADot(ap, s) -> Em.s(Em.unimp "ADot -> Term")
  | AAddrOf ap -> Em.s(Em.unimp "AAddrOf -> Term")
  | AQuestion(ap1, ap2, ap3) -> Em.s(Em.unimp "AQuestion -> Term")

  | _ -> Em.s(Em.error "Attrparam is not a term: %a" d_attrparam ap)


and term_of_forall (wc : wctxt) (apl : attrparam list) : T.term =
  let fat  = apl |> L.rev |> L.hd in
  let vl   = apl |> L.rev |> L.tl |> L.map freshvar_of_ap in
  let oldm = wc.vars in
  wc.vars <- L.fold_left (fun m (n,v) -> SM.add n v m) oldm vl;
  let t = term_of_attrparam wc fat in
  wc.vars <- oldm;
  T.t_forall_close (L.map snd vl) [] t


and term_of_implies (wc : wctxt) (a : attrparam) (c : attrparam) : T.term =
  let at = term_of_attrparam wc a in
  let ct = term_of_attrparam wc c in
  T.t_implies at ct


and term_of_apuop (wc : wctxt) (u : unop) (ap : attrparam) : T.term =
  let te = term_of_attrparam wc ap in
  match u with
  | Neg  -> T.t_app_infer wc.ops.iminus_op [(term_of_int 0);te]
  | LNot -> T.t_equ te (term_of_int 0)
  | BNot -> Em.s (Em.unimp "Attribute BNot: ~%a\n" d_attrparam ap)

and term_of_apbop (wc : wctxt) (b : binop) (ap1 : attrparam) (ap2 : attrparam) : T.term =
  let te1 = term_of_attrparam wc ap1 in
  let te2 = term_of_attrparam wc ap2 in
  match b with
  | PlusA  | PlusPI  | IndexPI -> T.t_app_infer wc.ops.iplus_op  [te1; te2]
  | MinusA | MinusPI | MinusPP -> T.t_app_infer wc.ops.iminus_op [te1; te2]


  | Mult -> T.t_app_infer wc.ops.itimes_op [te1; te2]
  | Div  -> T.t_app_infer wc.ops.idiv_op   [te1; te2]
  | Mod  -> T.t_app_infer wc.ops.imod_op   [te1; te2]
  | Lt   -> T.t_app_infer wc.ops.lt_op  [te1; te2]
  | Gt   -> T.t_app_infer wc.ops.gt_op  [te1; te2]
  | Le   -> T.t_app_infer wc.ops.lte_op [te1; te2]
  | Ge   -> T.t_app_infer wc.ops.gte_op [te1; te2]
  | Eq   -> T.t_equ te1 te2
  | Ne   -> T.t_neq te1 te2
  | LAnd -> T.t_and te1 te2
  | LOr  -> T.t_or  te1 te2

  | _ -> Em.s (Em.error "term_of_bop failed: %a %a %a\n"
              d_attrparam ap1 d_binop b d_attrparam ap2)


and term_of_star (wc : wctxt) (a : attrparam) : T.term =
  let at = term_of_attrparam wc a in
  let mt = T.t_var wc.memory in
  T.t_app_infer wc.ops.get_op [mt;at]


and term_of_index (wc : wctxt) (base : attrparam) (index : attrparam) : T.term =
  let bt = term_of_attrparam wc base in
  let it = term_of_attrparam wc index in
  let addr = T.t_app_infer wc.ops.iplus_op [bt; it] in
  let mt = T.t_var wc.memory in
  T.t_app_infer wc.ops.get_op [mt; addr]


let oldvar_of_ap (wc : wctxt) (ap : attrparam) : T.vsymbol =
  match ap with
  | ACons(n, []) -> SM.find n wc.vars
  | _ -> Em.s(Em.error "Names only")


let inv_of_attrs (wc : wctxt) (a : attributes)
                 : T.term * T.term * T.vsymbol list
  =
  match filterAttributes invariantAttrStr a with
  | [Attr(_,lc :: li :: rst)] ->
    term_of_attrparam wc lc,
    term_of_attrparam wc li,
    L.map (oldvar_of_ap wc) rst
  | _ -> Em.s(Em.error "Malformed invariant attribute: %a" d_attrlist a)


let cond_of_function (k : string) (wc : wctxt) (fd : fundec) : T.term option =
  match filterAttributes k (typeAttrs fd.svar.vtype) with
  | [Attr(_,[ap])] -> Some(term_of_attrparam wc ap)
  | _ -> None

let post_of_function = cond_of_function postAttrStr
let pre_of_function  = cond_of_function preAttrStr


let iterm_of_bterm (t : T.term) : T.term = T.t_if t (term_of_int 1) (term_of_int 0)
let bterm_of_iterm (t : T.term) : T.term = T.t_neq t (term_of_int 0)

let rec term_of_exp (wc : wctxt) (e : exp) : T.term = 

  match e with
  | Const(CInt64(i,_,_))   -> term_of_i64 i
  | Lval(Var vi, NoOffset) -> T.t_var (SM.find vi.vname wc.vars)
  | Lval(Mem e, NoOffset)  ->
    let et = term_of_exp wc e in
    let mt = T.t_var wc.memory in
    T.t_app_infer wc.ops.get_op [mt;et]
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
    e |> constFold true |> term_of_exp wc
  | UnOp(uo, e, typ) -> term_of_uop wc uo e
  | BinOp(bo, e1, e2, typ) -> term_of_bop wc bo e1 e2
  | CastE(t, e) -> term_of_exp wc e
  | AddrOf _
  | StartOf _
  | _ -> Em.s(Em.error "term_of_exp failed: %a" d_exp e)


and term_of_uop (wc : wctxt) (u : unop) (e : exp) : T.term = 

  let te = term_of_exp wc e in
  match u with
  | Neg  -> T.t_app_infer wc.ops.iminus_op [(term_of_int 0);te]
  | LNot -> iterm_of_bterm (T.t_equ te (term_of_int 0))
  | BNot -> Em.s (Em.error "term_of_uop failed: ~%a\n" d_exp e)


and term_of_bop (wc : wctxt) (b : binop) (e1 : exp) (e2 : exp) : T.term = 

  let te1 = term_of_exp wc e1 in
  let te2 = term_of_exp wc e2 in
  match b with
  | PlusA  | PlusPI  | IndexPI -> T.t_app_infer wc.ops.iplus_op  [te1; te2]
  | MinusA | MinusPI | MinusPP -> T.t_app_infer wc.ops.iminus_op [te1; te2]
  | Mult -> T.t_app_infer wc.ops.itimes_op [te1; te2]
  | Div  -> T.t_app_infer wc.ops.idiv_op   [te1; te2]
  | Mod  -> T.t_app_infer wc.ops.imod_op   [te1; te2]
  | Lt   -> iterm_of_bterm (T.t_app_infer wc.ops.lt_op  [te1; te2])
  | Gt   -> iterm_of_bterm (T.t_app_infer wc.ops.gt_op  [te1; te2])
  | Le   -> iterm_of_bterm (T.t_app_infer wc.ops.lte_op [te1; te2])
  | Ge   -> iterm_of_bterm (T.t_app_infer wc.ops.gte_op [te1; te2])
  | Eq   -> iterm_of_bterm (T.t_equ te1 te2)
  | Ne   -> iterm_of_bterm (T.t_neq te1 te2)
  | LAnd -> iterm_of_bterm (T.t_and te1 te2)
  | LOr  -> iterm_of_bterm (T.t_or  te1 te2)
  | _ -> Em.s (Em.error "term_of_bop failed: %a %a %a\n"
              d_exp e1 d_binop b d_exp e2)




let term_of_inst (wc : wctxt) (i : instr) : T.term -> T.term =
  match i with
  | Set((Var vi, NoOffset), e, loc) ->
    let te = term_of_exp wc e in
    let vs = SM.find vi.vname wc.vars in
    T.t_let_close vs te
    
  | Set((Mem me, NoOffset), e, loc) ->
    let te = term_of_exp wc e in
    let tme = term_of_exp wc me in
    let ms = wc.memory in
    let ume = T.t_app_infer wc.ops.set_op [T.t_var ms; tme; te] in
    T.t_let_close ms ume
    
  
  | _ -> Em.s (Em.error "term_of_inst: We can only handle assignment")


let rec term_of_stmt (wc : wctxt) (s : stmt) : T.term -> T.term =
  match s.skind with
  | Instr il          -> L.fold_right (fun i t -> (term_of_inst wc i) t) il
  | If(e,tb,fb,loc)   -> term_of_if wc e tb fb
  | Loop(b,loc,bo,co) -> term_of_loop wc b
  | Block b           -> term_of_block wc b
  | Return(eo, loc)   -> (fun t -> t)


  | Switch _
  | Goto _
  | Break _
  | Continue _ -> Em.s (Em.error "Unimplemented")

  | _ -> Em.s(Em.error "No support for try-finally, or try-except")


and term_of_if (wc : wctxt) (e : exp) (tb : block) (fb : block) : T.term -> T.term =
  let te  = e |> term_of_exp wc |> bterm_of_iterm in
  let tbf = term_of_block wc tb in
  let fbf = term_of_block wc fb in
  (fun t -> T.t_if te (tbf t) (fbf t))


and term_of_loop (wc : wctxt) (b : block) : T.term -> T.term =
  let test, body = L.hd b.bstmts, L.tl b.bstmts in
  let body_block = body |> L.hd |> force_block in
  let bf = term_of_block wc (mkBlock (body_block.bstmts @ (L.tl body))) in
  let ct, li, lvl = inv_of_attrs wc body_block.battrs in
  let lvl' = wc.memory :: lvl in
  (fun t -> t
    |> T.t_if ct (bf li)        
    |> T.t_implies li           
    |> T.t_forall_close lvl' [] 
    |> T.t_and li)              


and term_of_block (wc : wctxt) (b : block) : T.term -> T.term =
  L.fold_right (term_of_stmt wc) b.bstmts


let vsymbols_of_function (wc : wctxt) (fd : fundec) : T.vsymbol list =
  fd.sformals
  |> L.map (fun vi -> vi.vname)
  |> sm_find_all wc.vars
  |> L.append [wc.memory]


let pre_impl_t (wc : wctxt) (fd : fundec) (pre : T.term option) : T.term -> T.term =
  match pre with
  | None -> term_of_block wc fd.sbody
  | Some pre -> (fun t -> T.t_implies pre (term_of_block wc fd.sbody t))


let vcgen (wc : wctxt) (fd : fundec) (pre : T.term option) : T.term -> T.term =
  (fun t -> T.t_forall_close (vsymbols_of_function wc fd) [] (pre_impl_t wc fd pre t))


let validateWhyCtxt (w : wctxt) (p : T.term) : unit = 

  Format.printf "@[validate:@ %a@]@." W.Pretty.print_term p;
  let g = D.create_prsymbol (W.Ident.id_fresh "goal") in
  let t = W.Task.add_prop_decl w.task D.Pgoal g p in
  let res =
    W.Call_provers.wait_on_call
      (W.Driver.prove_task ~command:w.prover.Wc.command
                           ~timelimit:120 w.driver t ())
      ()
  in
  Format.printf "@[Prover answers:@ %a@]@.@[%s@]@."
    W.Call_provers.print_prover_result res res.W.Call_provers.pr_output;
  ()




let processFunction (wc : wctxt) (fd : fundec) (loc : location) : unit =
  wc.vars <-
    L.fold_left (fun m vi -> SM.add vi.vname (make_symbol vi.vname) m)
      SM.empty (fd.slocals @ fd.sformals);
  match post_of_function wc fd with
  | None   -> ()
  | Some g ->
    let pre = pre_of_function wc fd in
    let vc = vcgen wc fd pre g in
    validateWhyCtxt wc vc


class attrEraserVisitor = object(self)
  inherit nopCilVisitor

  method vattr (a : attribute) =
    match a with
    | Attr(s,_) when L.mem s tut11_attrs -> ChangeTo []
    | _ -> DoChildren

end

let eraseAttrs (f : file) : unit =
  let vis = new attrEraserVisitor in
  visitCilFile vis f



let tut11 (f : file) : unit =
  let wc = initWhyCtxt (!Ciltutoptions.prover) (!Ciltutoptions.prover_version) in
  iterGlobals f (onlyFunctions (processFunction wc));
	eraseAttrs f


ELSE


let tut11 (f : Cil.file) : unit = ()

END




