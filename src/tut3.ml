



open Cil 
open Pretty
open Tututil

module L  = List     
module E  = Errormsg 

module IH = Inthash  
module DF = Dataflow 


let debug = ref false


type oekind = Top | Odd | Even | Bottom


type varmap = int * (varinfo * oekind)

let id_of_vm   (vm : varmap) : int     = fst vm
let vi_of_vm   (vm : varmap) : varinfo = vm |> snd |> fst
let kind_of_vm (vm : varmap) : oekind  = vm |> snd |> snd


let string_of_oekind (k : oekind) : string =
  match k with
  | Top    -> "Top"
  | Odd    -> "Odd"
  | Even   -> "Even"
  | Bottom -> "Bottom"


let string_of_varmap (vm : varmap) : string =
  let vi = vi_of_vm vm in
  "("^vi.vname^", "^(vm |> kind_of_vm |> string_of_oekind)^")"


let string_of_varmap_list (vml : varmap list) : string =
  vml
  |> L.map string_of_varmap
  |> String.concat ", "


let varmap_list_pretty () (vml : varmap list) =
  vml |> string_of_varmap_list |> text


let oekind_neg (k : oekind) : oekind =
  match k with
  | Even -> Odd
  | Odd -> Even
  | _ -> k


let varmap_equal (vm1 : varmap) (vm2 : varmap) : bool =
  (id_of_vm vm1) = (id_of_vm vm2) &&
  (kind_of_vm vm1) = (kind_of_vm vm2)


let varmap_list_equal (vml1 : varmap list) (vml2 : varmap list) : bool =
  let sort = L.sort (fun (id1,_) (id2,_) -> compare id1 id2) in
  list_equal varmap_equal (sort vml1) (sort vml2)


let oekind_includes (is_this : oekind) (in_this : oekind) : bool =
  match is_this, in_this with
  | _, Top -> true
  | Bottom, _ -> true
  | _, _ -> false


let oekind_combine (k1 : oekind) (k2 : oekind) : oekind =
  match k1, k2 with
  | Top, _ | _, Top | Odd, Even | Even, Odd -> Top
  | Odd, _ | _, Odd -> Odd
  | Even, _ | _, Even -> Even
  | Bottom, Bottom -> Bottom


let varmap_combine (vm1 : varmap) (vm2 : varmap) : varmap option =
  match vm1, vm2 with
  | (id1, _), (id2, _) when id1 <> id2 -> None
  | (id1, (vi1, k1)), (_,(_,k2)) -> Some(id1,(vi1,oekind_combine k1 k2))


let varmap_list_combine_one (vml : varmap list) (vm : varmap) : varmap list =
  let id = id_of_vm vm in
  if L.mem_assoc id vml then
    let vm' = (id, L.assoc id vml) in
    let vm'' = forceOption (varmap_combine vm vm') in
    vm'' :: (L.remove_assoc (id_of_vm vm) vml)
  else vm :: vml


let varmap_list_combine (vml1 : varmap list) (vml2 : varmap list) : varmap list =
  L.fold_left varmap_list_combine_one vml1 vml2


let varmap_list_replace (vml : varmap list) (vm : varmap) : varmap list =
  vm :: (L.remove_assoc (id_of_vm vm) vml)


let kind_of_int64 (i : Int64.t) : oekind =
  let firstbit = Int64.logand i Int64.one in
  if firstbit = Int64.one then Odd else Even


let rec oekind_of_exp (vml : varmap list) (e : exp) : oekind =
  match e with
  | Const(CInt64(i, _, _)) -> kind_of_int64 i
  | Lval(Var vi, NoOffset) -> vml |> L.assoc vi.vid |> snd
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
    e |> constFold true |> oekind_of_exp vml
  | UnOp(uo, e, t) -> oekind_of_unop vml uo e
  | BinOp(bo, e1, e2, t) -> oekind_of_binop vml bo e1 e2
  | CastE(t, e) -> oekind_of_exp vml e
  | _ -> Top


and oekind_of_unop (vml : varmap list) (u : unop) (e : exp) : oekind =
  match u with
  | Neg  -> oekind_of_exp vml e
  | BNot -> e |> oekind_of_exp vml |> oekind_neg
  | LNot -> Top


and oekind_of_binop (vml : varmap list) (b : binop) (e1 : exp) (e2 : exp) : oekind =
  let k1, k2 = oekind_of_exp vml e1, oekind_of_exp vml e2 in
  match b with
  | PlusA -> begin
    match k1, k2 with
    | Even, Even -> Even
    | Odd, Odd -> Even
    | Even, Odd -> Odd
    | Odd, Even -> Odd
    | _, _ -> Top
  end
  
  | MinusA -> begin
    match k1, k2 with
    | Even, Even -> Even
    | Odd, Odd -> Even
    | Even, Odd -> Odd
    | Odd, Even -> Odd
    | _, _ -> Top
  end
  | Mult -> begin
    match k1, k2 with
    | Even, _ | _ , Even -> Even
    | Odd, Odd -> Odd
    | _, _ -> Top
  end
  
  
  | _ -> Top



let varmap_list_kill (vml : varmap list) : varmap list =
  L.map (fun (vid, (vi, k)) ->
    if vi.vaddrof then (vid, (vi, Top)) else (vid, (vi, k)))
  vml


let varmap_list_handle_inst (i : instr) (vml : varmap list) : varmap list =
  match i with
  | Set((Var vi, NoOffset), e, loc) when not(vi.vglob) && (isIntegralType vi.vtype) ->
    let k = oekind_of_exp vml e in
    varmap_list_replace vml (vi.vid,(vi,k)) 
  | Set((Mem _, _), _, _)
  | Call _ -> varmap_list_kill vml 
  | _ -> vml 


module OddEvenDF = struct

  let name = "OddEven"
  let debug = debug
  type t = varmap list
  let copy vml = vml
  let stmtStartData = IH.create 64
  let pretty = varmap_list_pretty
  let computeFirstPredecessor stm vml = vml


  let combinePredecessors (s : stmt) ~(old : t) (ll : t) =
    if varmap_list_equal old ll then None else
    Some(varmap_list_combine old ll)

  let doInstr (i : instr) (ll : t) =
    let action = varmap_list_handle_inst i in
    DF.Post action


  let doStmt stm ll = DF.SDefault
  let doGuard c ll = DF.GDefault
  let filterStmt stm = true

end


module OddEven = DF.ForwardsDataFlow(OddEvenDF)


let collectVars (fd : fundec) : varmap list =
  (fd.sformals @ fd.slocals)
  |> L.filter (fun vi -> isIntegralType vi.vtype)
  |> L.map (fun vi -> (vi.vid, (vi, Bottom)))


let computeOddEven (fd : fundec) : unit =
  Cfg.clearCFGinfo fd;
  ignore(Cfg.cfgFun fd);
  let first_stmt = L.hd fd.sbody.bstmts in
  let vml = collectVars fd in
  IH.clear OddEvenDF.stmtStartData;
  IH.add OddEvenDF.stmtStartData first_stmt.sid vml;
  OddEven.compute [first_stmt]


let getOddEvens (sid : int) : varmap list option =
  try Some(IH.find OddEvenDF.stmtStartData sid)
  with Not_found -> None


let instrOddEvens (il : instr list) (vml : varmap list) : varmap list list =
  let proc_one hil i =
    match hil with
    | [] -> (varmap_list_handle_inst i vml) :: hil
    | vml':: rst as l -> (varmap_list_handle_inst i vml') :: l
  in
  il |> L.fold_left proc_one [vml]
     |> L.tl
     |> L.rev


class vmlVisitorClass = object(self)
  inherit nopCilVisitor

  val mutable sid           = -1
  val mutable state_list    = []
  val mutable current_state = None

  method vstmt stm =
    sid <- stm.sid;
    begin match getOddEvens sid with
    | None -> current_state <- None
    | Some vml -> begin
      match stm.skind with
      | Instr il ->
        current_state <- None;
        state_list <- instrOddEvens il vml
      | _ -> current_state <- None
    end end;
    DoChildren

  method vinst i =
    try let data = L.hd state_list in
        current_state <- Some(data);
        state_list <- L.tl state_list;
        DoChildren
    with Failure "hd" -> DoChildren

  method get_cur_vml () =
    match current_state with
    | None -> getOddEvens sid
    | Some vml -> Some vml

end


class varUseReporterClass = object(self)
  inherit vmlVisitorClass as super

  method vvrbl (vi : varinfo) =
    match self#get_cur_vml () with
    | None -> SkipChildren
    | Some vml -> begin
      if L.mem_assoc vi.vid vml then begin
        let vm = (vi.vid, L.assoc vi.vid vml) in
        E.log "%a: %a\n" d_loc (!currentLoc) varmap_list_pretty [vm]
      end;
      SkipChildren
    end

end


let evenOddAnalysis (fd : fundec) (loc : location) : unit =
  computeOddEven fd;
  let vis = ((new varUseReporterClass) :> nopCilVisitor) in
  ignore(visitCilFunction vis fd)


let tut3 (f : file) : unit =
  iterGlobals f (onlyFunctions evenOddAnalysis)



