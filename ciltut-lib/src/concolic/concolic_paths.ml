open Concolic_util

module E = Concolic_exp
module C = Concolic_ctxt

module Y = Yices
module L = List
module A = Array
module H = Hashtbl
module S = String

type brState = NeitherTaken | TrueTaken | FalseTaken | BothTaken

type branchID = int * int

type ybranch = {
	mutable ybid : branchID;
	mutable yexp : Y.expr;
	mutable ybs  : brState;
}

type ebranch = {
	mutable bid : branchID;
	mutable exp : E.exp;
	mutable bs  : brState;
}

type path_state = {
	mutable branchHT     : (branchID, brState) H.t;
	mutable paths        : ybranch array list;
	mutable branchCounts : (int, int) H.t;
	mutable pathCond     : ebranch list;
}

let pState : path_state = {
	branchHT     = H.create 256;
	paths        = [];
	branchCounts = H.create 256;
	pathCond     = [];
}

let brState_of_int (r : int) : brState =
	match r with
	| 0 -> FalseTaken
	| 1 -> TrueTaken
	| _ -> raise(Failure "brState_of_int")

let merge_brState (bs1 : brState) (bs2 : brState) : brState =
	match bs1, bs2 with
	| BothTaken, _ | _, BothTaken
	| TrueTaken, FalseTaken | FalseTaken, TrueTaken -> BothTaken
	| NeitherTaken, bs | bs, NeitherTaken -> bs
	| _ -> bs1


let getBranchCount (cid : int) : int =
	try
		let oldcnt = H.find pState.branchCounts cid in
		H.replace pState.branchCounts cid (oldcnt + 1);
		oldcnt
	with Not_found ->
		H.replace pState.branchCounts cid 1;
		0

let mk_ebranch (e : E.exp) (cid : int) (cnt : int) (bs : brState) : ebranch =
	{exp = e; bid = cid,cnt; bs = bs;}

let addPathCond (cid : int) (r : int) (c : E.exp) : unit =
	let cnt = getBranchCount cid in
	let bs  = brState_of_int r in
	match C.simplifyExp c with
	| E.Op _ -> ()
	| _    -> pState.pathCond <- (mk_ebranch c cid cnt bs) :: pState.pathCond


let yicesb_of_expb (c : C.context) (eb : ebranch) : ybranch =
	{yexp = C.yices_of_exp c eb.exp; ybid = eb.bid; ybs = eb.bs}
let mk_yicesb_inv (c : C.context) (yel : Y.expr list) : ybranch =
	{yexp = yel |> A.of_list |> Y.mk_and c.C.yctx;
	 ybid = (-1,-1); ybs = NeitherTaken;}

let ybranch_swap (c : C.context) (yb : ybranch) : ybranch =
	{yb with yexp = Y.mk_not c.C.yctx yb.yexp}

let updateBState (eb : ebranch) : unit =
	try eb.bid |> H.find pState.branchHT
		         |> merge_brState eb.bs
		         |> H.replace pState.branchHT eb.bid
	with Not_found -> H.add pState.branchHT eb.bid eb.bs


let updatePathConds (c : C.context) : unit =
	let ybl = L.map (yicesb_of_expb c) pState.pathCond in
	let ybl = (mk_yicesb_inv c c.C.inv) :: ybl in
	pState.paths <- (A.of_list ybl) :: pState.paths;
	L.iter updateBState pState.pathCond

let has_swappable_branch ?(excludes : branchID list = [])
                          (yba : ybranch array) : bool =
	yba |> A.to_list
	    |> L.filter (fun yb -> yb.ybs <> NeitherTaken)
	    |> L.filter (fun yb -> not(L.mem yb.ybid excludes))
	    |> L.exists (fun yb -> H.find pState.branchHT yb.ybid <> BothTaken)

let is_swappable_branch ?(excludes : branchID list = []) (yb : ybranch) : bool =
	yb.ybs <> NeitherTaken &&
	H.find pState.branchHT yb.ybid <> BothTaken &&
	not(L.mem yb.ybid excludes)

let genNextPath ?(excludes : branchID list = []) (c : C.context)
                : (Y.expr array * branchID) option
	=
	try
		let p = pState.paths |> L.find (has_swappable_branch ~excludes) |> A.copy in
		let si = p |> array_find (is_swappable_branch ~excludes) |> L.hd in
		p.(si) <- ybranch_swap c p.(si);
		Some(p |> A.map (fun yb -> yb.yexp), p.(si).ybid)
	with Not_found -> None

let clearPathCond () : unit =
	H.clear pState.branchCounts;
	pState.pathCond <- []

let writeOneVal (c : C.context) (m : Y.model)
                (a : int64) ((n, vd) : string * Y.var_decl)
                : unit =
	try let ba = Y.get_bv_value m vd 64 in
	ba |> int64_of_bool_array |> C.ctxtStoreVal c n
	with _ -> C.ctxtStoreVal c n Int64.zero

let writeModelVals (c : C.context) : unit =
	let m = Y.get_model c.C.yctx in
	H.iter (writeOneVal c m) c.C.input;
	c.C.inv <- [];
	H.clear c.C.memory

let genNextInputs (c : C.context) (yea : Y.expr array) (bid : branchID) : bool =
	let ye = Y.mk_and c.C.yctx yea in
	let aid = Y.assert_retractable c.C.yctx ye in
	match Y.check c.C.yctx with
	| Y.True -> begin
		writeModelVals c;
		Y.retract c.C.yctx aid;
		true
	end
	| Y.False
	| Y.Undef ->
		Y.retract c.C.yctx aid;
		false

let pathsFindNext () : int =
	let c = !C.global_ctxt |> forceOption in
	updatePathConds c;
	clearPathCond ();
	let rec helper excludes =
		match genNextPath ~excludes c with
		| None -> 0
		| Some(yea, bid) ->
			if genNextInputs c yea bid then 1 else
			helper (bid :: excludes)
	in
	helper []

let resetPaths () : unit =
	H.clear pState.branchHT;
	H.clear pState.branchCounts;
	pState.paths <- [];
	pState.pathCond <- []
