open Concolic_util

module E = Concolic_exp
module Y = Yices
module H = Hashtbl

type context = {
	mutable yctx    : Y.context;
	mutable inv     : Y.expr list;
	mutable input   : (int64, string * Y.var_decl) H.t;
	mutable modelHT : (string, int64) H.t;
	mutable memory  : (int64, E.exp) H.t;
}

let global_ctxt : context option ref = ref None

let makeContext () : context =
	let ctx = Y.mk_context() in
	{yctx    = ctx;
	 inv     = [];
	 input   = H.create 256;
	 modelHT = H.create 256;
	 memory  = H.create 256;}

let resetContext () : unit =
	let c = !global_ctxt |> forceOption in
	Y.del_context c.yctx;
	c.yctx   <- Y.mk_context();
	c.inv    <- [];
	H.clear c.input;
	H.clear c.modelHT;
	H.clear c.memory

let makeVarDecl (c : context) (n : string) (t : Y.typ) : Y.var_decl =
	try Y.get_var_decl_from_name c.yctx n
	with Failure _ -> Y.mk_var_decl c.yctx n t

let makeBVLimit (c : context) (bits : int) : Y.expr =
	if bits < 64 then
		let maxval = Array.make 64 false in
		maxval.(bits) <- true;
		Y.mk_bv_constant_from_array c.yctx maxval
	else let maxval = Array.make 64 true in
	Y.mk_bv_constant_from_array c.yctx maxval

let makeBVZero (c : context) (bits : int) : Y.expr =
	Y.mk_bv_constant c.yctx 64 Int32.zero

let makeBVOne  (c : context) (bits : int) : Y.expr =
	Y.mk_bv_constant c.yctx 64 Int32.one

let makeNewInput (name : string) (addr : int64) (bits : int) : unit =
	let c   = !global_ctxt |> forceOption in
	let ul  = makeBVLimit c bits in
	let t   = Y.mk_bv_type c.yctx 64 in
	let vd  = makeVarDecl c name t in
	let var = Y.mk_var_from_decl c.yctx vd in
	H.replace c.input addr (name, vd);
	c.inv <- (Y.mk_bv_lt c.yctx var ul) :: c.inv

let getInput (c : context) (addr : int64) : Y.expr =
	addr
	|> H.find c.input
	|> snd
	|> Y.mk_var_from_decl c.yctx

let getState (v : int64) (a : int64) : E.exp =
	let c = !global_ctxt |> forceOption in
	try H.find c.memory a
	with Not_found ->
		if H.mem c.input a then E.Input a
		else E.Op(v, (E.Constant, v))

let rec simplifyExp (e : E.exp) : E.exp =
	match e with
	| E.Op(v, (E.Constant, v')) -> e
	| E.Op(v, (E.Address, a)) -> a |> getState v |> simplifyExp
	| E.UnOp(v, u, e') -> begin
		let e' = simplifyExp e' in
		match e' with
		| E.Op(v', (E.Constant, v'')) -> E.Op(v, (E.Constant, E.apply_uop u v'))
		| _ -> E.UnOp(v, u, e')
	end
	| E.BinOp(v, b, e1, e2) -> begin
		let e1' = simplifyExp e1 in
		let e2' = simplifyExp e2 in
		match e1', e2' with
		| E.Op(v1, (E.Constant, v1')), E.Op(v2, (E.Constant, v2')) ->
			E.Op(v, (E.Constant, E.apply_bop b v1 v2))
		| _ -> E.BinOp(v, b, e1', e2')
	end
	| E.Input a -> E.Input a

let addState (dest : int64) (e : E.exp) : unit =
	let c = !global_ctxt |> forceOption in
	H.replace c.memory dest (simplifyExp e)

let ctxtStoreVal (c : context) (n : string) (v : int64) : unit =
	let c = !global_ctxt |> forceOption in
	H.replace c.modelHT n v

let storeVal (n : string) (v : int64) : unit =
	let c = !global_ctxt |> forceOption in
	H.replace c.modelHT n v

let getVal (n : string) : int64 =
	let c = !global_ctxt |> forceOption in
	try H.find c.modelHT n with Not_found -> Int64.zero

let rec yices_of_exp (c : context) (e : E.exp) : Y.expr =
	match e with
	| E.Input a -> getInput c a
	| E.UnOp(v, u, e) -> begin
		let ye = yices_of_exp c e in
		match u with
		| E.Neg  -> Y.mk_bv_minus c.yctx ye
		| E.BNot -> Y.mk_bv_not   c.yctx ye
		| E.LNot -> Y.mk_ite c.yctx (Y.mk_eq c.yctx ye (makeBVZero c 64))
			                          (makeBVOne c 64) (makeBVZero c 64)
	end
	| E.BinOp(v, b, e1, e2) -> begin
		let ye1 = yices_of_exp c e1 in
		let ye2 = yices_of_exp c e2 in
		match b with
		| E.Plus     -> Y.mk_bv_add c.yctx ye1 ye2
		| E.Minus    -> Y.mk_bv_sub c.yctx ye1 ye2
		| E.Times    -> Y.mk_bv_mul c.yctx ye1 ye2
		| E.Div      -> Y.mk_bv_constant c.yctx 64 (int32_of_int64 v)
		| E.Mod      -> Y.mk_bv_constant c.yctx 64 (int32_of_int64 v)
		| E.Shiftl   -> Y.mk_bv_constant c.yctx 64 (int32_of_int64 v)
		| E.Shiftrl  -> Y.mk_bv_constant c.yctx 64 (int32_of_int64 v)
		| E.Shiftra  -> Y.mk_bv_constant c.yctx 64 (int32_of_int64 v)
		| E.Lt       -> Y.mk_bv_slt c.yctx ye1 ye2
		| E.Gt       -> Y.mk_bv_sgt c.yctx ye1 ye2
		| E.Le       -> Y.mk_bv_sle c.yctx ye1 ye2
		| E.Ge       -> Y.mk_bv_sge c.yctx ye1 ye2
		| E.Eq       -> Y.mk_eq     c.yctx ye1 ye2
		| E.Ne       -> Y.mk_diseq  c.yctx ye1 ye2
		| E.BAnd     -> Y.mk_bv_and c.yctx ye1 ye2
		| E.BXor     -> Y.mk_bv_xor c.yctx ye1 ye2
		| E.BOr      -> Y.mk_bv_or  c.yctx ye1 ye2
		| E.LAnd     -> Y.mk_and    c.yctx [|ye1; ye2|]
		| E.LOr      -> Y.mk_or     c.yctx [|ye1; ye2|]
	end
	| E.Op(v, (E.Constant, _)) -> Y.mk_bv_constant c.yctx 64 (int32_of_int64 v)
	| E.Op(v, (E.Address, _)) -> failwith "Bad exp in yices_of_exp"

