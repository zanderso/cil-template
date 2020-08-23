open Concolic_util

module C = Concolic_ctxt
module E = Concolic_exp
module P = Concolic_paths

module Y = Yices
module L = List
module A = Array
module H = Hashtbl
module S = String

let assign (lhs : int64) (op : int64) (opk : int) (opv : int64) : unit =
	C.addState lhs (E.mkOpExp op opk opv)

let assgn_bop (lhs : int64) (lhsv : int64) (bop : int)
              (op1 : int64) (op1k : int) (op1v : int64)
              (op2 : int64) (op2k : int) (op2v : int64)
              : unit =
	let e1 = E.mkOpExp op1 op1k op1v in
	let e2 = E.mkOpExp op2 op2k op2v in
	C.addState lhs (E.BinOp(lhsv, E.bop_of_int bop, e1, e2))

let assgn_uop (lhs : int64) (lhsv : int64) (uop : int)
              (op : int64) (opk : int) (opv : int64)
              : unit =
	let e = E.UnOp(lhsv, E.uop_of_int uop, E.mkOpExp op opk opv) in
	C.addState lhs e

let cond (cid : int) (r : int) (op : int64) (opk : int) (opv : int64) : unit =
	let op =
		if r = 1 then E.mkOpExp op opk opv |> C.simplifyExp
		else E.UnOp(Int64.one, E.LNot, E.mkOpExp op opk opv |> C.simplifyExp)
	in
	P.addPathCond cid r op

let cond_bop (cid : int) (bop : int) (r : int)
             (op1 : int64) (op1k : int) (op1v : int64)
             (op2 : int64) (op2k : int) (op2v : int64)
             : unit =
	let bop =
		if r = 1 then E.bop_of_int bop
		else bop |> E.bop_of_int |> E.opposite_test
	in
	let e1 = E.mkOpExp op1 op1k op1v |> C.simplifyExp in
	let e2 = E.mkOpExp op2 op2k op2v |> C.simplifyExp in
	let c = E.BinOp(Int64.of_int r, bop, e1, e2) in
	P.addPathCond cid r c

let cond_uop (cid : int) (uop : int) (r : int)
             (op : int64) (opk : int) (opv : int64)
             : unit =
	let e = E.mkOpExp op opk opv |> C.simplifyExp in
	let c = E.UnOp(Int64.of_int r, E.uop_of_int uop, e) in
	if r = 1 then P.addPathCond cid r c
	else P.addPathCond cid r (E.UnOp(Int64.one, E.LNot, c))

let push_val (n : string) (v : int64) : unit = C.storeVal n v
let pop_val (n : string) : int64 = C.getVal n

let register_input (name : string) (addr : int64) (bits : int) : unit =
	C.makeNewInput name addr bits

let gen_new_input () : int = P.pathsFindNext ()

let retval : E.exp ref = ref (E.Op(Int64.zero,(E.Address, Int64.zero)))

let return_push (p : int64) (v : int64) : unit =
	retval := (E.Op(v, (E.Address, p)))
let return_pop  (p : int64) (v : int64) : unit =
	match !retval with
	| E.Op(v', (E.Address, v'')) when v' = Int64.zero && v'' = Int64.zero ->
		C.addState p (E.Op(v,(E.Constant, v)))
	| e -> C.addState p e; retval := (E.Op(Int64.zero,(E.Address, Int64.zero)))

let autotest_reset() : unit =
	C.resetContext ();
	P.resetPaths ()

let _ =
	C.global_ctxt := Some (C.makeContext ());
	Printf.printf "Yices version = %s\n" (Y.version ());
	flush stdout;
	Callback.register "assign"         assign;
	Callback.register "assgn_bop"      assgn_bop;
	Callback.register "assgn_uop"      assgn_uop;
	Callback.register "cond"           cond;
	Callback.register "cond_bop"       cond_bop;
	Callback.register "cond_uop"       cond_uop;
	Callback.register "register_input" register_input;
	Callback.register "gen_new_input"  gen_new_input;
	Callback.register "push_val"       push_val;
	Callback.register "pop_val"        pop_val;
	Callback.register "return_push"    return_push;
	Callback.register "return_pop"     return_pop;
	Callback.register "autotest_reset" autotest_reset

