open Concolic_util

module A = Array
module L = List

type opkind = Constant | Address

let opkind_of_int (i : int) : opkind =
	match i with
	| 0 -> Constant
	| 1 -> Address
	| _ -> failwith "Bad opkind"

let string_of_op (op : int64) (ok : opkind) : string =
	match ok with
	| Constant -> Int64.to_string op
	| Address  -> "["^(Int64.to_string op)^"]"

type bop =
	| Plus | Minus | Times | Div | Mod
	| Shiftl | Shiftrl | Shiftra
	| Lt | Gt | Le | Ge | Eq | Ne
	| BAnd | BXor | BOr
	| LAnd | LOr

let bop_table = [|
	Plus; Minus; Times; Div; Mod;
	Shiftl; Shiftrl; Shiftra;
	Lt; Gt; Le; Ge; Eq; Ne;
	BAnd; BXor; BOr;
	LAnd; LOr;
|]
let bop_of_int (b : int) : bop = bop_table.(b)
let int_of_bop (b : bop) : int = b |> array_mem bop_table |> L.hd

let string_of_bop (b : bop) : string =
	match b with
	| Plus     -> "+"  | Minus    -> "-"   | Times    -> "*"
	| Div      -> "/"  | Mod      -> "%"   | Shiftl   -> ">>"
	| Shiftrl  -> ">>" | Shiftra  -> ">>>" | Lt       -> "<"
	| Gt       -> ">"  | Le       -> "<="  | Ge       -> ">="
	| Eq       -> "==" | Ne       -> "!="  | BAnd     -> "&"
	| BXor     -> "^"  | BOr      -> "|"   | LAnd     -> "&&"
	| LOr      -> "||"

let apply_bop (b : bop) (o1 : int64) (o2 : int64) : int64 =
	match b with
	| Plus     -> Int64.add o1 o2
	| Minus    -> Int64.sub o1 o2
	| Times    -> Int64.mul o1 o2
	| Div      -> Int64.div o1 o2
	| Mod      -> Int64.rem o1 o2
	| Shiftl   -> Int64.shift_left o1 (Int64.to_int o2)
	| Shiftrl  -> Int64.shift_right_logical o1 (Int64.to_int o2)
	| Shiftra  -> Int64.shift_right o1 (Int64.to_int o2)
	| Lt       -> if o1 < o2  then Int64.one else Int64.zero
	| Gt       -> if o1 > o2  then Int64.one else Int64.zero
	| Le       -> if o1 <= o2 then Int64.one else Int64.zero
	| Ge       -> if o1 >= o2 then Int64.one else Int64.zero
	| Eq       -> if o1 = o2  then Int64.one else Int64.zero
	| Ne       -> if o1 <> o2 then Int64.one else Int64.zero
	| BAnd     -> Int64.logand o1 o2
	| BXor     -> Int64.logxor o1 o2
	| BOr      -> Int64.logor  o1 o2
	| LAnd     -> if o1 <> Int64.zero && o2 <> Int64.zero
	              then Int64.one else Int64.zero
	| LOr      -> if o1 <> Int64.zero || o2 <> Int64.zero
	              then Int64.one else Int64.zero

let opposite_test (b : bop) : bop =
	match b with
	| Lt -> Ge | Ge -> Lt | Le -> Gt | Gt -> Le | Eq -> Ne | Ne -> Eq
	| _ -> b

type uop =
	| Neg
	| BNot
	| LNot

let uop_table = [| Neg; BNot; LNot; |]
let uop_of_int (u : int) : uop = uop_table.(u)
let int_of_uop (u : uop) : int = u |> array_mem uop_table |> L.hd

let string_of_uop (u : uop) : string =
	match u with
	| Neg  -> "-" | BNot -> "~" | LNot -> "!"

let apply_uop (u : uop) (o : int64) : int64 =
	match u with
	| Neg  -> Int64.neg o | BNot -> Int64.lognot o
	| LNot -> if o = Int64.zero then Int64.one else Int64.zero


type operand = opkind * int64
type exp =
	| BinOp of int64 * bop * exp * exp
	| UnOp  of int64 * uop * exp
	| Op    of int64 * operand
	| Input of int64

let rec string_of_exp (e : exp) : string =
	let i64s = Int64.to_string in
	let sofe = string_of_exp in
	match e with
	| Input a       -> "Input("^(i64s a)^")"
	| Op(v,(ok, o)) -> "Op("^(i64s v)^", "^(string_of_op o ok)^")"
	| UnOp(v,u,e)   -> "UnOp("^(i64s v)^", "^(string_of_uop u)^", "^(sofe e)^")"
	| BinOp(v,b,e1,e2) ->
		"BinOp("^(i64s v)^", "^(string_of_bop b)^", "^(sofe e1)^", "^(sofe e2)^")"

let mkOpExp (op : int64) (opk : int) (opv : int64) : exp =
	Op(opv, (opkind_of_int opk, op))

