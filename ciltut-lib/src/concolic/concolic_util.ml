module A = Array
module L = List
module S = String

let (|>) (a : 'a) (f : 'a -> 'b) : 'b = f a

let forceOption (a : 'a option) : 'a =
	match a with
	| Some a -> a
	| None -> failwith "forceOption"

let int32_of_int64 (i : int64) : int32 = i |> Int64.to_int |> Int32.of_int

let string_of_bool_array (ba : bool array) : string =
	let bs =
		ba
		|> A.to_list
		|> L.rev
		|> L.map (fun b -> if b then "1" else "0")
		|> S.concat ""
	in
	"0b"^bs

let int64_of_bool_array (ba : bool array) : int64 =
	ba |> string_of_bool_array |> Int64.of_string

let array_mem (a : 'a array) (x : 'a) : int list =
	let res = ref [] in
	A.iteri (fun i y -> if x = y then res := i :: (!res)) a;
	!res

let array_find (f : 'a -> bool) (a : 'a array) : int list =
	let res = ref [] in
	A.iteri (fun i x -> if f x then res := i :: (!res)) a;
	!res

