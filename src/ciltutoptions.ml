module C = Cil

open Printf
open Tututil


let size_t: string ref = ref ""
let outFile : string ref = ref ""
let debug : bool ref = ref false
let verbose : bool ref = ref false
let stats: bool ref = ref false
let parseFile : string ref = ref ""
let warnAsm: bool ref = ref false
let warnVararg: bool ref = ref false
let home : string ref = ref ""
let merge : bool ref = ref false

let num_tuts = 16
let enable_tut : bool ref array = Array.init num_tuts (fun i -> ref false)

let prover : string ref = ref "Alt-Ergo"
let prover_version : string ref = ref "0.94"
let tut13out : string ref = ref "callgraph.dot"

let options_ref = ref []

let align () =
  let options = !options_ref in
  
  let left = try
      options
      |> List.map fst3
      |> List.map String.length
      |> List.sort (sargs compare)
      |> List.hd
    with Not_found -> 0
  in
  
  let left = left + 4 in
  
  let width = 78 - left in
  
  let rec wrap str =
    if String.length str <= width then str else
    
    let break, skip =
      try let break = String.rindex_from str width ' ' in
        try String.index (String.sub str 0 break) '\n', 1
        with Not_found -> break, 1
      with Not_found -> width, 0
    in
    
    let lstr, rstr =
      String.sub str 0 break,
      String.sub str (break + skip) (String.length str - break - skip)
    in
    lstr ^ "\n" ^ String.make left ' ' ^ wrap rstr
  in
  
  List.map (fun (arg, action, str) ->
    if arg = "" then arg, action, "\n" ^ str ^ "\n"
    else let pre = String.make (left - String.length arg - 3) ' ' in
    arg, action, pre ^ wrap str)
  options

let tut_options =
  Array.to_list (
  Array.mapi (fun i br ->
    ("--enable-tut"^(string_of_int i),
     Arg.Set br,
     "Enable the code in tut"^(string_of_int i)^".ml")
  ) enable_tut)

let options = tut_options @ [

  
  ("--prover",
   Arg.Set_string prover,
   "The prover that Why3 should use in Tut11 [default: Alt-Ergo]");
  ("--prover-version",
   Arg.Set_string prover_version,
   "The version for the prover that Why3 should use in Tut11 [default: 0.94]");


  
  ("--tut13-out",
   Arg.Set_string tut13out,
   "The output dot file for tut13");

  
  "", Arg.Unit (fun () -> ()), "General:";
  "--out", Arg.Set_string outFile, "Set the name of the output file";
  "--home", Arg.Set_string home, "Set the name of ciltut's home directory";
  "--verbose", Arg.Set verbose,
    "Enable verbose output";
  "--stats", Arg.Set stats,
    "Output optimizer execution time stats";
  "--help", Arg.Unit (fun () -> Arg.usage (align ()) ""; exit 0),
    "Show this help message";
  "--merge", Arg.Set merge,
    "Operate in CIL merger mode";
   "--envmachine",
   Arg.Unit (fun _ ->
     try
       let machineModel = Sys.getenv "CIL_MACHINE" in
       Cil.envMachine := Some (Machdepenv.modelParse machineModel);
     with 
       Not_found ->
	 ignore (Errormsg.error "CIL_MACHINE environment variable is not set")
     | Failure msg ->
	 ignore (Errormsg.error "CIL_MACHINE machine model is invalid: %s" msg)),
   "Use machine model specified in CIL_MACHINE environment variable";
]

let _ = options_ref := options;;

