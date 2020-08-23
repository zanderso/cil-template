module F = Frontc
module C = Cil
module E = Errormsg

module O = Ciltutoptions

let parseOneFile (fname: string) : C.file =
  let cabs, cil = F.parse_with_cabs fname () in
  Rmtmps.removeUnusedTemps cil;
  cil

let outputFile (f : C.file) : unit =
  if !O.outFile <> "" then
    try
      let c = open_out !O.outFile in
      
      C.print_CIL_Input := false;
      Stats.time "printCIL" 
        (C.dumpFile (!C.printerForMaincil) c !O.outFile) f;
      close_out c
    with _ ->
      E.s (E.error "Couldn't open file %s" !O.outFile)

let processOneFile (cil: C.file) : unit =
  if !(O.enable_tut.(0)) then Tut0.tut0 cil;
  if !(O.enable_tut.(1)) then Tut1.tut1 cil;
  if !(O.enable_tut.(2)) then Tut2.tut2 ("foo","bar") cil;
  if !(O.enable_tut.(3)) then Tut3.tut3 cil;
  if !(O.enable_tut.(4)) then Tut4.tut4 cil;
  if !(O.enable_tut.(5)) then Tut5.tut5 cil;
  if !(O.enable_tut.(6)) then Tut6.tut6 cil;
  if !(O.enable_tut.(7)) then Tut7.tut7 cil;
  if !(O.enable_tut.(8)) then Tut8.tut8 cil;
  if !(O.enable_tut.(9)) then Tut9.tut9 cil;
  if !(O.enable_tut.(10)) then Tut10.tut10 cil;
  if !(O.enable_tut.(11)) then Tut11.tut11 cil;
  if !(O.enable_tut.(12)) then Tut12.tut12 cil;
  if !(O.enable_tut.(13)) then Tut13.tut13 cil;
  if !(O.enable_tut.(14)) then Tut14.tut14 cil;
  if !(O.enable_tut.(15)) then Tut15.tut15 cil;
  outputFile cil
;;

let main () =
  
  C.print_CIL_Input := true;

  
  C.insertImplicitCasts := false;

  
  C.lineLength := 100000;

  
  C.warnTruncate := false;

  
  E.colorFlag := true;

  
  Cabs2cil.doCollapseCallCast := true;

  let usageMsg = "Usage: ciltutcc [options] source-files" in
  Arg.parse (O.align ()) Ciloptions.recordFile usageMsg;

  Ciloptions.fileNames := List.rev !Ciloptions.fileNames;
  let files = List.map parseOneFile !Ciloptions.fileNames in
  let one =
    match files with
	  | [] -> E.s (E.error "No file names provided")
    | [o] -> o
    | _ -> Mergecil.merge files "stdout"
  in

  processOneFile one
;;  


begin 
  try 
    main () 
  with
  | F.CabsOnly -> ()
  | E.Error -> ()
end;
exit (if !E.hadErrors then 1 else 0)
