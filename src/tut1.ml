



open Cil
module E = Errormsg


let tut1FixInstr (i : instr) : bool =
  match i with
  | Set((Var vi, NoOffset), _, loc)
      when vi.vname = "deleted" && vi.vglob ->
    E.log "%a: Deleted assignment: %a\n" d_loc loc d_instr i;
    false
  | _ -> true


let rec tut1FixStmt (s : stmt) : unit =
  match s.skind with
  | Instr il ->
    s.skind <- Instr(List.filter tut1FixInstr il)
  | If(_,tb,fb,_) ->
    tut1FixBlock tb;
    tut1FixBlock fb
  

  | Switch(_,b,_,_) ->
    tut1FixBlock b
  | Loop(b,_,_,_) ->
    tut1FixBlock b
  | Block b ->
    tut1FixBlock b
  | TryFinally(b1, b2, _) ->
    tut1FixBlock b1;
    tut1FixBlock b2
  | TryExcept(b1,_,b2,_) ->
    tut1FixBlock b1;
    tut1FixBlock b2

  | _ -> ()

and tut1FixBlock (b : block) : unit = List.iter tut1FixStmt b.bstmts

let tut1FixFunction (fd : fundec) : unit = tut1FixBlock fd.sbody


let tut1 (f : file) : unit =
  List.iter (fun g ->
    match g with
    | GFun (fd, loc) when fd.svar.vname = "target" ->
      tut1FixFunction fd
    | _ -> ())
  f.globals



