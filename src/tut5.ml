




open Cil 
open Pretty
open Tututil

module L = List
module E = Errormsg



let compinfoOfLval (blv : lval) : compinfo =
  match unrollType(typeOfLval blv) with
  | TComp (ci, _) -> ci
  | _ -> E.s(E.bug "Expected TComp for type of %a" d_lval blv)


let zeroPtr (fd : fundec) (blv : lval) : stmt list =
  [i2s (Set(blv, CastE(voidPtrType, zero), locUnknown))]


let rec zeroType (fd : fundec) (blv : lval) : stmt list =
  match unrollType(typeOfLval blv) with
  | TPtr   _ -> zeroPtr   fd blv
  | TArray _ -> zeroArray fd blv
  | TComp  _ -> zeroComp  fd blv
  |        _ -> []


and zeroComp (fd : fundec) (blv : lval) : stmt list =
  let ci = compinfoOfLval blv in
  let sl =
    ci.cfields
    |> L.map (zeroField fd blv)
    |> L.concat
  in
  if ci.cstruct then sl
  else if sl <> [] then [L.hd sl]
  else []


and zeroField (fd : fundec) (blv : lval) (fi : fieldinfo) : stmt list =
  zeroType fd (addOffsetLval (Field(fi,NoOffset)) blv)


and zeroArray (fd : fundec) (blv : lval) : stmt list =
  let i = makeTempVar fd intType in
  let inits = zeroType fd (addOffsetLval (Index(v2e i, NoOffset)) blv) in
  let first = addOffsetLval (Index(zero, NoOffset)) blv in
  Formatcil.cStmts
    "
      %l:i = sizeof(%l:arr) / sizeof(%l:first) - 1;
      while (%l:i >= 0) {
        { %S:inits }
        %l:i -= 1;
      }
    "
    (fun n t -> makeTempVar fd ~name:n t) locUnknown
    [ ("i",     Fl(var i));
      ("arr",   Fl blv);
      ("first", Fl first);
      ("inits", FS inits);]


let processFunction (fd : fundec) (loc : location) : unit =
  let ini_stmts =
    fd.slocals
    |> L.map var
    |> L.map (zeroType fd)
    |> L.concat
  in
  fd.sbody.bstmts <- ini_stmts @ fd.sbody.bstmts


let tut5 (f : file) : unit =
  iterGlobals f (onlyFunctions processFunction)




