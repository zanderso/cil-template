




open Cil 
open Pretty
open Tututil
module L = List



type functions = {
  mutable begin_loop : varinfo;
  mutable end_loop   : varinfo;
}


let dummyVar = makeVarinfo false "_tut_foo" voidType


let tutfuns = {
  begin_loop = dummyVar;
  end_loop   = dummyVar;
}


let begin_loop_str = "tut_begin_loop"
let end_loop_str   = "tut_end_loop"


let tut_function_names = [
  begin_loop_str;
  end_loop_str;
]

let isTutFun (name : string) : bool =
  L.mem name tut_function_names


let mkFunTyp (rt : typ) (args : (string * typ) list) : typ =
  TFun(rt, Some(L.map (fun a -> (fst a, snd a, [])) args), false, [])


let initTutFunctions (f : file) : unit =
  let focf : string -> typ -> varinfo = findOrCreateFunc f in
  let bl_type = mkFunTyp voidType ["f", charConstPtrType; "l", intType] in
  let el_type = mkFunTyp voidType ["f", charConstPtrType; "l", intType; "c", intType;] in
  tutfuns.begin_loop <- focf begin_loop_str bl_type;
  tutfuns.end_loop   <- focf end_loop_str   el_type


let makeInstrStmts (counter : varinfo) (loc : location)
                   : stmt * stmt * stmt * stmt =
  let f, l = mkString loc.file, integer loc.line in
  i2s (Call(None, v2e tutfuns.begin_loop, [f; l], loc)),
  i2s (Set(var counter, zero, loc)),
  i2s (Set(var counter,BinOp(PlusA, v2e counter, one, counter.vtype), loc)),
  i2s (Call(None, v2e tutfuns.end_loop, [f; l; v2e counter], loc))



class loopInstrumenterClass (fd : fundec) = object(self)
  inherit nopCilVisitor

  method vstmt (s : stmt) =
    let action s =
      match s.skind with
      | Loop(b, loc, co, bo) ->
        let counter = makeTempVar fd intType in
        let ss, cis, is, es = makeInstrStmts counter loc in
        b.bstmts <- is :: b.bstmts;
        let nb = mkBlock [ss; cis; mkStmt s.skind; es] in
        s.skind <- Block nb;
        s
      | _ -> s
    in
    ChangeDoChildrenPost(s, action)

end


let processFunction (fd : fundec) (loc : location) : unit =
  let vis = new loopInstrumenterClass fd in
  ignore(visitCilFunction vis fd)


let tut4 (f : file) : unit =
  initTutFunctions f;
  iterGlobals f (onlyFunctions processFunction)





