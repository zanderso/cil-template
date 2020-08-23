




open Cil
open Pretty
open Tututil

module E = Errormsg
module L = List



let cacheReportStr = "cache_report"

let hasCacheReportAttrs : attributes -> bool = hasAttribute cacheReportStr
let isCacheReportType (t : typ) : bool = t |> typeAttrs |> hasCacheReportAttrs


let isCacheReportStmt (s : stmt) : block option =
  match s.skind with
  | If(CastE(t,z),b,_,_) when z = zero && isCacheReportType t -> Some b
  | Block b when hasCacheReportAttrs b.battrs -> Some b
  | _ -> None




type functions = {
  mutable cache_begin : varinfo;
  mutable cache_end   : varinfo;
}

let dummyVar = makeVarinfo false "_tut_foo" voidType

let cachefuns = {
  cache_begin = dummyVar;
  cache_end   = dummyVar;
}

let cache_begin_str = "tut_cache_begin"
let cache_end_str   = "tut_cache_end"

let cache_function_names = [
  cache_begin_str;
  cache_end_str;
]

let isCacheFun (name : string) : bool =
  L.mem name cache_function_names

let initCacheFunctions (f : file) : unit =
  let focf : string -> typ -> varinfo = findOrCreateFunc f in
  let cb_type = TFun(voidType, Some["f", charConstPtrType, [];
                                    "l", intType, [];],
                     false, [])
  in
  let ce_type = TFun(voidType, Some["f", charConstPtrType, [];
                                    "l", intType, [];],
                     false, [])
  in
  cachefuns.cache_begin <- focf cache_begin_str cb_type;
  cachefuns.cache_end   <- focf cache_end_str   ce_type



let makeCacheReportStmts (loc : location) : stmt * stmt =
  let f, l = mkString loc.file, integer loc.line in
  i2s (Call(None,v2e cachefuns.cache_begin, [f; l;], loc)),
  i2s (Call(None,v2e cachefuns.cache_end,   [f; l;], loc))


class cacheReportAdder = object(self)
  inherit nopCilVisitor

  method vstmt (s : stmt) =
    let action s =
      match isCacheReportStmt s with
      | Some b -> begin
        let bs, es = makeCacheReportStmts (get_stmtLoc s.skind) in
        let nb = mkBlock [bs; mkStmt(Block b); es] in
        s.skind <- Block nb;
        s
      end
      | None -> s
    in
    ChangeDoChildrenPost(s, action)
end


let tut10 (f : file) : unit =
  initCacheFunctions f;
  let vis = new cacheReportAdder in
  visitCilFile vis f


