




open Cil
open Pretty
open Tututil

module E = Errormsg
module L = List
module A = Array
module S = String



type argument = {
  mutable argName   : string;  
  mutable argType   : typ;     
  mutable argShort  : string;  
  mutable argHelp   : string;  
  mutable argFmt    : string;  
  mutable argDef    : exp;     
  mutable argReq    : exp;     
  mutable argOpt    : bool;     
  mutable argVi     : varinfo; 
  mutable argGot    : varinfo; 
  mutable argOption : bool;    
}


let dummyVar = makeVarinfo false "_tut_foo" voidType



let makeArgument () =  

  {argName   = "";
   argType   = intType;
   argShort  = "";
   argHelp   = "";
   argFmt    = "%d";
   argDef    = zero;
   argReq    = one;
   argOpt    = true;
   argVi     = dummyVar;
   argGot    = dummyVar;
   argOption = false;}





let argStr       = "ciltutarg"
let assertStr    = "ciltut_assert"
let mandatoryStr = "mandatory"
let attr_strings = [argStr; assertStr; mandatoryStr;]


let hasArgAttr       : attributes -> bool = hasAttribute argStr
let hasAssertAttr    : attributes -> bool = hasAttribute assertStr
let hasMandatoryAttr : attributes -> bool = hasAttribute mandatoryStr


let isArgType       (t : typ) : bool = t |> typeAttrs |> hasArgAttr
let isMandatoryType (t : typ) : bool = t |> typeAttrs |> hasMandatoryAttr


let getAssertAttr (t : typ) : attrparam =
  match filterAttributes assertStr (typeAttrs t) with
  | [Attr(_, [ap])] -> ap
  | _ -> E.s (E.error "Malformed %s attribute: %a" assertStr d_type t)


let rec string_of_exp (e : exp) : string =
  match e with
  | Const(CStr s) -> s
  | z when z = zero -> ""
  | CastE(_,e) -> string_of_exp e
  | _ -> E.s (E.error "Expected string literal: %a" d_exp e)


let name_of_argname (s : string) : string =
  if S.length s < 8 then
    E.s (E.error "Invalid argument name: %s" s);
  S.sub s 8 (S.length s - 8)


module T = Tut8

let req_of_exp (c : T.ctxt) (loc : location) (e : exp) : exp =
  match e with
  | CastE(t, z) when z = zero ->
    t |> getAssertAttr |> T.exp_of_ap c loc
  | _ -> one


let handle_field (c : T.ctxt) (loc : location) (a : argument)
                 (off : offset) (ini : init) (t : typ) ()
                 : unit
  =
  match off, ini with
  | Field(f, NoOffset), SingleInit e -> begin
    match f.fname with
    | "short_form" -> a.argShort  <- string_of_exp e
    | "help_text"  -> a.argHelp   <- string_of_exp e
    | "def"        -> a.argDef    <- e;
                      a.argType   <- f.ftype;
                      a.argVi     <- makeGlobalVar a.argName a.argType
    | "requires"   -> a.argReq    <- req_of_exp c loc e
    | "format"     -> a.argFmt    <- string_of_exp e
    | "has_opt"    -> a.argOption <- e = one
    | _            -> E.s(E.bug "malformed arg struct")
  end
  | _ -> E.s(E.bug "Unexpected initializer in argument_of_global")


let argument_of_global (c : T.ctxt) (g : global) : argument list =
  match g with
  | GVar(vi, {init = Some(CompoundInit(t,ini))}, loc)
    when isArgType vi.vtype -> begin
    let a = makeArgument () in
    a.argName <- name_of_argname vi.vname;
    a.argGot  <- makeGlobalVar (a.argName^"got") intType;
    a.argOpt  <- not(isMandatoryType vi.vtype);
    iterCompound ~implicit:false ~doinit:(handle_field c loc a)
                 ~ct:vi.vtype    ~initl:ini;
    [a]
  end
  | _ -> []


let gatherArguments (f : file) : argument list =
  let c = T.context_for_globals f in
  f.globals
  |> L.map (argument_of_global c)
  |> L.concat


let field_of_option (o : varinfo) (ot : typ) (i : int) (n : string) : lval =
  (Var o),
  Index(integer i, Field(fieldinfo_of_name ot n, NoOffset))


let has_arg_of_argument (a : argument) : exp = 
  if a.argOption then one else zero
let int_code_of_argument (a : argument) : exp =
  a.argShort.[0] |> int_of_char |> integer


let initialize_options (foo : int -> string -> lval)
                       (i : int) (a : argument)
                       : instr list
  =
  [Set(foo i "name",    mkString a.argName,     locUnknown);
   Set(foo i "has_arg", has_arg_of_argument a,  locUnknown);
   Set(foo i "val",     int_code_of_argument a, locUnknown)]


let create_long_options (f : file) (main : fundec) (al : argument list)
                        : varinfo * instr * instr list
  =
  let malloc = findOrCreateFunc f "malloc" (mallocType f) in
  let ot     = findType f.globals "option" in
  let o      = makeTempVar main (TPtr(ot, [])) in
  let foo    = field_of_option o ot in
  let size   = integer((L.length al + 1) * ((bitsSizeOf ot)/8)) in
  let mcall  = Call(Some(var o), v2e malloc, [size], locUnknown) in
  let inits  = al |> A.of_list
                  |> A.mapi (initialize_options foo)
                  |> A.to_list
                  |> L.concat
  in
  o, mcall, inits


let create_short_options (al : argument list) : exp =
  let short_arg_of_arg (a : argument) : string =
    a.argShort^(if a.argOption then ":" else "")
  in
  al |> L.map short_arg_of_arg
     |> S.concat ""
     |> mkString


let getMainArgs (main : fundec) : varinfo * varinfo =
  match main.sformals with
  | argc :: argv :: _ -> argc, argv
  | _ -> E.s (E.error "Must give main argc and argv")


let string_of_short_arg (a : argument) : string =
  a.argShort.[0] |> int_of_char |> string_of_int
let string_of_arg_opt (a : argument) : string =
  if a.argOption then "1" else "0"


let create_def_int_string (a : argument) : string =
  if isIntegralType a.argType && not(a.argOption)
  then "else {%l:"^a.argVi.vname^"l = 1;}"
  else ""


let create_if_str (a : argument) : string =
  "if (c == "^(string_of_short_arg a)^") {"^
     "if ("^(string_of_arg_opt a)^") { if(%e:optarg) {"^
       "%l:scan(%e:optarg,%e:"^a.argVi.vname^"fmt,%e:"^a.argVi.vname^"addr);"^
     "}}"^
    (create_def_int_string a)^
   "%l:"^a.argGot.vname^" = 1;"^
   "}"


let create_opt_loop_str (al : argument list) : string =
  "while(1) {"^
     "int c;"^
     "c = %l:gol(%e:argc, %e:argv, %e:sstr, %e:lopts, (void * )0);"^
     "if (c == -1) break;"^
     (al |> L.map create_if_str |> S.concat " else ")^
   "}"


let makeArgStmts (f : file) (main : fundec) (al : argument list) : stmt list =
  let gol        = findOrCreateFunc f "getopt_long" intType in
  let scan       = findOrCreateFunc f "sscanf" intType in
  let optarg     = findGlobalVar f.globals "optarg" in
  let so         = create_short_options al in
  let o, m, i    = create_long_options f main al in
  let argc, argv = getMainArgs main in
  (L.map i2s (m :: i)) @
  Formatcil.cStmts (create_opt_loop_str al)
   (fun n t -> makeTempVar main ~name:n t) locUnknown
   ([("argc",  Fe(v2e argc));
     ("argv",  Fe(v2e argv));
     ("sstr",  Fe so);
     ("lopts", Fe (v2e o));
     ("gol",   Fl(var gol));
     ("scan",  Fl(var scan));
     ("optarg",Fe(v2e optarg))]@
     (L.map (fun a -> (a.argVi.vname^"fmt"), Fe (mkString a.argFmt)) al)@
     (L.map (fun a -> (a.argVi.vname^"addr"),Fe (AddrOf(var a.argVi))) al)@
     (L.map (fun a -> (a.argVi.vname^"l"),   Fl(var a.argVi)) al)@
     (L.map (fun a -> (a.argGot.vname),      Fl(var a.argGot)) al))


let initArgs (al : argument list) : stmt list =
  L.map (fun a -> i2s(Set(var a.argVi, a.argDef, locUnknown))) al


let printHelp (f : file) (al : argument list) : stmt list =
  let print = findOrCreateFunc f "printf" intType in
  let s s   = mkString s in
  [i2s(Call(None,v2e print,[s "Improper arguemnts\n"],locUnknown))]@
  (L.map (fun a ->
    let af = if a.argFmt <> "" then a.argFmt else "%d" in
    let fmt = if a.argOpt
      then s ("\t-%s,--%s\t%s ("^af^")\n")
      else s "\t-%s,--%s\t%s (mandatory)\n"
    in
    let args = if a.argOpt
      then [fmt;s a.argShort;s a.argName;s a.argHelp;a.argDef]
      else [fmt;s a.argShort;s a.argName;s a.argHelp]
    in
    i2s (Call(None,v2e print,args,locUnknown))
  ) al) @
  [mkStmt (Return(Some mone, locUnknown))]


let makeArgChecks (f : file) (al : argument list) : stmt =
  let got_arg a = if a.argOpt then one else v2e a.argGot in
  let bexp, mexp =
    L.fold_left (fun b a -> BinOp(LAnd, b, a.argReq, intType)) one al,
    L.fold_left (fun b a -> BinOp(LAnd, b, (got_arg a), intType)) one al
  in
  mkStmt (If(BinOp(LOr,UnOp(LNot,mexp,intType),UnOp(LNot,bexp,intType),intType),
             mkBlock (printHelp f al), mkBlock[],
             locUnknown))


let processFunction (f : file) (al : argument list)
                    (fd : fundec) (loc : location) : unit =
  if fd.svar.vname = "main" then
    fd.sbody.bstmts <- (initArgs al)          @
                       (makeArgStmts f fd al) @
                       [makeArgChecks f al]   @
                       fd.sbody.bstmts


class attrEraserVisitor = object(self)
  inherit nopCilVisitor

  method vattr (a : attribute) =
    match a with
    | Attr(s,_) when L.mem s attr_strings -> ChangeTo []
    | _ -> DoChildren

end

let eraseAttrs (f : file) : unit =
  let vis = new attrEraserVisitor in
  visitCilFile vis f



let tut14 (f : file) : unit =
  f |> gatherArguments
    |> processFunction f
    |> onlyFunctions
    |> iterGlobals f;
  eraseAttrs f


