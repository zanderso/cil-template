




open Cil 
open Pretty
open Tututil

module E = Errormsg
module L = List
module S = String



module SM = Map.Make(struct
  type t = string
  let compare = Pervasives.compare
end)


type rgb = exp * exp * exp


type color =
  | ExactRGB of rgb
  | LowerRGB of rgb
  | UpperRGB of rgb

type colors = color list


let exactRGBStr = "ExactRGB"
let lowerRGBStr = "LowerRGB"
let upperRGBStr = "UpperRGB"

let color_strings = [exactRGBStr; lowerRGBStr; upperRGBStr;]


let rgb_of_color (c : color) : rgb =
  match c with
  | ExactRGB(r,g,b)
  | LowerRGB(r,g,b)
  | UpperRGB(r,g,b) -> r, g, b


let string_of_rgb (t : rgb) : string =
  let t =
    t |> triplemap (d_exp ())
      |> triplemap (sprint ~width:80)
  in
  "("^(fst3 t)^", "^(snd3 t)^", "^(thd3 t)^")"


let string_of_color (c : color) : string =
  let k =
    match c with
    | ExactRGB _ -> exactRGBStr
    | LowerRGB _ -> lowerRGBStr
    | UpperRGB _ -> upperRGBStr
  in
  k^(c |> rgb_of_color |> string_of_rgb)


let string_of_colors (c : colors) : string =
  c
  |> L.map string_of_color
  |> S.concat ", "


type ctxt = exp SM.t

let exp_of_string  (c : ctxt) (s : string)   : exp  = SM.find s c
let ctxt_add_var   (c : ctxt) (vi : varinfo) : ctxt = SM.add vi.vname (v2e vi) c
let ctxt_add_exp   (c : ctxt) (s : string) (e : exp) : ctxt = SM.add s e c
let ctxt_add_field (blv : lval) (c : ctxt) (fi : fieldinfo) : ctxt =
  SM.add fi.fname (Lval(addOffsetLval (Field(fi,NoOffset)) blv)) c


let context_for_globals (f : file) : ctxt =
  L.fold_left (fun c g ->
    match g with
    | GVarDecl(vi, _) -> ctxt_add_var c vi
    | GVar(vi, _, _)  -> ctxt_add_var c vi
    | _ -> c)
  SM.empty f.globals


let context_for_locals (c : ctxt) (fd : fundec) : ctxt =
  L.fold_left ctxt_add_var c (fd.slocals @ fd.sformals)


let context_for_call (c : ctxt) (fe : exp) (args : exp list) : ctxt =
  match typeOf fe with
  | TFun(_, Some stal, _, _) ->
    let formals = L.map fst3 stal in
    let actuals = list_take (L.length stal) args in
    L.fold_left2 ctxt_add_exp c formals actuals
  | _ -> c


let context_for_struct (c : ctxt) (loc : location) (lv : lval) : ctxt =
  let blv, off = removeOffsetLval lv in
  match off with
  | NoOffset | Index _ -> c
  | Field(fi, NoOffset) -> L.fold_left (ctxt_add_field blv) c fi.fcomp.cfields
  | _ -> E.s(E.bug "%a: Expected field w/o offset: %a" d_loc loc d_lval lv)


let rec exp_of_ap (c : ctxt) (loc : location) (ap : attrparam) : exp =
  let eoap = exp_of_ap c loc in
  match ap with
  | AInt i -> integer i
  | AStr s -> mkString s
  | ACons(s, []) -> begin
    try exp_of_string c s
    with Not_found ->
      E.s (E.error "%a: %s not in context for %a"
             d_loc loc s d_attrparam ap)
  end
  | AUnOp(uop,ap) ->
    let e = eoap ap in
    UnOp(uop, e, typeOf e)
  
  | ASizeOf typ -> SizeOf typ
  | ASizeOfE ap -> SizeOfE (eoap ap)
  | AAlignOf typ -> AlignOf typ
  | AAlignOfE ap -> AlignOfE (eoap ap)
  | ABinOp(bop, ap1, ap2) -> begin
    let e1 = eoap ap1 in
    let e2 = eoap ap2 in
    
    match bop, unrollType (typeOf e1) with
    | PlusA, (TPtr _ | TArray _) -> BinOp(PlusPI, e1, e2, typeOf e1)
    | _, _ -> BinOp(bop, e1, e2, typeOf e1)
  end
  | ADot(ap, s) -> begin
    let e  = eoap ap in
    let fi = fieldinfo_of_name (typeOf e) s in
    match e with
    | Lval(lh, off) ->
      let newoff = Field(fi,NoOffset) in
      if isArrayType fi.ftype then
      StartOf(lh, addOffset newoff off)
      else Lval(lh, addOffset newoff off)
    | _ ->
      E.s (E.error "%a: exp_of_ap: Base expression not an l-value: %a"
        d_loc loc d_attrparam ap)
  end
  | AStar ap -> begin
    let e = eoap ap in
    match typeOf e with
    | TPtr _ -> Lval(Mem e, NoOffset)
    | _ ->
      E.s (E.error "%a: exp_of_ap: Base expression not of pointer type: %a"
        d_loc loc d_attrparam ap)
  end
  | AAddrOf aap -> begin
    let e = eoap aap in
    match e with
    | Lval lv -> AddrOf lv
    | _ ->
      E.s (E.error "%a: exp_of_ap: Base expression not an l-value: %a"
        d_loc loc d_attrparam ap)
  end
  | AIndex(bap,iap) -> begin
    let be = eoap bap in
    let ie = eoap iap in
    match be with
    | Lval(lh,off) ->
      let newoff = Index(ie,NoOffset) in
      Lval(lh, addOffset newoff off)
    | StartOf(lh, off) ->
      let newoff = Index(ie, NoOffset) in
      Lval(lh, addOffset newoff off)
    | _ ->
      E.s (E.error "%a: exp_of_ap: Base expression not an array: %a"
        d_loc loc d_attrparam ap)
  end
  
  
  | _ ->
    E.s (E.error "%a: exp_of_ap: Attribute parameter is not an expression: %a"
      d_loc loc d_attrparam ap)


let make_colorqual (k : string) (loc : location) (et : rgb) : color =
  match k with
  | s when s = exactRGBStr -> ExactRGB et
  | s when s = lowerRGBStr -> LowerRGB et
  | s when s = upperRGBStr -> UpperRGB et
  | _ -> E.s (E.bug "%a: Expected an RGBStr got %s" d_loc loc k)


let colorqual_of_type (k : string) (c : ctxt) (loc : location) (t : typ)
                      : colors
  = 
  match filterAttributes k (typeAttrs t) with
  | (Attr(_,[rap; gap; bap])) :: rst->
    if rst <> [] then
      E.warn "%a: Type with multiple %s qualifiers. Keeping only the first: %a"
        d_loc loc k d_type t;
    [(rap, gap, bap)
     |> triplemap (exp_of_ap c loc)
     |> make_colorqual k loc]
  | (Attr _) :: _ ->
    E.warn "%a: Malformed color attribute: %a"
      d_loc loc d_type t;
    []
  | _ -> []


let exactRGB_of_type = colorqual_of_type exactRGBStr
let lowerRGB_of_type = colorqual_of_type lowerRGBStr
let upperRGB_of_type = colorqual_of_type upperRGBStr


let colors_of_type (c : ctxt) (loc : location) (t : typ) : colors =
  let exact = exactRGB_of_type c loc t in
  let lower = lowerRGB_of_type c loc t in
  let upper = upperRGB_of_type c loc t in
  match exact, lower, upper with
  | e, [], [] -> e
  | [], l, u  -> l @ u
  | _         ->
    E.error ("%a: At most one exact, or one upper"^^
             "and one lower bound allowed: %a")
      d_loc (!currentLoc) d_type t;
    []




type color_checks = {
  mutable color_eq : varinfo;
  mutable color_le : varinfo;
}

let dummyVar = makeVarinfo false "_tut_foo" voidType

let color_funcs = {
  color_eq = dummyVar;
  color_le = dummyVar;
}

let color_eq_str = "tut_color_eq"
let color_le_str = "tut_color_le"

let color_function_names = [
  color_eq_str;
  color_le_str;
]

let initColorFunctions (f : file) : unit = 
  let focf : string -> typ -> varinfo = findOrCreateFunc f in
  let eqtyp =
    TFun(voidType, Some["r1",intType,[]; "g1",intType,[]; "b1",intType,[];
                        "r2",intType,[]; "g2",intType,[]; "b2",intType,[];
                        "f",charConstPtrType,[]; "l",intType,[];],
         false, [])
  in
  color_funcs.color_eq <- focf color_eq_str eqtyp;
  color_funcs.color_le <- focf color_le_str eqtyp

let mkColorInst (vi : varinfo) (loc : location) (c1 : rgb) (c2 : rgb) : instr =
  let f, l = mkString loc.file, integer loc.line in
  Call(None, v2e vi,
      [fst3 c1; snd3 c1; thd3 c1; fst3 c2; snd3 c2; thd3 c2; f; l], loc)


let mkColorEqInst () = mkColorInst color_funcs.color_eq
let mkColorLeInst () = mkColorInst color_funcs.color_le


let color_includes (loc : location)
                   (is_this : color) (in_this : color)
                   : instr list
  =
  match is_this, in_this with
  | ExactRGB c1, ExactRGB c2 -> [mkColorEqInst () loc c1 c2]
  | ExactRGB c1, LowerRGB c2
  | LowerRGB c1, LowerRGB c2 -> [mkColorLeInst () loc c2 c1]
  | ExactRGB c1, UpperRGB c2
  | UpperRGB c1, UpperRGB c2 -> [mkColorLeInst () loc c1 c2]
  | _ -> E.error "%a: color inclusion test will always fail" d_loc (!currentLoc);
         []


let colors_includes (loc : location)
                    (is_this : colors) (in_this : colors)
                    : instr list
  =
  if (is_this = [] && in_this <> []) || (is_this <> [] && in_this = []) then
    
    E.error "%a: color mismatch" d_loc loc;
  L.concat (
    L.map (fun c1 ->
      L.concat(L.map (color_includes loc c1) in_this)
    ) is_this
  )


let colors_of_lval (c : ctxt) (loc : location) (lv : lval) : colors =
  colors_of_type (context_for_struct c loc lv) loc (typeOfLval lv)

let colors_of_exp (c : ctxt) (loc : location) (e : exp) : colors =
  match e with
  | Lval lv -> colors_of_lval c loc lv
  | _       -> colors_of_type c loc (typeOf e)


class colorCheckVisitor (c : ctxt) = object(self)
  inherit nopCilVisitor

  method vinst (i : instr) =
    match i with
    | Set(lv, e, loc) ->
      let lvc = colors_of_lval c loc lv in
      let ec  = colors_of_exp  c loc e  in
      self#queueInstr (colors_includes loc ec lvc);
      DoChildren
    
    | Call(rlvo, fe, args, loc) ->
      let rt, stal = function_elements fe in
      let cc = context_for_call c fe args in
      L.iter2 (fun at a ->
        let atc = colors_of_type cc loc at in
        let ac  = colors_of_exp c loc a in
        self#queueInstr (colors_includes loc ac atc)
      ) (L.map snd3 stal) (list_take (L.length stal) args);
      begin      
        match rlvo with
        | Some lv ->
          let lvc = colors_of_lval c loc lv in
          let rc  = colors_of_type c loc rt in
          self#queueInstr (colors_includes loc rc lvc)
        | None -> ()
      end;
      DoChildren
    
    | _ -> DoChildren

end


let checkColorTypes (c : ctxt) (fd : fundec) (loc : location) : unit =
  let c = context_for_locals c fd in
  let vis = new colorCheckVisitor c in
  ignore(visitCilFunction vis fd)


class colorEraserVisitor = object(self)
  inherit nopCilVisitor

  method vattr (a : attribute) =
    match a with
    | Attr(s,_) when L.mem s color_strings -> ChangeTo []
    | _ -> DoChildren

end

let eraseColors (f : file) : unit =
  let vis = new colorEraserVisitor in
  visitCilFile vis f

let tut8_init (f : file) : unit =
  initColorFunctions f



let tut8 (f : file) : unit =
  tut8_init f;
  let c = context_for_globals f in
  c |> checkColorTypes |> onlyFunctions |> iterGlobals f;
  eraseColors f




