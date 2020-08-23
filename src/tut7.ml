




open Cil 
open Tututil

module E = Errormsg
module S = String
module L = List



type color =  Red | Blue | Green


let redStr   = "red"
let blueStr  = "blue"
let greenStr = "green"


let color_strings = [redStr; blueStr; greenStr;]


let string_of_color (c : color) : string =
  match c with
  | Red   -> redStr
  | Blue  -> blueStr
  | Green -> greenStr


let color_of_string (cs : string) : color =
  match S.lowercase cs with
  | s when s = redStr   -> Red
  | s when s = blueStr  -> Blue
  | s when s = greenStr -> Green
  | _ -> E.s(E.bug "Expected a color string, got: %s" cs)


let isColorType (cs : string) (t : typ) : bool =
  hasAttribute cs (typeAttrs t)

let isTypeColor (t : typ) (cs : string) : bool = isColorType cs t
let isRedType   : typ -> bool = isColorType redStr
let isBlueType  : typ -> bool = isColorType blueStr
let isGreenType : typ -> bool = isColorType greenStr


let colors_of_type (t : typ) : color list =
  color_strings
  |> L.filter (isTypeColor t)
  |> L.map color_of_string


type typecheck_result =
  | TypesOkay
  | TypesMismatch of typ * typ
  | ColorsMismatch of typ * typ


let rec colorTypesCompat (t1 : typ) (t2 :typ) : typecheck_result =
  let cl1 = colors_of_type t1 in
  let cl2 = colors_of_type t2 in
  if cl1 <> cl2 then ColorsMismatch(t1, t2) else begin
    match t1, t2 with
    | TVoid _, TVoid _ -> TypesOkay
    | TPtr(t1, _), TPtr(t2, _)
    | TArray(t1,_,_), TArray(t2,_,_) -> colorTypesCompat t1 t2
    | TFun _, TFun _ -> TypesOkay 
    
    | TInt(ik1, _), TInt(ik2, _) when ik1 = ik2 -> TypesOkay
    | TFloat(fk1, _), TFloat(fk2, _) when fk1 = fk2 -> TypesOkay
    | TFun _, TPtr(t2, _) -> colorTypesCompat t1 t2
    | TPtr(t1, _), TFun _ -> colorTypesCompat t1 t2
    | TNamed(ti1, _), _ -> colorTypesCompat ti1.ttype t2
    | _, TNamed(ti2, _) -> colorTypesCompat t1 ti2.ttype
    | TComp(ci1, _), TComp(ci2, _) when ci1.cname = ci2.cname -> TypesOkay
    | TEnum(ei1, _), TEnum(ei2, _) when ei1.ename = ei2.ename -> TypesOkay
    | TBuiltin_va_list _, TBuiltin_va_list _ -> TypesOkay
    
    
    | _, _ -> TypesMismatch(t1, t2)
  end


let warning_for_tcres (tcr : typecheck_result) : unit =
  match tcr with
  | TypesMismatch(t1, t2) ->
    E.warn "%a: type mismatch: %a <> %a" d_loc (!currentLoc) d_type t1 d_type t2
  | ColorsMismatch(t1, t2) ->
    E.warn "%a: color mismatch: %a <> %a" d_loc (!currentLoc) d_type t1 d_type t2
  | TypesOkay -> ()


class colorCheckVisitor = object(self)
  inherit nopCilVisitor

  method vinst (i : instr) =
    match i with
    | Set(lv, e, loc) ->
      let tcres = colorTypesCompat (typeOfLval lv) (typeOf e) in
      warning_for_tcres tcres;
      DoChildren
    | Call(rlvo, fe, args, loc) -> DoChildren 
    | _ -> DoChildren

  method vexpr (e : exp) =
    match e with
    | CastE(t, e) when not(isConstant e) ->
      let tcres = colorTypesCompat t (typeOf e) in
      warning_for_tcres tcres;
      DoChildren
    | _ -> DoChildren
end


let checkColorTypes (fd : fundec) (loc : location) : unit =
  let vis = new colorCheckVisitor in
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


let tut7 (f : file) : unit =
  iterGlobals f (onlyFunctions checkColorTypes);
	eraseColors f



