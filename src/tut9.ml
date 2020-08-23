




open Cil
open Pretty
open Tututil

module E  = Errormsg
module L  = List
module S  = String
module A  = Array



module Q  = Queue
module T = Tut7
type colors = T.color list


let nodeStr = "Node"
let nodeAttr (id : int) : attributes = [Attr(nodeStr,[AInt id])]


let node_of_type (t : typ) : int =
  match filterAttributes nodeStr (typeAttrs t) with
  | [(Attr(_, [AInt id]))] -> id
  | [] -> 0
  | _ -> E.s (E.bug "%a: Malformed node id on %a" d_loc (!currentLoc) d_type t)


class typeNodeMarker (node_count : int ref) = object(self)
  inherit nopCilVisitor

  method vtype (t : typ) =
    let action t =
      if not(hasAttribute nodeStr (typeAttrs t)) then begin
        let attr = nodeAttr (!node_count) in
        incr node_count;
        typeAddAttributes attr t
      end else t
    in
    ChangeDoChildrenPost(t, action)
end


let addNodeMarks (f : file) : int =
  let cntr = ref 1 in
  let vis = new typeNodeMarker cntr in
  visitCilFile vis f;
  !cntr


class typeNodeEraser = object(self)
  inherit nopCilVisitor

  method vattr (a : attribute) =
    match a with
    | Attr(s,_) when s = nodeStr -> ChangeTo []
    | _ -> DoChildren
end

let eraseNodeMarks (f : file) : unit =
  let vis = new typeNodeEraser in
  visitCilFile vis f



type node = {
  mutable ncolors   : colors;
  mutable incoming  : int list;
  mutable outgoing  : int list;
}


let newNode (id : int) : node =
  {ncolors = []; incoming = []; outgoing = []}


type graph = node array


let graphCreate (n : int) : graph = A.init n newNode


let graphAddEdge (g : graph) (from_node : int) (to_node : int) : unit =
  if not(L.mem to_node g.(from_node).outgoing) then
    g.(from_node).outgoing <- to_node :: g.(from_node).outgoing;
  if not(L.mem from_node g.(to_node).incoming) then
    g.(to_node).incoming <- from_node :: g.(to_node).incoming


let rec typesAddEdge (g : graph) (from_type : typ) (to_type : typ) : unit =
  let from_id = node_of_type from_type in
  let to_id   = node_of_type to_type   in
  graphAddEdge g from_id to_id


let addEdgesForCallArgs (g : graph) (fe : exp) (aes  : exp list) : unit =
  let fts = fe |> function_elements |> snd |> L.map snd3 in
  let ats = aes |> list_take (L.length fts) |> L.map typeOf in
  L.iter2 (typesAddEdge g) ats fts


let addEdgesForCallRet (g : graph) (fe : exp) (rlvo : lval option) : unit =
  match rlvo with
  | None -> ()
  | Some rlv ->
    let rt, _ = function_elements fe in
    typesAddEdge g rt (typeOfLval rlv)


class graphBuilder (g : graph) (fd : fundec) = object(self)
  inherit nopCilVisitor

  method vinst (i : instr) =
    match i with
    | Set(lv, e, loc) ->
      typesAddEdge g (typeOf e) (typeOfLval lv);
      DoChildren
  
  
    | Call(rlvo, fe, aes, loc) ->
      addEdgesForCallArgs g fe aes;
      addEdgesForCallRet  g fe rlvo;
      DoChildren
    | _ -> DoChildren

  method vexpr (e : exp) =
    match e with
    | CastE(t, e) ->
      typesAddEdge g (typeOf e) t;
      DoChildren
    | _ -> DoChildren

  method vstmt (s : stmt) =
    match s.skind with
    | Return(Some e, _) ->
      let rt = fd.svar |> v2e |> function_elements |> fst in
      typesAddEdge g (typeOf e) rt;
      DoChildren
    | _ -> DoChildren
  
end


let functionBuildGraph (g : graph) (fd : fundec) (loc : location) : unit =
  let vis = new graphBuilder g fd in
  ignore(visitCilFunction vis fd)


let fileBuildGraph (f : file) : graph =
  let g = f |> addNodeMarks |> graphCreate in
  functionBuildGraph g
  |> onlyFunctions
  |> iterGlobals f;
  g


class nodeColorFinder (g : graph) = object(self)
  inherit nopCilVisitor

  method vtype (t : typ) =
    let id = node_of_type t in
    let c  = T.colors_of_type t in
    g.(id).ncolors <- c;
    DoChildren

end


let findColoredNodes (f : file) (g : graph) : unit =
  let vis = new nodeColorFinder g in
  visitCilFile vis f


let colors_equal (c1 : colors) (c2 : colors) : bool =
  L.for_all (fun c -> L.mem c c2) c1 &&
  L.for_all (fun c -> L.mem c c1) c2


let enqueueNodes (g : graph) : int Q.t =
  let q = Q.create () in
  A.iteri (fun i _ -> Q.add i q) g;
  q


let processNode (g : graph) (id : int) : bool =
  let c' =
    L.fold_left (fun c id' -> list_union c g.(id').ncolors)
      g.(id).ncolors g.(id).incoming
  in
  if not(colors_equal g.(id).ncolors c') then begin
    g.(id).ncolors <- c';
    true
  end else false


let processQueue (g : graph) (q : int Q.t) : unit =
  while not(Q.is_empty q) do
    let id = Q.pop q in
    if processNode g id then begin
      L.iter (fun id' -> Q.add id' q) g.(id).outgoing
    end
  done


let attr_of_color (c : T.color) : attribute = Attr(T.string_of_color c,[])


class colorAdder (g : graph) = object(self)
  inherit nopCilVisitor

  method vtype (t : typ) =
    let oc = T.colors_of_type t in
    let ic = (t |> node_of_type |> A.get g).ncolors in
    if oc <> [] && not(colors_equal ic oc) then DoChildren
    else if list_equal (=) ic oc then DoChildren else
    let nattr = L.map attr_of_color ic in
    ChangeTo (typeAddAttributes nattr t)

end


let addInferredColors (f : file) (g : graph) : unit =
  let vis = new colorAdder g in
  visitCilFile vis f


let tut9 (f : file) : unit =
  let g = fileBuildGraph f in
  let q = enqueueNodes g in
  findColoredNodes f g;
  processQueue g q;
  addInferredColors f g;
  eraseNodeMarks f



