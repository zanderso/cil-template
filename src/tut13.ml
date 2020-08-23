




open Cil
open Tututil


module H = Hashtbl
module CG = Callgraph


module SG = Graph.Imperative.Digraph.ConcreteBidirectional(struct

  type t = CG.callnode
  let hash n = H.hash n.CG.cnid
  let compare n1 n2 =
    compare n1.CG.cnid n2.CG.cnid
  let equal n1 n2 = 
    n1.CG.cnid = n2.CG.cnid


end)


module D = Graph.Graphviz.Dot(struct

  type t = SG.t
  module V = SG.V
  module E = SG.E
  let iter_vertex = SG.iter_vertex
  let iter_edges_e = SG.iter_edges_e
  let graph_attributes g = []
  let default_vertex_attributes g = []
  let vertex_name v =
    match v.CG.cnInfo with
    | CG.NIVar (vi, _) -> vi.vname
    | CG.NIIndirect (s, _) -> s
  let vertex_attributes v = []
  let get_subgraph v = None
  let default_edge_attributes g = []
  let edge_attributes e = []


end)


let graph_of_callgraph (cg : CG.callgraph) : SG.t =
  let g = SG.create () in
  H.iter (fun s n -> SG.add_vertex g n) cg;
  H.iter (fun s n ->
    Inthash.iter (fun i n' ->
      SG.add_edge g n n'
    ) n.CG.cnCallees
  ) cg;
  g


let tut13 (f : file) : unit =
  let o = open_out !Ciltutoptions.tut13out in
  f |> CG.computeGraph |> graph_of_callgraph |> D.output_graph o;
  close_out o



