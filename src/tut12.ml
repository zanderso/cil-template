



open Cil 
open Pretty
open Tututil
module S = String
module L = List



module GA = GrowArray
module A  = Cabs
module CH = Cabshelper


let prepareCommentArray (cca : comment array) (fname : string) : comment array =
  cca |> array_filter (fun (cl,_,_) -> fname = cl.A.filename)
      |> array_sort_result comment_compare


let commentsAdjacent (cca : comment array) (l : location)
                     : int list * comment array =
  if l = locUnknown then [], cca else
  let cca = prepareCommentArray cca l.file in
  (cca |> array_bin_search comment_compare (comment_of_cilloc l)), cca


let commentsBetween (cca : comment array) (l1 : location) (l2 : location)
                    : int list * comment array
  =
  if l1 = locUnknown then commentsAdjacent cca l2 else
  if l1.file <> l2.file then commentsAdjacent cca l2 else begin
  let cca = prepareCommentArray cca l1.file in
  let ll = array_bin_search comment_compare (comment_of_cilloc l1) cca in
  let hl = array_bin_search comment_compare (comment_of_cilloc l2) cca in
  let l, h =
    match ll, hl with
    | ([l] | [_;l]), h :: _ -> l, h
    | _ -> E.s(E.bug "bad result from array_bin_search")
  in
  (Array.init (h - l + 1) (fun i -> i + l) |> Array.to_list), cca
  end


let markComment (l : A.cabsloc) (cca : comment array) : unit =
  Array.iteri (fun i (l',s,b) ->
    if compare l l' = 0 then cca.(i) <- (l',s,true)
  ) cca


let printComments (cca : comment array) (l : location)
                  ((il,cca') : int list * comment array) : location =
  L.iter (fun i -> let c = cca'.(i) in
    if not(thd3 c) then begin
      markComment (fst3 c) cca;
      E.log "%a: Comment: %a -> %s\n"
        d_loc l d_loc (cilloc_of_cabsloc (fst3 c)) (snd3 c)
    end
  ) il;
  if il <> []
  then il |> L.rev |> L.hd |> Array.get cca' |> fst3 |> cilloc_of_cabsloc
  else l


class commentVisitorClass (cca : comment array) = object(self)
  inherit nopCilVisitor

  val mutable last = locUnknown

  method vinst (i : instr) =
    last <- i |> get_instrLoc
              |> commentsBetween cca last
              |> printComments cca (get_instrLoc i);
    DoChildren

  method vstmt (s : stmt) =
    last <- s.skind |> get_stmtLoc
                    |> commentsBetween cca last
                    |> printComments cca (get_stmtLoc s.skind);
    DoChildren

end


let tut12 (f : file) : unit =
  let cca = array_of_growarray CH.commentsGA in
  let vis = new commentVisitorClass cca in
  visitCilFile vis f




