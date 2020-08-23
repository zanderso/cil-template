


open Cil            
module E = Errormsg 


let tut0 (f : file) : unit =
  E.log "I'n in tut0 and I could change %s if I wanted to!\n" f.fileName;
  ()



