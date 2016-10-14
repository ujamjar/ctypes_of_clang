open Printf

module Clang = Clang.Make(struct let from = None end)
module Cparse = Cparse.Make(Clang)
open Cparse.LessSimple
  
let types_file = Some "types.ml.gen" 
let functions_file = Some "fns.ml.gen"

let () = 
  let args = List.tl @@ Array.to_list Sys.argv in
  match Cparse.Monad.run args with
  | Error e -> Array.iter print_endline e
  | Ok ast ->
    Extract.main ~regex:".*" ~ast ~types_file ~functions_file
