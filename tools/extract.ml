open Printf
open Rresult.R

module Clang = Coc_clang.Make(struct let from = None end)
module Cparse = Coc_parse.Extract(Clang)
  
let x = 
  let args = List.tl @@ Array.to_list Sys.argv in
  Cparse.run args >>= Coc_gen.run >>= fun ast ->
  Coc_gen.write_functions Format.std_formatter ast >>= fun () ->
  Coc_gen.write_functions Format.std_formatter ast 

let () = 
  match x with
  | Error x -> Array.iter (printf "%s\n") x
  | _ -> ()
