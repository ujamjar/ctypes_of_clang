open Ctypes_of_clang

module L = Log.Make(struct let section = "ppx" end)
let () = 
  let f = open_out "coc.log" in
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  Log.set_output f

module Clang = Coc_clang.Make(struct let from = None end)
module Cgen = Coc_gen.Make(Clang)

let () = Cgen.register ()
