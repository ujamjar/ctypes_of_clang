module Clang = Coc_clang.Make(struct let from = None end)
module Cgen = Coc_gen2.Make(Clang)

let () = Cgen.register ()
