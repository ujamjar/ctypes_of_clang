module Clang = Coc_clang.Make(struct let from = None end)
module Cgen = Coc_gen.Make(Clang)

let () = Cgen.register ()
