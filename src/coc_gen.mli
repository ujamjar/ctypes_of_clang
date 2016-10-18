module Make(Clang : Coc_clang.S) : sig 
  module Cparse : module type of Coc_parse.Make(Clang)
  open Cparse
  
  type ctx

  val init_ctx : global list -> ctx

  (* move to its own module and/or sub-library? *)
  module Ppx : sig
    val register : unit -> unit
  end

end

