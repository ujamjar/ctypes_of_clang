module Make(Clang : Coc_clang.S) : sig 

  module Attrs : sig

    type t = 
      [ `clangargs of string list ]

    val get : (string Asttypes.loc * Parsetree.payload) list -> t list

    val clangargs : t list -> string list

  end

  val gen_ccode : loc:Location.t -> attrs:Attrs.t list -> code:string -> 
    (string * Parsetree.expression) list

  val ccode : loc:Location.t -> attrs:Attrs.t list -> code:string -> 
    Parsetree.structure_item list

  val register : unit -> unit

end

