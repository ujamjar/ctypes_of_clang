module Make(Clang : Coc_clang.S) : sig 

  module Attrs : sig

    type t = 
      [ `clangargs of string list ]

    val get : (string Asttypes.loc * Parsetree.payload) list -> t list

    val clangargs : t list -> string list

  end

  val cfn : loc:Location.t -> code:string -> Parsetree.expression

  val cstruct : loc:Location.t -> code:string -> Parsetree.expression

  val cenum : loc:Location.t -> code:string -> Parsetree.expression

  val ccode : loc:Location.t -> attrs:Attrs.t list -> code:string -> 
    Parsetree.structure_item list

  val register : unit -> unit

end

