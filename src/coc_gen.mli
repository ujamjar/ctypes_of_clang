module Make(Clang : Coc_clang.S) : sig 

  module Attrs : sig

    type t = 
      {
        clangargs : string list;
        ctypesmodule : string;
        foreignmodule : string;
      }

    val get : (string Asttypes.loc * Parsetree.payload) list -> t 

  end

  val gen_ccode : loc:Location.t -> attrs:Attrs.t -> code:string -> 
    (string * Parsetree.expression) list

  val ccode : loc:Location.t -> attrs:Attrs.t -> code:string -> 
    Parsetree.structure_item list

  val register : unit -> unit

end

