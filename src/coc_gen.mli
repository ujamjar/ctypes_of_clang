module Make(Clang : Coc_clang.S) : sig 

  module Attrs : sig

    type t = 
      {
        mutable clangargs : string list;
        mutable ctypesmodule : string;
        mutable foreignmodule : string;
      }

    val get : (string Asttypes.loc * Parsetree.payload) list -> t 

  end

  type t = 
    {
      (* ccode string location *)
      loc : Location.t;
      (* conversion attributes *)
      attrs : Attrs.t;
      (* generate unique let name bindings *)
      mangle : string -> string;
      (* mapping from types to bindings *)
      typetbl :
        ([ `Comp of string
         | `Enum of string
         | `Typedef of string ], 
         string)
        Hashtbl.t;
    }

  val gen_ccode : ctx:t -> code:string -> 
    (string * Parsetree.expression) list

  val ccode : ctx:t -> code:string -> 
    Parsetree.structure_item list

  val register : unit -> unit

end

