module Make(Clang : Coc_clang.S) : sig 

  val cfn : Location.t -> string -> Parsetree.expression

  val cstruct : Location.t -> string -> Parsetree.expression

  val cenum : Location.t -> string -> Parsetree.expression

  val ccode : Location.t -> string -> Parsetree.structure_item list

  val register : unit -> unit

end

