module Make(Clang : Coc_clang.S) : sig 

  module Cparse : module type of Coc_parse.Make(Clang)

  module Attrs : sig
    type t = 
      {
        mutable clangargs : string list;
        mutable ctypesmodule : string;
        mutable foreignmodule : string;
        mutable foreignfnmodule : string;
        mutable typesmodule : string;
        mutable funptr : string;
        mutable staticstructs : bool;
        mutable deferbindingexn : bool;
        mutable viewstring : bool;
        mutable viewstringopt : bool;
        mutable viewint : bool;
        mutable doccomments : bool;
        mutable gentypes : bool;
        mutable gendecls : bool;
        mutable excludedecls : string list;
        mutable includedecls : string list;
        mutable excludetypes : string list;
        mutable includetypes  : string list;
      }
    val def_attrs : unit -> t
    val get : (string Asttypes.loc * Parsetree.payload) list -> t 
  end

  module G : Map.S with type key = Cparse.global

  type t = 
    {
      loc : Location.t;
      attrs : Attrs.t;
      mangle : string -> string;
      global_to_binding : string G.t;
      builtins : Cparse.global list;
      comp_members_map : Cparse.member list Cparse.TypeMap.t;
      enum_items_map : ((string * int64) list * Cparse.typ) Cparse.TypeMap.t;
      gendecl : Clang.Loc.t -> string -> bool;
      gentype : Clang.Loc.t -> string -> bool;
    }

  val ctype : ctx:t -> Cparse.typ -> Parsetree.expression

  val func_ctype : 
    ?evar:(Attrs.t -> string -> Parsetree.expression) -> 
    ctx:t -> Cparse.typ -> Cparse.typ list ->
    Parsetree.expression

  val gen_cfn : ctx:t -> Cparse.typ -> string -> Cparse.typ list -> Parsetree.expression

  val gen_cvar : ctx:t -> string -> Cparse.typ -> Parsetree.expression
                                                    
  val gen_enum : ctx:t -> (string * int64) list -> Cparse.typ -> Parsetree.expression

  val gen_cstruct_decl : ctx:t -> string -> string -> Cparse.kind -> 
    Parsetree.expression -> Parsetree.expression

  val gen_cstruct : ctx:t -> string -> string -> Cparse.member list -> Parsetree.expression

  val gen_cstruct_static : ctx:t -> string -> string -> 
    Cparse.member list -> Cparse.clayout ->
    Parsetree.expression

  val run : ctx:t -> code:string -> (Cparse.ctx -> 'a) -> 'a

  val fwd_decl : ctx:t -> (string * string * Cparse.global) -> 
    (Cparse.global * string * Parsetree.expression) option

  val gen_decl : ctx:t -> (string * string * Cparse.global) -> 
    (Cparse.global * string * Parsetree.expression) option

  val mangle_declarations : ctx:t -> Cparse.global list -> 
    (string * string * Cparse.global) list

  val gen_ccode : ctx:t -> code:string -> 
    (Cparse.global * string * Parsetree.expression) list

  val ccode : ctx:t -> code:string -> 
    Parsetree.structure_item list

  val init_ctx : Location.t -> Attrs.t -> t

  val register : unit -> unit

end

