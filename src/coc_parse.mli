module Make(Clang : Coc_clang.S) : sig
  open Coc_enums
  open Clang

  type global = 
    | GComp of { loc : Loc.t; name : name; kind : kind; clayout : clayout }
    | GEnum of { loc : Loc.t; name : name }
    | GTypedef of { loc : Loc.t; name : name; typ : typ }
    | GVar of { loc : Loc.t; name : name; typ : typ; is_const : bool }
    | GFunc of { loc : Loc.t; name : name; typ : typ }
    | GBuiltin of { name : string; typ : typ }
    
  and kind = Struct | Union

  and name = string * int

  and clayout = { size : int; align : int }

  and typ = 
    | TVoid
    | TBase of string
    | TNamed of string * bool
    | TGlobal of global 
    | TArray of typ * int
    | TPtr of typ 
    | TFuncPtr of { ret : typ; args : typ list; variadic : bool }
    | TFuncProto of { ret : typ; args : typ list; variadic : bool }
    | TEnum of { global : global; items : (string * int64) list; kind : typ }
    | TComp of { global : global; members : member list }

  and member = 
    | Field of {name:string; typ:typ; offset:int}
    | Bitfield of {name:string; typ:typ; width:int; offset:int}

  val builtin : 
    p:(Loc.t -> (string * int) -> Coc_enums.CXCursorKind.t -> bool) -> 
    (global * string) -> global option

  val name_of_global : global -> string

  val loc_of_global : global -> Loc.t

  val string_of_global : global -> string

  val string_of_typ : typ -> string

  val default_enum_type : typ

  module TypeMap : Map.S with type key = global

  type ctx = 
    {
      decls : (cursor * global) list;
      globals : global list;
      builtins : (global * string) list;
      comp_members_map : member list TypeMap.t;
      enum_items_map : ((string * int64) list * typ) TypeMap.t;
      id : unit -> int;
    }

  module BT : sig
    val schar : typ
    val char : typ
    val uchar : typ
    val ushort : typ
    val uint : typ
    val ulong : typ
    val ullong : typ
    val short : typ
    val int : typ
    val long : typ
    val llong : typ
    val float : typ
    val double : typ
  end

  val run : 
    ?log:bool -> ?pedantic:bool -> 
    ?unsaved:(string*string) list -> 
    builtins:(global * string) list ->
    string list -> 
    (ctx, error_msgs) result

end

