module Make(Clang : Coc_clang.S) : sig
  open Coc_enums
  open Clang

  type global = 
    | GComp of { loc : Loc.t; name : name; kind : kind }
    | GEnum of { loc : Loc.t; name : name }
    | GTypedef of { loc : Loc.t; name : name; typ : typ }
    | GVar of { loc : Loc.t; name : name; typ : typ; is_const : bool }
    | GFunc of { loc : Loc.t; name : name; typ : typ }
    
  and kind = Struct | Union

  and name = string * int

  and typ = 
    | TVoid
    | TBase of string
    | TGlobal of global 
    | TArray of typ * int64
    | TPtr of typ 
    | TFuncPtr of { ret : typ; args : typ list; variadic : bool }
    | TEnum of { global : global; items : (string * int64) list; kind : typ }
    | TComp of { global : global; members : (string * typ) list }

  val name_of_global : global -> string

  val string_of_global : global -> string

  val string_of_typ : typ -> string

  val default_enum_type : typ

  module TypeMap : Map.S with type key = global

  type ctx = 
    {
      decls : (cursor * global) list;
      comp_members_map : (string * typ) list TypeMap.t;
      enum_items_map : ((string * int64) list * typ) TypeMap.t;
      id : unit -> int;
    }

  val run : 
    ?log:bool -> ?pedantic:bool -> 
    ?unsaved:(string*string) list -> string list -> 
    (ctx, error_msgs) result

end

