module Make(Clang : Coc_clang.S) : sig
  open Coc_enums
  open Clang

  module CHash : Hashtbl.S with type key = cursor

  type ctx = 
    {
      mutable name : global CHash.t [@printer fun fmt x -> Format.fprintf fmt "<CHash>"];
      mutable globals : global list;
    }

  (*and options = 
    {
    }*)

  and global = 
    | GType of type_info
    | GComp of comp_info
    | GCompDecl of comp_info
    | GEnum of enum_info
    | GEnumDecl of enum_info
    | GVar of var_info
    | GFunc of var_info
    | GOther

  and func_sig = 
    {
      fs_ret : typ;
      fs_args : (string * typ) list;
      fs_variadic : bool;
    }

  and typ = 
    | TVoid
    | TInt of ikind
    | TFloat of fkind
    | TPtr of typ * bool 
    | TArray of typ * int64
    | TFuncProto of func_sig
    | TFuncPtr of func_sig
    | TNamed of type_info
    | TComp of comp_info
    | TEnum of enum_info

  and ikind = 
    | IBool
    | ISChar
    | IUChar
    | IShort
    | IUShort
    | IInt
    | IUInt
    | ILong
    | IULong
    | ILongLong
    | IULongLong
    | IWChar

  and fkind = 
    | FFloat
    | FDouble
  
  and comp_member = 
    | Field of field_info
    | Comp of comp_info
    | Enum of enum_info
    | CompField of comp_info * field_info
    | EnumField of enum_info * field_info

  and comp_kind = 
    | Struct
    | Union
    
  and comp_info = 
    {
      ci_kind : comp_kind;
      ci_name : string;
      mutable ci_members : comp_member list;
    }

  and field_info = 
    {
      fi_name : string;
      fi_typ : typ;
      (*fi_bitfields : (string * int) list option;*)
    }

  and enum_info = 
    {
      ei_name : string;
      ei_items : enum_item list;
      ei_kind : ikind;
    }

  and enum_item = 
    {
      eit_name : string;
      eit_val : int64;
    }

  and type_info =
    {
      ti_name : string;
      ti_typ : typ;
    }

  and var_info = 
    {
      vi_name : string;
      vi_typ : typ;
      vi_val : int64 option;
      vi_is_const : bool;
    }
    [@@ deriving show]

  module GSet : Set.S with type elt = global

  val run : ?unsaved:(string*string) list -> string list -> (ctx, unit) result

end

