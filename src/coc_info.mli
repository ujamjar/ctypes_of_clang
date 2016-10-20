module Make(Clang : Coc_clang.S) : sig

  open Coc_enums
  open Clang

  type enum_field = string * int64

  type t = 
    | Enum of 
      { 
        loc : Loc.t;
        name : string; 
        int_type : string; 
        fields : enum_field list;
        kindname : string;
        typename : string;
      } 

    | Function of 
      {
        loc : Loc.t;
        name : string; 
        returns : string;
        args : string list;
        kindname : string;
        typename : string;
      }

    | Struct of 
      {
        loc : Loc.t;
        name : string; 
        fields : (string * string * string) list;
        kindname : string;
        typename : string;
      }

    | Union of 
      {
        loc : Loc.t;
        name : string; 
        fields : (string * string * string) list;
        kindname : string;
        typename : string;
      }

    | Typedef of
      {
        loc : Loc.t;
        name : string; 
        aliases : string;
        kindname : string;
        typename : string;
      }

  val visit_cb : cursor -> cursor -> t list -> CXChildVisitResult.t * t list

  val run : ?unsaved:(string*string) list -> string list -> 
    (t list, error_msgs) result

end


