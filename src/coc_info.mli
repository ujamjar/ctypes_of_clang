module Make(Clang : Coc_clang.S) : sig

  open Coc_enums
  open Clang

  type enum_field = string * int64

  type loc = 
    {
      file : string;
      line : int;
      col : int;
      offset : int;
    }

  type t = 
    | Enum of 
      { 
        loc : loc;
        name : string; 
        int_type : string; 
        fields : enum_field list;
        kindname : string;
        typename : string;
      } 

    | Function of 
      {
        loc : loc;
        name : string; 
        returns : string;
        args : string list;
        kindname : string;
        typename : string;
      }

    | Struct of 
      {
        loc : loc;
        name : string; 
        fields : (string * string) list;
        kindname : string;
        typename : string;
      }

    | Union of 
      {
        loc : loc;
        name : string; 
        fields : (string * string) list;
        kindname : string;
        typename : string;
      }

    | Typedef of
      {
        loc : loc;
        name : string; 
        aliases : string;
        kindname : string;
        typename : string;
      }

  val visit_cb : cursor -> cursor -> t list -> CXChildVisitResult.t * t list

  val run : ?unsaved:(string*string) list -> string list -> (t list, string array) result

end


