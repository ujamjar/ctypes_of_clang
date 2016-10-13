open Enums
open Clang

module Simple : sig

  type t = [ `enum of string * (string * int64) list
           | `func of string * string * string list
           | `union of string * (string * string) list
           | `struc of string * (string * string) list
           | `typedef of string * string ]

  val func_cb : cursor -> cursor -> 
    string list -> CXChildVisitResult.t * string list

  val struct_cb : cursor -> cursor -> 
    (string * string) list -> CXChildVisitResult.t * (string * string) list
  
  val enum_cb : cursor -> cursor -> 
    (string * int64) list -> CXChildVisitResult.t * (string * int64) list
  
  val visit_cb : cursor -> cursor -> t list -> CXChildVisitResult.t * t list

  val run : string list -> (t list, string array) result

end

module LessSimple : sig

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
        int_type : Ctyping.builtin_int_type option; 
        fields : enum_field list;
        kindname : string;
        typename : string;
      } 

    | Function of 
      {
        loc : loc;
        name : string; 
        kindname : string;
        typename : string;
      }

    | Struct of 
      {
        loc : loc;
        name : string; 
        kindname : string;
        typename : string;
      }

    | Union of 
      {
        loc : loc;
        name : string; 
        kindname : string;
        typename : string;
      }

    | Typedef of
      {
        loc : loc;
        name : string; 
        kindname : string;
        typename : string;
      }

  val visit_cb : cursor -> cursor -> t list -> CXChildVisitResult.t * t list

  val run : string list -> (t list, string array) result

end

