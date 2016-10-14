
module Info(Clang : Clang.S) : sig

  open Enums
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
        int_type : Ctyping.builtin_int_type option; 
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

  val run : string list -> (t list, string array) result

end

module Extract(Clang : Clang.S) : sig
  open Enums
  open Clang

  val visit_cb : cursor -> cursor -> unit Extract.Stage1.M.t -> 
    CXChildVisitResult.t * unit Extract.Stage1.M.t 

  val run : string list -> (unit Extract.Stage1.M.t, string array) result

end

