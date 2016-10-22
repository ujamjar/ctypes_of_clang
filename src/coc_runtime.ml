(* structure *)

type ('a,'b) structure = 
  {
    ctype : 'a Ctypes.structure Ctypes.typ;
    members : 'b;
  }

type ('a,'b,'c) substructure =
  {
    field : ('a Ctypes.structure, 'b Ctypes.structure) Ctypes.field;
    structure : ('a, 'c) structure;
  }

(* enum *)

type ('a,'b) enum = 
  {
    ctype : 'b Ctypes.typ;
    to_int : 'b -> int;
    of_int : int -> 'b;
  }

