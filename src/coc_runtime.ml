(* structure *)

type ('a,'b) structure = 
  {
    ctype : 'a Ctypes.structure Ctypes.typ;
    members : 'b;
  }

(* enum *)

type ('a,'b) enum = 
  {
    ctype : 'b Ctypes.typ;
    to_int : 'b -> int;
    of_int : int -> 'b;
  }

