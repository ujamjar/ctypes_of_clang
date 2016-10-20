(** structure *)
type ('a,'b) structure = 
  {
    ctype : 'a Ctypes.structure Ctypes.typ;
    members : 'b;
  }

(** enum *)
type ('a,'b) enum = 
  {
    to_int : 'b -> int;
    of_int : int -> 'b;
  }


