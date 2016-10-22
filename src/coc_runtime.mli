(** structure/union *)
type ('a,'b) structure = 
  {
    ctype : 'a Ctypes.typ;
    members : 'b;
  }

type ('a,'b,'c,'d) substructure =
  {
    field : ('a, 'b) Ctypes.field;
    structure : ('c, 'd) structure;
  }

(** enum *)
type ('a,'b) enum = 
  {
    ctype : 'b Ctypes.typ;
    to_int : 'b -> int;
    of_int : int -> 'b;
  }

type ('a,'b,'c,'d) subenum =
  {
    field : ('a, 'b) Ctypes.field;
    enum : ('c, 'd) enum;
  }

