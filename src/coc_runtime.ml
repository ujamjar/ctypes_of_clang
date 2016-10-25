type ('a,'b) rt_structured = 
  {
    ctype : 'a Ctypes.typ;
    members : 'b;
  }

type ('a,'b,'c,'d) rt_structured_field =
  {
    field : ('a, 'b) Ctypes.field;
    structure : ('c, 'd) rt_structured;
  }

type 'p rt_enum = 
  {
    ctype : int Ctypes.typ;
    to_int : 'p -> int;
    of_int : int -> 'p;
  }

type ('a,'b,'c) rt_enum_field =
  {
    field : ('a, 'b) Ctypes.field;
    enum : 'c rt_enum;
  }

