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

type ('a,'b) rt_enum = 
  {
    ctype : 'b Ctypes.typ;
    to_int : 'b -> int;
    of_int : int -> 'b;
  }

type ('a,'b,'c,'d) rt_enum_field =
  {
    field : ('a, 'b) Ctypes.field;
    enum : ('c, 'd) rt_enum;
  }

