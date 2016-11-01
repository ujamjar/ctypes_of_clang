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

type ('e,'p) rt_enum = 
  {
    ctype : 'e Ctypes.typ;
    to_int : 'p -> int64;
    of_int : int64 -> 'p;
  }

type ('a,'b,'c,'d) rt_enum_field =
  {
    field : ('a, 'b) Ctypes.field;
    enum : ('c,'d) rt_enum;
  }

val __builtin_va_list : [`__builtin_va_list] Ctypes.structure Ctypes.typ

val ldouble : [`ldouble] Ctypes.structure Ctypes.typ

