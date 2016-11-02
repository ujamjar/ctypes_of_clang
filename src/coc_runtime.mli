type ('a,'b) rt_structured = 
  {
    ctype : 'a;
    members : 'b;
  }

type ('e,'p) rt_enum = 
  {
    ctype : 'e;
    to_int : 'p -> int64;
    of_int : int64 -> 'p;
  }

val __builtin_va_list : [`__builtin_va_list] Ctypes.structure Ctypes.typ

val ldouble : [`ldouble] Ctypes.structure Ctypes.typ

