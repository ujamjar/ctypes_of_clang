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

val cstr_length : char Ctypes.ptr -> int
val cstr : char Ctypes.ptr -> string

val field : offset:int -> 't Ctypes.typ -> string -> 'a Ctypes.typ -> 
            ('a, (('s, [<`Struct | `Union]) Ctypes_static.structured as 't)) Ctypes.field

val seal : size:int -> align:int -> 
           (_, [< `Struct | `Union]) Ctypes_static.structured Ctypes.typ -> 
           unit

