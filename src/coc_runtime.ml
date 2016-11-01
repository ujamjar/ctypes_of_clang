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

open Ctypes

(* my understanding of how it works on linux *)
let __builtin_va_list = 
  let x = structure "__builtin_va_list" in
  let _ = field x "f0" int in
  let _ = field x "f1" int in
  let _ = field x "f2" (ptr void) in
  let _ = field x "f3" (ptr void) in
  let () = seal x in
  x

let ldouble = 
  let x = structure "ldouble" in
  let _ = field x "f0" llong in
  let _ = field x "f1" llong in
  let () = seal x in
  x


