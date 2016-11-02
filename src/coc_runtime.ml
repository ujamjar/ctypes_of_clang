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


