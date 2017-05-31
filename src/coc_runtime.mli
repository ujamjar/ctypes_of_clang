val field : offset:int -> 't Ctypes.typ -> string -> 'a Ctypes.typ -> 
            ('a, (('s, [<`Struct | `Union]) Ctypes_static.structured as 't)) Ctypes.field

val seal : size:int -> align:int -> 
           (_, [< `Struct | `Union]) Ctypes_static.structured Ctypes.typ -> 
           unit

module type Foreign_options = sig
  val from : Dl.library option
  val stub : bool
end

module Make_foreign(Options : Foreign_options) : Cstubs.FOREIGN
  with type 'a result = 'a
   and type 'a return = 'a 

module Ffi_foreign : Cstubs.FOREIGN
  with type 'a result = 'a
   and type 'a return = 'a 

