open Ctypes
open Foo
module Fns = Bindings(Foo_stubs)
open Fns

let a = init_foo 7 13.
let b = init_foo 3 7.5
let c = init_foo 0 0.

let () = add_foo_ptr (addr a) (addr b) (addr c)
let () = print_foo c

let c = add_foo a b 

let coerce t fn = coerce (Foreign.funptr t) (static_funptr t) fn

let cb_t = int @-> returning int
                  
let c = map_foo_a c (coerce cb_t incr)
let () = print_foo c

let c = map_foo_a c (coerce cb_t (fun x -> x*2))
let () = print_foo c
