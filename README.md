# Ctypes Of Clang

Auto-generate Ctypes bindings from C header/source files using
Clang.

_Current status is just about working for some simple cases, but
hardly tested and lacking many features._

# Examples

### Function definition

```
# open Ctypes;;
# open Foreign;;
# #ppx "./ppx_coc.byte";;
# let sqrt = [%cfn {| double sqrt(double); |} ];;
val sqrt : float -> float = <fun>
# sqrt 2.;;
- : float = 1.41421356237309515
```

### Structure definition

A C-structure is turned into an object expression with
corresponding fields.

A special field called `_struct` is added (_potential issue;
name clashes)_ with the ocaml structure type and allocation
routines.

Note that an OCaml (phantom) type with the same name as
the C struct should be pre-declared (it is used in the 
Ctypes structure definition).

```
# type foo;;
# let foo = [%cstruct {|struct foo { int x; int y }; |}];;
val foo :
  < _struct : < alloc : ?finalise:(foo Ctypes.structure Ctypes.ptr -> unit) ->
                        foo Ctypes.structure ->
                        foo Ctypes.structure Ctypes.ptr;
                make : ?finalise:((foo, [ `Struct ]) Ctypes.structured ->
                                  unit) ->
                       unit -> (foo, [ `Struct ]) Ctypes.structured;
                typ : foo Ctypes.structure Ctypes.typ >;
    x : (int32, foo Ctypes.structure) Ctypes.field;
    y : (int32, foo Ctypes.structure) Ctypes.field > =
  <obj>
# let foo_struct = foo#_struct#make();;
val foo_struct : (foo, [ `Struct ]) Ctypes.structured = { x = 0, y = 0  }
# setf foo_struct foo#x 1l;;
- : unit = ()
# let foo_ptr = foo#_struct#alloc foo_struct;;
val foo_ptr : foo Ctypes.structure Ctypes.ptr = (struct foo*) 0x33a8280
# !@ foo_ptr;;
- : foo Ctypes.structure = { x = 1, y = 0  }
# setf (!@ foo_ptr) foo#y 2l;;
- : unit = ()
# !@ foo_ptr;;
- : foo Ctypes.structure = { x = 1, y = 2  }
# foo_struct;;
- : (foo, [ `Struct ]) Ctypes.structured = { x = 1, y = 0  }
```

The following shows the actual generated ocaml code.

```
$ ocamlfind ppx_tools/rewriter -ppx ./ppx_coc.byte -str "let a = [%cstruct {|struct foo { int x; int y; }; |}]"
let a =
  let _struct : foo structure typ = structure "foo"  in
  let x = field _struct "x" int32_t  in
  let y = field _struct "y" int32_t  in
  let () = seal _struct  in
  object
    method _struct =
      object
        method typ = _struct
        method alloc ?finalise  def = allocate ?finalise _struct def
        method make ?finalise  () = make ?finalise _struct
      end
    method x = x
    method y = y
  end 
```

# References

- https://github.com/yallop/ocaml-bindings-generator
- https://github.com/Yamakaky/rust-bindgen


