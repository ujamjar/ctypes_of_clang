# Ctypes Of Clang

Auto-generate Ctypes bindings from C header/source files using
Clang.

_Current status; struct and function bindings, over basic 
types, roughly working._

# Examples

### Function definition

```
# open Ctypes;;
# open Foreign;;
# #ppx "./ppx_coc.byte";;
# let%cfn sqrt = {| double sqrt(double); |};;
val sqrt : float -> float = <fun>
# sqrt 2.;;
- : float = 1.41421356237309515
```

### Structure definition

A C-structure is turned into a record of type `Coc\_runtime.structure`
which contains a ctypeis structure value and object expression with 
the corresponding structure fields.

Note that an OCaml polymorphic variant type with the same name as
the C struct is used within the Ctypes structure definition.

```
# let%cstruct sfoo, mfoo = {| struct foo { int x; float y; }; |};;
val sfoo : [ `foo ] structure typ =
  struct foo { int32_t x; float y;  }
val mfoo :
  < x : (int32, [ `foo ] structure) field;
    y : (float, [ `foo ] structure) field > =
  <obj>

val foo : 
  ([ `struct_foo ], 
   < x : (int32, [ `struct_foo ] structure) field;
     y : (float, [ `struct_foo ] structure) field >) Coc_runtime.structure =
    { ctype = struct foo { int32_t x; float y;  }; 
      members = <obj> }

# let sfoo = make foo.ctype;;
val foo : ([ `foo ], [ `Struct ]) structured = { x = 0, y = 0  }
# setf sfoo foo.members#x 1l;;
- : unit = ()

# let pfoo = allocate foo.ctype sfoo;;
val pfoo : [ `foo ] structure ptr = (struct foo*) 0x3381ca0
# setf (!@ pfoo) foo.members#y 2.3;;
- : unit = ()

# !@ pfoo;;
- : [ `foo ] structure = { x = 1, y = 2.29999995232  }
# sfoo;;
- : ([ `foo ], [ `Struct ]) structured = { x = 1, y = 0  }
```

### Enums

....

### Code blocks

C-code can be associated with a structure allowing multiple definitions.

The structure should contain a single string item which contains the c-code
to be processed.

```
# module%ccode X = struct {|
  
    struct foo { int x; int y; };
  
    void fn(struct foo *foo);
  
|} end;;

module X : 
  sig 
    val foo : 
      ([ `struct_foo ],
       < x : (int32, [ `struct_foo ] structure) field;
         y : (int32, [ `struct_foo ] structure) field >)
      Coc_runtime.structure
    val fn : [ `struct_foo ] structure ptr -> unit
  end
```

### Binding qsort

```
# let%cfn qsort = {| void qsort(void *base, int nmemb, int size, int (*)(void *, void *)); |};;
val qsort :
  unit ptr -> int32 -> int32 ->
  (unit ptr -> unit ptr -> int32) -> unit = <fun>

# let a = CArray.make int 10;;
val a : int CArray.t = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
# for i=0 to 9 do CArray.set a i (Random.int 100) done;;
- : unit = ()

# let parr a = to_voidp (CArray.start a);;
val parr : 'a CArray.t -> unit ptr = <fun>
# let convi p = !@ (from_voidp int p);; 
val convi : unit ptr -> int = <fun>

# qsort (parr a) 10l 4l (fun a b -> Int32.of_int @@ Pervasives.compare (convi a) (convi b));;
- : unit = ()
# a;;
- : int CArray.t = { 0, 4, 20, 21, 39, 41, 44, 70, 82, 85 }
```

# References

- https://github.com/yallop/ocaml-bindings-generator
- https://github.com/Yamakaky/rust-bindgen


