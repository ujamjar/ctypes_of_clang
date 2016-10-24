# Ctypes Of Clang

Auto-generate Ctypes bindings from C header/source files using
Clang.

_Current status; struct and function bindings, over basic 
types, roughly working._

# How it works

A ppx preprocessor is run over an OCaml file which detects quoted strings 
with the id `ccode` ie.

```
{ccode| ... |ccode}
```

Such `ccode` quoted strings can be declared as either expressions or
structure items.  In expression form the final definition in the c-code
will be extracted.  As a structure item all the definitions will be
extracted.

### Expression form

```
# let sqrt = {ccode| double sqrt(double); |ccode}
val sqrt : float -> float = <fun>
```

### Structure item

```
# module X = struct
    {ccode|
      double sqrt(double);
      void exit(int);
    |ccode}
end
module X : sig 
  val sqrt : float -> float 
  val exit : int32 -> unit 
end
```

# Examples

In the following examples the `Ctypes` library and `ctypes_of_ocaml`
ppx need to be loaded.

_note: opening Ctypes etc should not be needed_

```
# #use "topfind";;
# #require "ctypes.foreign";;
# open Ctypes;;	
# open Foreign;;
# #ppx "./ppx_coc.byte";;
```

### Function definition

```
# let sqrt = {ccode| double sqrt(double); |ccode};;
val sqrt : float -> float = <fun>
# sqrt 2.;;
- : float = 1.41421356237309515
```

### Structure definition

A C-structure is turned into a record of type `Coc_runtime.rt_structured`
which contains a ctypes structure value and an object expression with 
the corresponding structure fields.

```
# let foo = {ccode| struct foo { int x; float y; }; |ccode};;
val foo :
  ([ `struct_foo ] structure,
   < x : (int32, [ `struct_foo ] structure) field;
     y : (float, [ `struct_foo ] structure) field >)
  Coc_runtime.rt_structured =
  {Coc_runtime.ctype = struct foo { int32_t x; float y;  }; members = <obj>}
```

### Enums

...to be decided...

### Binding qsort

```
# {ccode| void qsort(void *base, int nmemb, int size, int (*)(void *, void *)); |ccode};;
val qsort :
  unit ptr -> int32 -> int32 ->
  (unit ptr -> unit ptr -> int32) -> unit = <fun>

# let a = CArray.make int 10;;
val a : int CArray.t = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
# for i=0 to 9 do CArray.set a i (Random.int 100) done;;
- : unit = ()
# a;;
- : int CArray.t = { 44, 85, 82, 41, 39, 0, 4, 20, 21, 70 }

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

# Issues

- {ccode| enum {A,B}; |ccode} as stri causes pprintast crash as it generates `let = ...` with
  no binding name.  Such anonymous enums need a different generation strategy.

- struct a { int a; } - field name same as outer structure leads to a clash in generated code.

