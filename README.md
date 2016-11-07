# Ctypes Of Clang

Auto-generate Ctypes bindings from C header/source files using
Clang.

# How it works

A ppx preprocessor is run over an OCaml file which detects 
extension points with embedded C-code ie

```
[%ccode {| #include <math.h> |}]
```

The c-code is parsed by clang and an AST queried to extract 
types and function definitions.

The process is fairly lax - if a definition cannot be converted it will 
be skipped.

### Expression form

```
# let sqrt = [%ccode "double sqrt(double);"];;
val sqrt : float -> float = <fun>
```

### Structure item

```
# module X = struct
    [%ccode {|
      double sqrt(double);
      void exit(int);
    |}]
end
module X : sig 
  val sqrt : float -> float 
  val exit : int32 -> unit 
end
```

# Examples

In the following examples the `Ctypes` library and `ctypes_of_ocaml`
ppx need to be loaded.

```
# #use "topfind";;
# #require "ctypes.foreign";;
# open Ctypes;;	
# open Foreign;;
# #ppx "./ppx_coc.byte";;
```

### Function definition

```
# let sqrt = [%ccode {| double sqrt(double); |} ];;
val sqrt : float -> float = <fun>
# sqrt 2.;;
- : float = 1.41421356237309515
```

### Structure definition

A C-structure is turned into a record of type `Coc_runtime.rt_structured`
which contains a ctypes structure value and an object expression with 
the corresponding structure fields.

```
# [%ccode {| struct foo { int x; float y; }; |}];;
val foo_0 : [ `foo ] structure typ = struct foo { int x; float y;  }
val foo : 
  ([ `foo ] structure typ,
   < x : (int, [ `foo ] structure) field;
     y : (float, [ `foo ] structure) field >)
  rt_structured =
  {Coc_runtime.ctype = struct foo { int x; float y;  }; 
               members = <obj> }
```

Note that structures define a pair of values (this is to deal with recursive
structures) so can't be defined as expressions.

### Enums

Enum definitions are turned into a record of type "Coc_runtime.rt_enum` which
contains the underlying enum type (some form of int) and a pair of functions
to convert between a polymorphic variant representing the enum and an int.

```
# [%ccode {| enum foo { A,B }; |}];;
val foo : (Unsigned.uint typ, [ `A | `B ]) rt_enum =
  { Coc_runtime.ctype = unsigned int; 
    to_int = <fun>; 
    of_int = <fun> }
```

When enums are used in other type definitions or functions the underlying
int type is used.

### Binding qsort

```
# [%ccode {| void qsort(void *base, int nmemb, int size, int (*)(void *, void *)); |}[@funptr "Foreign.funptr"]];;
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
# qsort (parr a) 10 4 (fun a b -> Pervasives.compare (convi a) (convi b));;
- : unit = ()
# a;;
- : int CArray.t = { 0, 4, 20, 21, 39, 41, 44, 70, 82, 85 }
```

### Attributes

Various attributes can be attached to the string containing c-code to control
the conversion process.

* `[@clangargs <string-list>]` arguments passed to the clang c-compiler
* `[@ctypesmodule <string>]`, `[@foreignmodule <string>]`, `[@foreignfnmodule <string>]`, `[@typesmodule <string>]` control access to various ctypes modules.  _This will probably be simplified to just support Foreign and Cstubs generation._
* `[@funptr <string>]` when callbacks are described by default we use `Ctypes.static_funptr`.  This option can override that choice ie to use `Foreign.funptr`.
* `[@staticstructs]` extract alignment/size/offset information from clang, rather than infer it using ctypes.
* `[@deferbindingexn]` (Foreign only) wrap functions and capture binding exceptions so they occur when the function is called rather than when bound
* `[@gentypes]`, `[@gendecls]` only generate types (structs, enums, typedefs etc) or declarations (functions and variables).
* `[@includedecls <string-list>]`, `[@excludedecls <string-list>]`, `[@includetypes <string-list>]`, `[@excludetypes <string-list>]` control generation of bindings.

### Controlling conversion

The include/exclude types and decls attributes allow specification of what should be
converted.  Each specify a list of regular expressions in `Humane_re` form.

For each definition a string is constructed with the filename and declaration 
name seperated by a colon ie `"stdlib.h:qsort"`.  First the exclude list will 
be checked - if any regular expression matches then the definition will be 
skipped.  Then the include list will be checked - if any matches the definition 
will be generated.  By default (empty include/exclude lists) nothing is excluded 
and everything is included.

# References

- https://github.com/yallop/ocaml-bindings-generator
- https://github.com/Yamakaky/rust-bindgen

