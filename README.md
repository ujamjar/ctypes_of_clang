# Ctypes Of Clang

Auto-generate Ctypes bindings from C header/source files using
Clang.

_Current status is just about working for some simple cases, but
hardly tested and lacking many features._

# Example

```
# open Ctypes;;
# open Foreign;;
# #ppx "./ppx_coc.byte";;
# let sqrt = [%cfn {| double sqrt(double); |} ];;
val sqrt : float -> float = <fun>
# sqrt 2.;;
- : float = 1.41421356237309515
```

# References

https://github.com/yallop/ocaml-bindings-generator
https://github.com/Yamakaky/rust-bindgen


