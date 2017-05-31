open Ctypes_of_clang;;

[%ccode 
  {| #include "examples/foo/clib/foo.h" |}
    [@onlytypes][@viewint]]
;;

module Bindings (F : Cstubs.FOREIGN) = struct

  [%ccode 
    {| #include "examples/foo/clib/foo.h" |}
    [@foreignmodule "F"]
    [@foreignfnmodule "F"]
    [@onlydecls]
    [@viewint]]

end
;;
