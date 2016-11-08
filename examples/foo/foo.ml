[%ccode 
  {| #include "examples/foo/foo.h" |}
    [@onlytypes][@viewint]]

module Bindings (F : Cstubs.FOREIGN) = struct

  [%ccode 
    {| #include "examples/foo/foo.h" |}
    [@foreignmodule "F"]
    [@foreignfnmodule "F"]
    [@onlydecls]
    [@viewint]]

end

