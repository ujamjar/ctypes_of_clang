[%ccode 
  {| #include "examples/foo/foo.h" |}
  [@onlytypes]]

module Bindings (F : Cstubs.FOREIGN) = struct

  [%ccode 
    {| #include "examples/foo/foo.h" |}
    [@foreignmodule "F"]
    [@foreignfnmodule "F"]
    [@onlydecls]]

end

