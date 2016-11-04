{ccode| #include "examples/foo/foo.h" |ccode}
  [@onlytypes]

module Bindings (F : Cstubs.FOREIGN) = struct

  {ccode| #include "examples/foo/foo.h" |ccode}
    [@foreignmodule "F"]
    [@foreignfnmodule "F"]
    [@onlydecls]

end

