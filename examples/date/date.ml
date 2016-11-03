(* types *)
{ccode| #include <time.h> |ccode}
  [@clangargs ["-I/usr/lib/clang/3.8/include"]]
  [@onlytypes]

(* variables and functions *)
module Bindings(F : Cstubs.FOREIGN) = struct

  {ccode| #include <time.h> |ccode}
    [@clangargs ["-I/usr/lib/clang/3.8/include"]]
    [@foreignmodule "F"] (* foreign, foreign_value etc *)
    [@foreignfnmodule "F"] (* (@->), returning *)
    [@onlydecls]

end

