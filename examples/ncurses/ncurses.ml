(* types *)
{ccode| #include <ncurses.h> |ccode}
  [@clangargs ["-I/usr/lib/clang/3.8/include"]]
  [@onlytypes]

(* variables and functions *)
module Bindings(F : Cstubs.FOREIGN) = struct

  {ccode| #include <ncurses.h> |ccode}
    [@clangargs ["-I/usr/lib/clang/3.8/include"]]
    [@foreignmodule "F"] 
    [@foreignfnmodule "F"] 
    [@onlydecls]
    [@excludedecls ["_.*";"trace"]]

end

