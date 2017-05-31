(* types *)
[%ccode 
  {| #include <ncurses.h> |}
    [@clangargs ["-I/usr/lib/clang/3.8/include"]]
    [@viewint][@viewstring]
    [@onlytypes]]

(* variables and functions *)
module Bindings(F : Cstubs.FOREIGN) = struct

  [%ccode 
    {| #include <ncurses.h> |}
    [@clangargs ["-I/usr/lib/clang/3.8/include"]]
    [@foreignmodule "F"] 
    [@foreignfnmodule "F"] 
    [@viewint][@viewstring]
    [@onlydecls]
    [@excludedecls [".*:_.*";".*:trace"]]]

end

