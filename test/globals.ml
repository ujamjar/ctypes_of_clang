module G1 = struct
  [%ccode 
    {| #include "test/globals1.h" |}
      [@saveglobals "globals1.defs"]
      [@deferbindingexn]
  ]
end

module G2 = struct
  [%ccode
    {| #include "test/globals2.h" |}
      [@loadglobals ("globals1.defs", "G1")]
      [@deferbindingexn]
  ]
end

