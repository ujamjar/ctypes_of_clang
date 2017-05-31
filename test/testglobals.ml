module X = struct
  [%ccode 
    {|
      struct foo {
        int goo;
      };
    |}
    [@saveglobals "foo.globals"] ]
end

module Y = struct
  [%ccode 
    {|
      struct foo {
        int goo;
      };
      struct foo fn(void);
    |}
    [@loadglobals ("foo.globals", "X")]
    [@deferbindingexn]]
end

