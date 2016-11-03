open Ocamlbuild_plugin

let link libs = S (List.flatten @@ List.map (fun l -> [A "-cclib"; A ("-l" ^ l)]) libs)

let () = dispatch @@ function
  | Before_options ->
    Options.use_ocamlfind := true
  | After_rules -> begin
    (* bytecode link with clang *)
    flag ["ocaml"; "link"; "byte"; "clang"] @@ 
      S[ A"-custom"; 
      A"-cclib"; A"-L/usr/lib/llvm-3.8/lib"; 
      A"-cclib"; A"-lclang-3.8";
      A"-linkall" ];
(*
    (* examples/foo *)
    dep ["link";"ocaml";"use_cfoo"] ["examples/foo/libcfoo.a"];
    dep ["c"; "compile"] ["./examples/foo/foo.h"];
    flag ["link";"ocaml";"use_cfoo"] (S[
        A"-cclib";A"-Lexamples/foo"; 
        A"-dllpath"; A"examples/foo";
        A"-dllib";A"-lcfoo";
        A"-cclib";A"-lcfoo"]);
*)

    (* stub generation *)
    rule "cstubs: x_stubgen.byte -> x_cstubs.c x_stubs.ml" 
      ~prods:["%_cstubs.c"; "%_stubs.ml"]
      ~deps:["%_stubgen.byte"] 
      (fun env _ ->
        Seq [
          Cmd (S [ A (env "%_stubgen.byte"); A "-ml"; Sh ">"; A (env "%_stubs.ml") ]);
          Cmd (S [ A (env "%_stubgen.byte"); A "-c"; Sh ">"; A (env "%_cstubs.c") ]);
        ]);

    (* date example *)
    dep ["ocaml"; "link"; "use_date"] ["examples/date/libodate.a"];
    flag ["c"; "compile"; "use_date"] @@ S[ A"-ccopt"; A"-shared"; ]; 
    flag ["c"; "ocamlmklib"; "use_date" ] @@ S[ A"-lrt"; A"-linkall" ];
    flag ["ocaml"; "link"; "byte"; "use_date"] @@ 
      S[ A"-I"; A"examples/date"; A"-dllib"; A"-lodate";
        A"-dllpath"; A"_build/examples/date"; A"-I"; A"examples/date"; ];
    flag ["ocaml"; "link"; "native"; "use_date"] @@ 
      S[ A"-cclib"; A"-lrt"; A"-cclib"; A"-lodate"; A"-cclib"; A"-Lexamples/date"; ];

  end
  | _ -> ()



