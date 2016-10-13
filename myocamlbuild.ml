open Ocamlbuild_plugin


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
      flag ["ocaml"; "link"; "byte"; "toplevel"] @@ 
        S[ 
        A"-cclib"; A"-L/usr/lib/llvm-3.8/lib"; 
        A"-cclib"; A"-lclang-3.8";
        A"-linkall" ];
  end
  | _ -> ()



