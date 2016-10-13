open Printf

let () = 
  let args = List.tl @@ Array.to_list Sys.argv in
  match Parse.Simple.run args with
  | Error e -> Array.iter print_endline e
  | Ok r ->
    List.iter (function
        | `func (n,r,a) -> 
          printf "%s %s(%s);\n\n" r n (String.concat "," a)
        | `struc (n,f) -> 
          printf "struct %s {\n" n;
          List.iter (fun (f,t) -> printf "  %s %s;\n" t f) f;
          printf "};\n\n"
        | `union (n,f) -> 
          printf "union %s {\n" n;
          List.iter (fun (f,t) -> printf "  %s %s;\n" t f) f;
          printf "};\n\n"
        | `typedef (a,t) -> 
          printf "typedef %s %s\n\n" t a;
        | `enum(n,f) -> 
          printf "enum %s {\n" n;
          List.iter (fun (n,v) -> printf "  %s = %Li;\n" n v) f;
          printf "}\n\n") 
      r

