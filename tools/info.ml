open Printf

module Clang = Clang.Make(struct let from = None end)
module Cparse = Cparse.Info(Clang)
open Cparse

let () = 
  let args = List.tl @@ Array.to_list Sys.argv in
  match run args with
  | Error e -> Array.iter print_endline e
  | Ok r ->
    let show x l n k t = 
      printf "%s %s [%s] [%s] @%s:%i:%i:%i\n" x n k t l.file l.line l.col l.offset
    in
    List.iter (function
      | Function{loc;name;returns;args;kindname;typename} -> 
        show "function" loc name kindname typename;
        printf "  returns: %s\n" returns;
        printf "  args: (%s)" (String.concat ", " args)
      | Enum{loc;name;int_type;fields;kindname;typename} -> 
        show "enum" loc name kindname typename;
        printf "  type: %s\n" 
          (match int_type with None -> "?"
                             | Some(x) -> Ctyping.show_builtin_int_type x);
        List.iter (fun (n,v) -> printf "  %s = %Li\n" n v) fields
      | Struct{loc;name;fields;kindname;typename} -> 
        show "struct" loc name kindname typename;
        List.iter (fun (n,t) -> printf "  %s %s\n" t n) fields
      | Union{loc;name;fields;kindname;typename} -> 
        show "union" loc name kindname typename;
        List.iter (fun (n,t) -> printf "  %s %s\n" t n) fields
      | Typedef{loc;name;aliases;kindname;typename} -> 
        show "typedef" loc name kindname typename;
        printf "  -> %s\n" aliases
      )
      r

