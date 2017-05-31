open Printf

open Ctypes_of_clang
module Clang = Coc_clang.Make(struct let from = None end)
module Cinfo = Coc_info.Make(Clang)
open Cinfo

let clang_args = ref []
let code = ref []
let log_level = ref Log.WARN

let log_levels = [
  "debug", Log.DEBUG;
  "info", Log.INFO;
  "warn", Log.WARN;
  "error", Log.ERROR;
  "fatal", Log.FATAL;
]

let () = Arg.(parse
  (align [
    "-code", String (fun s -> code := s :: !code), " convert given code snippet(s)";
    "-log", Symbol(List.map fst log_levels, (fun s -> log_level := List.assoc s log_levels)), 
      " logging level";
    "--", Rest(fun s -> clang_args := s :: !clang_args), " clang arguments";
  ])  
  (fun s -> raise (Bad ("invalid parameter: " ^ s)))
{|
  info [options] -- [clang options] 
|})

let () = 
  Log.set_log_level !log_level;
  Log.color_on ();
  Log.set_output stdout

let code = 
  let code = List.rev !code in
  List.mapi (fun i x -> "code_snippet" ^ string_of_int i ^ ".c", x) code

let x = 
  let args = List.rev !clang_args in
  let args = args @ (List.map fst code) in
  Cinfo.run ~unsaved:code args 

let () = 
  match x with
  | Error _ -> ()
  | Ok r ->
    let show x l n k t = 
      let open Clang.Loc in
      printf "%s %s [%s] [%s] @%s:%i:%i:%i\n" x n k t l.file l.line l.col l.offset
    in
    List.iter (function
      | Function{loc;name;returns;args;kindname;typename} -> 
        show "function" loc name kindname typename;
        printf "  returns: %s\n" returns;
        printf "  args: (%s)\n" (String.concat ", " args)
      | Enum{loc;name;int_type;fields;kindname;typename} -> 
        show "enum" loc name kindname typename;
        printf "  type: %s\n" int_type;
        List.iter (fun (n,v) -> printf "  %s = %Li\n" n v) fields
      | Struct{loc;name;fields;kindname;typename} -> 
        show "struct" loc name kindname typename;
        List.iter (fun (n,t,f) -> printf "  %s %s (kind=%s)\n" t n f) fields
      | Union{loc;name;fields;kindname;typename} -> 
        show "union" loc name kindname typename;
        List.iter (fun (n,t,f) -> printf "  %s %s (kind=%s)\n" t n f) fields
      | Typedef{loc;name;aliases;kindname;typename} -> 
        show "typedef" loc name kindname typename;
        printf "  -> %s\n" aliases
      )
      r

