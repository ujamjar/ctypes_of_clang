open Printf
open Rresult.R

module L = Log.Make(struct let section = "top" end)

module Clang = Coc_clang.Make(struct let from = None end)
module Cparse = Coc_parse.Make(Clang)

let clang_args = ref []
let types = ref false
let fns = ref false
let re = ref ".*"
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
    "-types", Set types, " write types module";
    "-fns", Set fns, " write functions module";
    "-re", Set_string re, " write bindings matching regular expression";
    "-code", String (fun s -> code := s :: !code), " convert given code snippet(s)";
    "-log", Symbol(List.map fst log_levels, (fun s -> log_level := List.assoc s log_levels)), 
      " logging level";
    "--", Rest(fun s -> clang_args := s :: !clang_args), " clang arguments";
  ])  
  (fun s -> raise (Bad ("invalid parameter: " ^ s)))
{|
  extract [options] -- [clang options] 
  
  generate ctypes bindings from c source files
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
  Cparse.run ~unsaved:code args 

let () = 
  match x with
  | Error x -> L.fatal "%s" "fatal error."
  | Ok(ctx) -> begin
    L.info "%s" "Ast generation OK.";
    L.debug "%t" (fun f -> fprintf f "%s" (Cparse.show_ctx ctx));
  end

