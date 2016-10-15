open Printf
open Rresult.R

module Clang = Coc_clang.Make(struct let from = None end)
module Cparse = Coc_parse.Extract(Clang)

let clang_args = ref []
let types = ref false
let fns = ref false
let re = ref ".*"
let code = ref []

let () = Arg.(parse
  (align [
    "-types", Set types, " write types module";
    "-fns", Set fns, " write functions module";
    "-re", Set_string re, " write bindings match regular expression";
    "-code", String (fun s -> code := s :: !code), " convert given code snippet(s)";
    "--", Rest(fun s -> clang_args := s :: !clang_args), " clang arguments";
  ])  
  (fun s -> raise (Bad ("invalid parameter: " ^ s)))
{|
  extract [options] -- [clang options] 
  
  generate ctypes bindings from c source files
|})

let write_types ast = 
  if !types then Coc_gen.write_types Format.std_formatter ast
  else return ()

let write_functions ast =
  if !fns then Coc_gen.write_functions Format.std_formatter ast
  else return ()

let code = 
  let code = List.rev !code in
  List.mapi (fun i x -> "code_snippet" ^ string_of_int i ^ ".c", x) code

let x = 
  let args = List.rev !clang_args in
  let args = args @ (List.map fst code) in
  Cparse.run ~unsaved:code args >>= Coc_gen.run ~regex:!re >>= fun ast ->
  write_types ast >>= fun () ->
  write_functions ast 

let () = 
  match x with
  | Error x -> Array.iter (fprintf stderr "%s\n") x
  | _ -> ()
