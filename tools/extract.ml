open Printf
open Rresult.R

module L = Log.Make(struct let section = "top" end)

module Clang = Coc_clang.Make(struct let from = None end)
module Cparse = Coc_parse.Make(Clang)

let clang_args = ref []
let code = ref []
let log_level = ref Log.WARN
let std_inc = ref false

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
    "-Istd", Set(std_inc), " include builtin clang search path";
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
  let args = if !std_inc then "-I/usr/lib/clang/3.8/include" :: args else args in
  let builtins = Cparse.[ GBuiltin{name="__builtin_va_list"; typ=TPtr(TVoid)} ] in
  Cparse.run ~log:true ~unsaved:code ~builtins args 

let rec show_type ?(inner=false) = 
  let open Cparse in
  function
  | TVoid -> "void"
  | TNamed(str) -> str
  | TPtr(t) -> sprintf "%s*" (show_type ~inner t)
  | TArray(t, size) -> sprintf "%s[%i]" (show_type ~inner t) size
  | TFuncPtr{ret;args;variadic} -> 
    sprintf "%s (*)(%s%s)"
      (show_type ~inner ret) 
      (String.concat ", " (List.map (fun t -> show_type ~inner t) args))
      (if variadic then ",..." else "")
  | TComp{global=(GComp{name=(name,_);kind});members} ->
    let kind = 
      match kind with
      | Struct -> "struct"
      | Union -> "union"
    in
    let f = function
      | Field{name;typ} ->
        sprintf "%s %s" (show_type ~inner:true typ) name
      | Bitfield{name;typ;width} ->
        sprintf "%s:%i %s" (show_type ~inner:true typ) width name
    in
    sprintf "%s %s { %s }"
      kind name
      (if inner then "" else String.concat "; " (List.map f members));
  | TEnum{global} -> sprintf "enum %s" (name_of_global global)
  | TGlobal global -> sprintf "global %s" (name_of_global global)
  | _ -> failwith "show_type"

let show ctx (_, g) = 
  let open Printf in
  let open Cparse in
  match g with
  | GTypedef{name=(name,_);typ} -> printf "%s = %s\n" name (show_type typ)
  | GComp{name;kind;clayout} -> 
    let members=try TypeMap.find g ctx.comp_members_map with Not_found -> [] in
    printf "%s [%i %i]\n" (show_type (TComp{global=g;members}))
      clayout.size clayout.align;
  | GEnum{name} -> 
    let items, kind = 
      try TypeMap.find g ctx.enum_items_map 
      with Not_found -> [], default_enum_type
    in
    printf "%s\n" (show_type (TEnum{global=g;items;kind}));
  | GVar {name=(name,_);typ} -> printf "%s = %s\n" name (show_type typ)
  | GFunc {name=(name,_);typ} -> printf "%s = %s\n" name (show_type typ)
  | GBuiltin {name;typ} -> printf "[builtin] %s = %s\n" name (show_type typ)

let () = 
  match x with
  | Error _ -> L.fatal "%s" "fatal error."
  | Ok(ctx) -> begin
    L.info "%s" "Ast generation OK.";
    List.iter (show ctx) ctx.Cparse.decls
  end

