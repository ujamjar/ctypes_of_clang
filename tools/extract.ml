open Printf
open Rresult.R

module L = Log.Make(struct let section = "top" end)

module Clang = Coc_clang.Make(struct let from = None end)
module Cparse = Coc_parse.Make(Clang)

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
  Cparse.run ~log:true ~unsaved:code args 

let rec show_type ?(inner=false) = 
  let open Cparse in
  function
  | TVoid -> "void"
  | TInt(IBool) -> "bool"
  | TInt(ISChar) -> "schar"
  | TInt(IUChar) -> "uchar"
  | TInt(IShort) -> "sshort"
  | TInt(IUShort) -> "ushort"
  | TInt(IInt) -> "int"
  | TInt(IUInt) -> "uint"
  | TInt(ILong) -> "long"
  | TInt(IULong) -> "ulong"
  | TInt(ILongLong) -> "llong"
  | TInt(IULongLong) -> "ullong"
  | TInt(IWChar) -> "wchar"
  | TFloat(FFloat) -> "float"
  | TFloat(FDouble) -> "double"
  | TPtr(t,_) -> sprintf "%s*" (show_type ~inner t)
  | TArray(t, size) -> sprintf "%s[%Li]" (show_type ~inner t) size
  | TFuncProto (f) -> 
    sprintf "%s (_)(%s%s)"
      (show_type ~inner f.fs_ret) 
      (String.concat ", " (List.map (fun (_,t) -> show_type ~inner t) f.fs_args))
      (if f.fs_variadic then ",..." else "")
  | TFuncPtr(f) -> 
    sprintf "%s (*)(%s%s)"
      (show_type ~inner f.fs_ret) 
      (String.concat ", " (List.map (fun (_,t) -> show_type ~inner t) f.fs_args))
      (if f.fs_variadic then ",..." else "")
  | TNamed(t) ->
    sprintf "%s = %s" t.ti_name (show_type ~inner t.ti_typ)
  | TComp(ci) ->
    let kind = function
      | Struct -> "struct"
      | Union -> "union"
    in
    let f = function
      | Field(f) -> sprintf "%s %s" (show_type ~inner:true f.fi_typ) f.fi_name
      | Comp(ci) -> show_type ~inner:true (TComp ci)
      | Enum(ei) -> show_type ~inner:true (TEnum ei)
      | CompField(ci,f) -> show_type ~inner:true (TComp ci)
      | EnumField(ei,f) -> show_type ~inner:true (TEnum ei)
    in
    sprintf "%s %s { %s }"
      (kind ci.ci_kind) ci.ci_name
      (if inner then "" else String.concat "; " (List.map f @@ List.rev ci.ci_members));
  | TEnum(ei) -> sprintf "enum %s" ei.ei_name

let show g = 
  let open Printf in
  let open Cparse in
  match g with
  | GType(ti) -> printf "%s\n" (show_type (TNamed ti))
  | GComp(ci) -> printf "%s\n" (show_type (TComp(ci)));
  | GCompDecl(ci) -> printf "%s\n" (show_type (TComp(ci)));
  | GEnum(ci) -> printf "%s\n" (show_type (TEnum(ci)));
  | GEnumDecl(ci) -> printf "%s\n" (show_type (TEnum(ci)));
  | GVar _ -> printf "GVar\n"
  | GOther -> printf "GOther\n"
  | GFunc v -> printf "%s = %s\n" v.vi_name (show_type v.vi_typ)

let () = 
  match x with
  | Error _ -> L.fatal "%s" "fatal error."
  | Ok(ctx) -> begin
    L.info "%s" "Ast generation OK.";
    List.iter show (List.rev ctx.Cparse.globals)
  end

