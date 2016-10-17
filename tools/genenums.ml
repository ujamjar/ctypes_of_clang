(* extract interesting enums from 'clang-c/Index.h' and write types for them.
   This is used as an early bootstapping process as these enums are then
   used in the library *)
open Printf

module Clang = Coc_clang.Make(struct let from = None end)
module Cinfo = Coc_info.Make(Clang)

let enums_c = [
  "CXAvailabilityKind";
  "CXDiagnosticSeverity";
  "CXLoadDiag_Error";
  "CXDiagnosticDisplayOptions";
  "CXErrorCode";
  "CXTranslationUnit_Flags";
  "CXSaveTranslationUnit_Flags";
  "CXSaveError";
  "CXReparseFlags";
  "CXTUResourceUsageKind";
  "CXCursorKind";
  "CXLinkageKind";
  "CXVisibilityKind";
  "CXLanguageKind";
  "CXTypeKind";
  "CXCallingConv";
  "CXTemplateArgumentKind";
  "CXTypeLayoutError";
  "CXRefQualifiedKind";
  "CX_CXXAccessSpecifier";
  "CX_StorageClass";
  "CXChildVisitResult";
  "CXNameRefFlags";
  "CXTokenKind";
  "CXCompletionChunkKind";
  "CXCodeComplete_Flags";
  "CXCompletionContext";
  "CXVisitorResult";
]

(* these are typedef'd enums ... we'll need to be a bit more sophisticated to
   reconstruct these *)
let enums_t = [
  "CXGlobalOptFlags";
  "CXObjCPropertyAttrKind";
  "CXObjCDeclQualifierKind";
  "CXResult";
  "CXIdxEntityKind";
  "CXIdxEntityCXXTemplateKind";
  "CXIdxAttrKind";
  "CXIdxDeclInfoFlags";
  "CXIdxObjCContainerKind";
  "CXIdxEntityRefKind";
  "CXIndexOptFlags";
]

let shared_prefix a b = 
  let len = min (String.length a) (String.length b) in
  let rec f i = 
    if i = len then len
    else if a.[i] = b.[i] then f (i+1)
    else i
  in
  f 0

let drop_field_prefix name fields = 
  let pre = List.fold_left (fun a (n,_) -> min a (shared_prefix name n)) (String.length name) fields in
  List.map 
    (fun (n,v) ->
      let drop s n = String.sub s n (String.length s - n) in
      let n = 
        try 
          if n.[pre] = '_' then drop n (pre+1) 
          else if n.[pre] <> Char.uppercase_ascii n.[pre] then raise Not_found
          else drop n pre
        with _ -> n
      in
      n, v
    ) 
    fields

let skip_hacks = 
  let prefix n l = List.map (fun m -> n,m) l in
  List.flatten 
    [
      prefix "CXTUResourceUsageKind" [ 
        "MEMORY_IN_BYTES_BEGIN";
        "MEMORY_IN_BYTES_END";
        "First";
        "Last";
      ];
      prefix "CXCursorKind" [
        "FirstDecl";
        "LastDecl";
        "FirstRef";
        "LastRef";
        "FirstInvalid";
        "LastInvalid";
        "FirstExpr";
        "LastExpr";
        "FirstStmt";
        "GCCAsmStmt";
        "LastStmt";
        "FirstAttr";
        "LastAttr";
        "MacroInstantiation";
        "FirstPreprocessing";
        "LastPreprocessing";
        "FirstExtraDecl";
        "LastExtraDecl";
      ];
      prefix "CXTypeKind" [
        "FirstBuiltin";
        "LastBuiltin";
      ]
    ]

let write_enum_module name fields = 
  let fields = drop_field_prefix name fields in
  let skip n = List.mem (name,n) skip_hacks in
  printf "module %s = struct\n\n" name;
  printf "  type t = \n";
  List.iter (fun (n,v) -> printf "    | %s\n" n) fields;
  printf "\n";
  printf "  let to_int64 = function\n";
  List.iter (fun (n,v) -> printf "    | %s -> %sL\n" n (Int64.to_string v)) fields;
  printf "\n";
  printf "  let of_int64 = function\n";
  List.iter (fun (n,v) -> if not (skip n) then 
                          printf "    | %sL -> %s\n" (Int64.to_string v) n) fields;
  printf "    | _ -> failwith \"%s.to_int\"\n\n" name;
  printf "  let of_int x = of_int64 (Int64.of_int x)\n\n";
  printf "  let to_int x = Int64.to_int (to_int64 x)\n\n";
  printf "  let to_string = function\n";
  List.iter (fun (n,v) -> printf "    | %s -> \"%s\"\n" n n) fields;
  printf "\n";
  printf "end\n\n"

let write_enum_signature name fields =
  let fields = drop_field_prefix name fields in
  printf "module %s : sig\n\n" name;
  printf "  type t = \n";
  List.iter (fun (n,v) -> printf "    | %s\n" n) fields;
  printf "\n";
  printf "  val to_int64 : t -> int64\n";
  printf "  val of_int64 : int64 -> t\n";
  printf "  val to_int : t -> int\n";
  printf "  val of_int : int -> t\n";
  printf "  val to_string : t -> string\n";
  printf "\nend\n\n"

let write_ml = ref false
let write_mli = ref false
let () = Arg.parse
  [
    "-ml", Arg.Set write_ml, "write .ml file to stdout";
    "-mli", Arg.Set write_mli, "write .mli file to stdout";
  ]
  (fun _ -> ()) ""

let _ = 
  let args = 
    [
      "-I/usr/lib/llvm-3.8/include";
      "-I/usr/lib/clang/3.8.0/include";
      "/usr/lib/llvm-3.8/include/clang-c/Index.h"
    ]
  in
  match Cinfo.run args with
  | Error () -> ()
  | Ok r ->
    List.iter 
      (function
        | Cinfo.Enum{name; fields} when List.mem name enums_c -> begin
            begin if !write_ml then write_enum_module name fields end;
            begin if !write_mli then write_enum_signature name fields end
        end
        | _ -> ())
      (List. rev r)

