open Ctypes
open Foreign
open Enums

module U = Unsigned.UInt
let unsigned = uint
type pvoid = unit ptr 
module Kind = struct
  let t = unsigned
end

let (<<) f g x = f (g x)
let (>>) f g x = g (f x)

type str'
type str = str' structure
let str : str typ = structure "CXString"
let str_data = field str "data" (ptr void)
let str_private_flags = field str "private_flags" unsigned
let () = seal str

module Str = struct

  let getC = foreign "clang_getCString" (str @-> returning @@ string)
  let dispose' = foreign "clang_disposeString" (str @-> returning void)

  let to_string ?(dispose=true) s = 
    let c = getC s in
    (if dispose then dispose' s);
    c

  let dispose = dispose'

end

type idx = pvoid
let idx = ptr void

module Index = struct
  let create = foreign "clang_createIndex" (int @-> int @-> returning idx)
  let dispose = foreign "clang_disposeIndex" (idx @-> returning void)

end

type file = pvoid
let file = ptr void 

module File = struct
  let name = 
    let name = foreign "clang_getFileName" (file @-> returning str) in
    Str.to_string << name
end

type unsaved'
type unsaved = unsaved' structure
let unsaved : unsaved typ = structure "CXUnsavedFile"
let unsaved_filename = field unsaved "Filename" string
let unsaved_contents = field unsaved "Contents" string
let unsaved_length = field unsaved "Length" ulong
let () = seal unsaved

module Unsaved = struct
  let set ~unsaved ~filename ~contents = 
    setf unsaved unsaved_filename filename;
    setf unsaved unsaved_contents contents;
    setf unsaved unsaved_length (Unsigned.ULong.of_int (String.length contents))
end

type loc'
type loc = loc' structure
let loc : loc typ = structure "CXSourceLocation"
let loc_ptr_data = Array.init 2 (fun i -> field loc ("ptr_data"^string_of_int i) (ptr void))
let loc_int_data = field loc "int_data" unsigned
let () = seal loc

module Loc = struct

  let location = 
    let loc = foreign "clang_getSpellingLocation"
      (loc @-> ptr file @-> ptr unsigned @-> ptr unsigned @-> ptr unsigned @-> returning void)
    in
    fun s -> 
      let fp = allocate file null in
      let line = allocate unsigned U.zero in
      let col = allocate unsigned U.zero in
      let ofs = allocate unsigned U.zero in
      let () = loc s fp line col ofs in
      !@ fp, U.to_int (!@ line), U.to_int (!@ col), U.to_int (!@ ofs)

end

type ctyp'
type ctyp = ctyp' structure
let ctyp : ctyp typ = structure "CXType"
let ctyp_kind = field ctyp "kind" Kind.t
let ctyp_data = Array.init 2 (fun i -> field ctyp ("data" ^ string_of_int i) (ptr void))
let () = seal ctyp

type cursor'
type cursor = cursor' structure
let cursor : cursor typ = structure "CXCursor"
let cursor_kind = field cursor "kind" Kind.t
let cursor_xdata = field cursor "xdata" int
let cursor_data = Array.init 3 (fun i -> field cursor ("data" ^ string_of_int i) (ptr void))
let () = seal cursor

module Type = struct

  let name = 
    let spelling = foreign "clang_getTypeSpelling" (ctyp @-> returning str) in
    (Str.to_string << spelling)

  let kind s = getf s ctyp_kind |> (CXTypeKind.of_int64 << U.to_int64)

  let kind_name = 
    let name = foreign "clang_getTypeKindSpelling" (Kind.t @-> returning str) in
    Str.to_string << name << U.of_int64 << CXTypeKind.to_int64

  let declaration = foreign "clang_getTypeDeclaration" (ctyp @-> returning cursor)

  let c2ll n = 
    let f = foreign "clang_Type_getSizeOf" (ctyp @-> returning llong) in
    f >> Signed.LLong.to_int64 

  let min0 x = if x < 0L then 0L else x

  let c2c n = foreign n (ctyp @-> returning ctyp)

  let c2b n = 
    let f = foreign n (ctyp @-> returning unsigned) in
    (fun t -> if U.to_int (f t) = 0 then false else true)

  let is_const = c2b "clang_isConstQualifiedType" 

  let size = c2ll "clang_Type_getSizeOf" >> min0

  let align = c2ll "clang_Type_getAlignOf" >> min0

  let pointee_type = c2c "clang_getPointeeType" 

  let elem_type = c2c "clang_getArrayElementType"

  let array_size = c2ll "clang_getArraySize"

  let canonical_type = c2c "clang_getCanonicalType"

  let is_variadic = c2b "clang_isFunctionTypeVariadic"

  let num_args = foreign "clang_getNumArgTypes" (ctyp @-> returning int)

  let arg_types = 
    let get = foreign "clang_getArgType" (ctyp @-> int @-> returning ctyp) in
    (fun t -> Array.init (num_args t) (get t))

  let ret_type = c2c "clang_getResultType"

  let calling_conv = 
    let cc = foreign "clang_getFunctionTypeCallingConv" (ctyp @-> returning unsigned) in
    cc >> U.to_int64 >> CXCallingConv.of_int64

end

module Cursor = struct
  
  let equal = 
    let eq = foreign "clang_equalCursors" (cursor @-> cursor @-> returning unsigned) in
    (fun a b -> if (U.to_int (eq a b)) = 0 then false else true)
  
  let is_null = 
    let is_null = foreign "clang_Cursor_isNull" (cursor @-> returning int) in
    is_null >> ((<>)0)

  let spelling = 
    let spelling = foreign "clang_getCursorSpelling" (cursor @-> returning str) in
    Str.to_string << spelling 

  let kind = 
    let kind = foreign "clang_getCursorKind" (cursor @-> returning unsigned) in
    CXCursorKind.of_int64 << U.to_int64 << kind

  let location = foreign "clang_getCursorLocation" (cursor @-> returning loc)

  let cur_type = foreign "clang_getCursorType" (cursor @-> returning ctyp) 

  let definition = foreign "clang_getCursorDefinition" (cursor @-> returning cursor) 

  let canonical = foreign "clang_getCanonicalCursor" (cursor @-> returning cursor) 

  let bit_width c = 
    let f = foreign "clang_getFieldDeclBitWidth" (cursor @-> returning int) in
    match f c with
    | -1 -> None
    | x -> Some(x)

  let enum_type = foreign "clang_getEnumDeclIntegerType" (cursor @-> returning ctyp)

  let enum_val = 
    let f = foreign "clang_getEnumConstantDeclValue" (cursor @-> returning llong) in
    Signed.LLong.to_int64 << f

  let typedef_type = foreign "clang_getTypedefDeclUnderlyingType" (cursor @-> returning ctyp)

  let linkage = 
    let f = foreign "clang_getCursorLinkage" (cursor @-> returning Kind.t) in
    CXLinkageKind.of_int64 << U.to_int64 << f

  let num_args = foreign "clang_Cursor_getNumArguments" (cursor @-> returning int)

  let ret_type = foreign "clang_getCursorResultType" (cursor @-> returning ctyp)

  let get_arg = foreign "clang_Cursor_getArgument" (cursor @-> int @-> returning cursor)

  let args c = Array.init (num_args c) (fun i -> get_arg c i)

  let callback = cursor @-> cursor @-> ptr void @-> returning Kind.t
  let visit_kids = foreign "clang_visitChildren"
    (cursor @-> funptr callback @-> ptr void @-> returning unsigned)

  let visit cursor f arg = 
    let data = ref arg in
    let wrap_visitor f data = 
      (fun cursor parent _ -> 
         let r, d = f cursor parent !data in
         data := d;
         CXChildVisitResult.to_int r |> U.of_int)
    in
    let _ = visit_kids cursor (wrap_visitor f data) null in
    !data

end

type tu'
type tu = tu' structure ptr
let tu : tu typ = ptr (structure "CXTranslationUnitImpl")

module TU = struct

  let parse' = foreign "clang_parseTranslationUnit"
      (idx @-> 
       string_opt @-> (* filename *)
       ptr string @-> (* command line args *)
       int @-> (* numb command line args *)
       ptr void @-> (* unsaved_files *)
       int @-> (* num unsaved files *)
       int @-> (* CXTranslationUnit_Flags *)
       returning tu)

  let parse ?(options=[CXTranslationUnit_Flags.None]) ~index args = 
    let options = List.fold_left (fun flags flag ->
        CXTranslationUnit_Flags.to_int flag lor flags) 0 options
    in
    parse' 
      index None
      (CArray.start (CArray.of_list string args))
      (List.length args)
      null
      0
      options

  let cursor = foreign "clang_getTranslationUnitCursor" (tu @-> returning cursor)

  let dispose = foreign "clang_disposeTranslationUnit" (tu @-> returning void)

end

type diag = pvoid
let diag = ptr void

module Diag = struct
  
  let num = U.to_int << foreign "clang_getNumDiagnostics" (tu @-> returning unsigned)
  
  let get = 
    let get = foreign "clang_getDiagnostic" (tu @-> unsigned @-> returning diag) in
    (fun tu i -> get tu (U.of_int i))
  
  let to_string =
    let default = foreign "clang_defaultDiagnosticDisplayOptions"
      (void @-> returning unsigned) ()
    in
    let fmt = foreign "clang_formatDiagnostic" (diag @-> unsigned @-> returning str) in
    (fun diag -> fmt diag default |> Str.to_string)

  let diags tu = Array.init (num tu) (to_string << get tu)

end

let run ?options args f arg = 
  let index = Index.create 0 0 in
  let tu = TU.parse ?options ~index args in
  if Diag.num tu <> 0 then begin
    let diags = Diag.diags tu in
    TU.dispose tu;
    Error diags
  end else
    let result = f tu arg in
    TU.dispose tu;
    Index.dispose index;
    Ok result


