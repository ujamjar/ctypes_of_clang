module type Dllib = sig
  val from : Dl.library option
end

module type S = sig

  open Ctypes
  open Coc_enums

  type str
  val str : str typ

  module Str : sig
    val getC : str -> string
    val dispose : str -> unit
    val to_string : ?dispose:bool -> str -> string
  end

  type idx
  val idx : idx typ

  module Index : sig
    val create : int -> int -> idx
    val dispose : idx -> unit
  end

  type file
  val file : file typ

  module File : sig
    val name : file -> string
  end

  type unsaved
  val unsaved : unsaved typ

  module Unsaved : sig
    val set : unsaved:unsaved -> filename:string -> contents:string -> unit
    val make : filename:string -> contents:string -> unsaved
  end

  type loc
  val loc : loc typ

  module Loc : sig
    type t = 
      {
        file : string;
        line : int;
        col : int;
        offset : int;
      }
    val location : loc -> t
  end

  type ctyp
  val ctyp : ctyp typ
  type cursor
  val cursor : cursor typ

  module Type : sig
    val name : ctyp -> string
    val kind : ctyp -> CXTypeKind.t
    val kind_name : CXTypeKind.t -> string
    val declaration : ctyp -> cursor
    val is_const : ctyp -> bool
    val size : ctyp -> int
    val align : ctyp -> int
    val offset : ctyp -> string -> int
    val pointee_type : ctyp -> ctyp
    val elem_type : ctyp -> ctyp
    val array_size : ctyp -> int
    val canonical_type : ctyp -> ctyp
    val is_variadic : ctyp -> bool
    val num_args : ctyp -> int
    val arg_types : ctyp -> ctyp array
    val ret_type : ctyp -> ctyp
    val calling_conv : ctyp -> CXCallingConv.t
  end

  module Cursor : sig
    val equal : cursor -> cursor -> bool
    val hash : cursor -> int
    val is_null : cursor -> bool
    val spelling : cursor -> string
    val kind : cursor -> CXCursorKind.t
    val location : cursor -> loc
    val cur_type : cursor -> ctyp
    val definition : cursor -> cursor
    val canonical : cursor -> cursor
    val bit_width : cursor -> int option
    val enum_type : cursor -> ctyp
    val enum_val : cursor -> int64
    val typedef_type : cursor -> ctyp
    val linkage : cursor -> CXLinkageKind.t
    val num_args : cursor -> int
    val ret_type : cursor -> ctyp
    val args : cursor -> cursor array
    val field_offset : cursor -> int
    val visit : cursor -> (cursor -> cursor -> 'a -> CXChildVisitResult.t * 'a) -> 'a -> 'a
  end

  type tu
  val tu : tu typ

  module TU : sig
    val parse : 
      ?options:CXTranslationUnit_Flags.t list -> 
      unsaved:(string * string) list -> 
      index:idx -> string list -> tu
    val cursor : tu -> cursor
    val dispose : tu -> unit
  end

  type diag
  val diag : diag typ

  module Diag : sig
    val num : tu -> int
    val get : tu -> int -> diag
    val severity : diag -> CXDiagnosticSeverity.t
    val to_string : diag -> string
    val diags : tu -> string array
  end

  type error_msg = Loc.t * string 
  type error_msgs = error_msg list

  val run : 
    ?log:bool ->
    ?pedantic:bool ->
    ?options:CXTranslationUnit_Flags.t list -> 
    ?unsaved:(string * string) list ->
    args:string list -> 
    (tu -> 'a -> 'b) -> 'a -> 
    ('b, error_msgs) result

end

module Make(X : Dllib) = struct

  open Ctypes
  open Foreign
  open Coc_enums

  let foreign = foreign ?from:X.from

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
      (fun file -> if ptr_compare null file = 0 then "???" else Str.to_string (name file))
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
    let make ~filename ~contents = 
      let unsaved = make unsaved in
      set ~unsaved ~filename ~contents;
      unsaved
  end

  type loc'
  type loc = loc' structure
  let loc : loc typ = structure "CXSourceLocation"
  let loc_ptr_data = Array.init 2 (fun i -> field loc ("ptr_data"^string_of_int i) (ptr void))
  let loc_int_data = field loc "int_data" unsigned
  let () = seal loc

  module Loc = struct

    type t = 
      {
        file : string;
        line : int;
        col : int;
        offset : int;
      }

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
        let file = File.name (!@ fp) in
        {
          file;
          line=U.to_int (!@ line); 
          col=U.to_int (!@ col); 
          offset=U.to_int (!@ ofs);
        }

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
      let f = foreign n (ctyp @-> returning llong) in
      f >> Signed.LLong.to_int64 

    let min0 x = if x < 0L then 0L else x

    let c2c n = foreign n (ctyp @-> returning ctyp)

    let c2b n = 
      let f = foreign n (ctyp @-> returning unsigned) in
      (fun t -> if U.to_int (f t) = 0 then false else true)

    let is_const = c2b "clang_isConstQualifiedType" 

    let size = c2ll "clang_Type_getSizeOf" >> min0 >> Int64.to_int

    let align = c2ll "clang_Type_getAlignOf" >> min0 >> Int64.to_int

    let offset = 
      let f = foreign "clang_Type_getOffsetOf" (ctyp @-> string @-> returning llong) in
      (fun t n -> (f t n) |> Signed.LLong.to_int)

    let pointee_type = c2c "clang_getPointeeType" 

    let elem_type = c2c "clang_getArrayElementType"

    let array_size = c2ll "clang_getArraySize" >> Int64.to_int

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
    
    let hash = 
      let hash = foreign "clang_hashCursor" (cursor @-> returning unsigned) in
      (fun c -> U.to_int (hash c))

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

    let field_offset =
      let f = foreign "clang_Cursor_getOffsetOfField" (cursor @-> returning llong) in
      (fun c -> f c |> Signed.LLong.to_int)

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
         int @-> (* num command line args *)
         ptr void @-> (* unsaved_files *)
         int @-> (* num unsaved files *)
         int @-> (* CXTranslationUnit_Flags *)
         returning tu)

    let u' = unsaved
    let parse ?(options=[CXTranslationUnit_Flags.None]) ~unsaved ~index args = 
      let options = List.fold_left (fun flags flag ->
          CXTranslationUnit_Flags.to_int flag lor flags) 0 options
      in
      let unsaved, num_unsaved = 
        if unsaved = [] then null, 0
        else
          let u = 
            List.map (fun (filename,contents) -> Unsaved.make ~filename ~contents) unsaved
          in
          CArray.(to_voidp @@ start @@ of_list u' @@ u), List.length u
      in
      parse' 
        index None
        (CArray.start (CArray.of_list string args))
        (List.length args)
        unsaved
        num_unsaved
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
    
    let severity = 
      let severity = foreign "clang_getDiagnosticSeverity" (diag @-> returning unsigned) in
      CXDiagnosticSeverity.of_int << U.to_int << severity

    let location = foreign "clang_getDiagnosticLocation" (diag @-> returning loc) 

    let to_string =
      let default = foreign "clang_defaultDiagnosticDisplayOptions"
        (void @-> returning unsigned) ()
      in
      let fmt = foreign "clang_formatDiagnostic" (diag @-> unsigned @-> returning str) in
      (fun diag -> fmt diag default |> Str.to_string)

    let diags tu = Array.init (num tu) (to_string << get tu)

  end

  module L = Log.Make(struct let section = "clang" end)

  type error_msg = Loc.t * string 
  type error_msgs = error_msg list

  let errors_and_warnings ~log ~pedantic ~tu = 
    let num = Diag.num tu in
    let rec pmap e = function
      | [] -> List.rev e
      | `error s :: t -> pmap (s::e) t
      | `ignore :: t -> pmap e t
    in
    pmap [] @@
    Array.to_list @@
    Array.init num (fun i ->
      let open CXDiagnosticSeverity in
      let diag = Diag.get tu i in
      let f (l : ('a,out_channel,unit) format -> 'a) is_err = 
        let s = Diag.to_string diag in
        (if log then l "%s" s);
        if is_err then 
          let loc = Loc.location (Diag.location diag) in
          `error (loc,s)
        else 
          `ignore
      in
      match Diag.severity diag with
      | Ignored -> f L.debug false
      | Note -> f L.debug false
      | Warning -> f L.warn pedantic
      | Error -> f L.error true
      | Fatal -> f L.fatal true
    )

  let run ?(log=false) ?(pedantic=false) ?options ?(unsaved=[]) ~args f arg = 
    let index = Index.create 0 0 in
    let tu = TU.parse ?options ~unsaved ~index args in
    let errs = errors_and_warnings ~log ~pedantic ~tu in
    if errs <> [] then begin
      TU.dispose tu;
      Index.dispose index;
      Error errs
    end else
      let result = f tu arg in
      TU.dispose tu;
      Index.dispose index;
      Ok result

end

