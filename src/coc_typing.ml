open Coc_enums

type builtin_int_type = 
  | Char_U 
  | UChar 
  | Char16 
  | Char32 
  | UShort 
  | UInt 
  | ULong 
  | ULongLong 
  | Char_S 
  | SChar 
  | WChar 
  | Short 
  | Int 
  | Long 
  | LongLong 
  [@@deriving show]

type builtin_float_type = 
  | Float 
  | Double 
  [@@deriving show]

type builtin_type = 
  | B_Void
  | B_Int of builtin_int_type
  | B_Float of builtin_float_type
  [@@deriving show]

let get_builtin_int_type ctyp = 
  match ctyp with
  | CXTypeKind.Char_U    -> Some Char_U 
  | CXTypeKind.UChar     -> Some UChar 
  | CXTypeKind.Char16    -> Some Char16 
  | CXTypeKind.Char32    -> Some Char32 
  | CXTypeKind.UShort    -> Some UShort 
  | CXTypeKind.UInt      -> Some UInt 
  | CXTypeKind.ULong     -> Some ULong 
  | CXTypeKind.ULongLong -> Some ULongLong 
  | CXTypeKind.Char_S    -> Some Char_S 
  | CXTypeKind.SChar     -> Some SChar 
  | CXTypeKind.WChar     -> Some WChar 
  | CXTypeKind.Short     -> Some Short 
  | CXTypeKind.Int       -> Some Int 
  | CXTypeKind.Long      -> Some Long 
  | CXTypeKind.LongLong  -> Some LongLong 
  | _ -> None

let get_builtin_float_type ctyp = 
  match ctyp with
  | CXTypeKind.Float     -> Some Float 
  | CXTypeKind.Double    -> Some Double 
  | _ -> None

let get_builtin_type ctyp = 
  match get_builtin_int_type ctyp with
  | Some(x) -> Some (B_Int x)
  | None ->
    match get_builtin_float_type ctyp with
    | Some(x) -> Some (B_Float x)
    | None ->
      match ctyp with
      | CXTypeKind.Void -> Some B_Void
      | _ -> None


