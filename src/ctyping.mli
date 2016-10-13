open Enums

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

val get_builtin_int_type : CXTypeKind.t -> builtin_int_type option
val get_builtin_float_type : CXTypeKind.t -> builtin_float_type option
val get_builtin_type : CXTypeKind.t -> builtin_type option

