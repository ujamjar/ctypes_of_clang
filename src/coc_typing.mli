open Coc_enums

type struct_tag = [`Struct | `Union] * string

type typexpr =
  | Array of typexpr * int
  | Unsupported of string
  | Enum of string
  | Function of typexpr list * typexpr
  | Funptr of typexpr list * typexpr
  | Name of string
  | Pointer of typexpr * bool
  | Structured of struct_tag

val string_of_typexpr : typexpr -> string

