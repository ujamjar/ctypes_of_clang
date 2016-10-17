(** Types describing the output. *)
type struct_tag = [`Struct | `Union] * string

type typexpr =
    Array of typexpr * int
  | Unsupported of string
  | Enum of string
  | Function of typexpr list * typexpr
  | Funptr of typexpr list * typexpr
  | Name of string
  | Pointer of typexpr * bool
  | Structured of struct_tag

(* a bit rough... *)
let rec string_of_typexpr t = 
  match t with
  | Array(t,n) -> string_of_typexpr t ^ "[" ^ string_of_int n ^ "]"
  | Unsupported(s) -> s
  | Enum(s) -> "enum " ^ s
  | Function(args,ret) -> string_of_typexpr ret ^ "_(" ^ (String.concat "," (List.map string_of_typexpr args)) ^ ")"
  | Funptr(args,ret) -> string_of_typexpr ret ^ "*(" ^ (String.concat "," (List.map string_of_typexpr args)) ^ ")"
  | Name(s) -> s 
  | Pointer(t,_) -> string_of_typexpr t ^ " *"
  | Structured (t,s) -> (match t with `Struct -> "struct" | `Union -> "union") ^ " " ^ s

