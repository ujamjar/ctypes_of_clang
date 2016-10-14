module Print : sig

  val ocaml_keywords : string list

  val correct : string -> string
  
  val typ : Format.formatter -> Coc_extract.typexpr -> unit
  
  val typ_paren : Format.formatter -> Coc_extract.typexpr -> unit
  
  val fn :
    Format.formatter ->
    Coc_extract.typexpr list * Coc_extract.typexpr -> unit
  
  val fn_paren :
    Format.formatter ->
    Coc_extract.typexpr list * Coc_extract.typexpr -> unit
  
  val kind : [< `Struct | `Union ] -> string
  
  val stritem : Format.formatter -> Coc_extract.static_stritem -> unit
  
  val link_time : Format.formatter -> Coc_extract.foreign_stritem -> unit

end

val functions_prologue : (string -> 'a, 'b, 'a) format

val functions_epilogue : ('a, 'b, 'a) format

val types_prologue : ('a, 'b, 'a) format

val types_epilogue : ('a, 'b, 'a) format

val make_module_name : Humane_re.Str.str -> string

val run :
  ?regex:string ->
  unit Coc_extract.Stage1.M.t ->
  (Coc_extract.Stage2.M.state, string array) result

val write_types : Format.formatter -> Coc_extract.Stage2.state' -> (unit, 'a) result

val write_functions : Format.formatter -> Coc_extract.Stage2.state' -> (unit, 'a) result

