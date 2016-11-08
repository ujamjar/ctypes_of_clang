(* identifier management *)

module M = Map.Make(String)

(* create ocaml compatible lower case identifier from C-ident. *)
let ocaml_lid s = 
  if s = "" then "anonymous"
  else if Char.uppercase_ascii s.[0] = s.[0] then 
    let has_lowercase s = 
      let b = ref false in
      String.iter (function 'a'..'z' -> b := true | _ -> ()) s;
      !b
    in
    if String.length s = 1 || has_lowercase s then String.uncapitalize_ascii s
    else "_" ^ s
  else s

(* initialise mangler *)
let init l = List.fold_left (fun m a -> M.add a 0 m) M.empty l

(* name mangler - add numeric suffixes for repeated names *)
let rec mangle m s = 
  let suffix n = s ^ "_" ^ string_of_int n in
  match M.find s m with
  | n -> 
    let m = M.add s (n+1) m in
    let s = suffix n in
    mangle m s
  | exception Not_found -> 
    M.add s 0 m, s 

let make init = 
  let mangler = ref init in
  (fun s ->
     let m, s = mangle !mangler s in
     mangler := m;
     s)

let ocaml_keywords = 
  [
    "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
    "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
    "lazy"; "let"; "match"; "method"; "module"; "mutable"; "new"; "object";
    "of"; "open"; "or"; "private"; "rec"; "raise"; "sig"; "struct"; "then"; "to";
    "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";
  ]

let ctypes_keywords = 
  [
    "structure"; "union"; "field"; "returning";
    "void"; "schar"; "uchar"; "short"; "ushort"; "int32_t";
    "uint32_t"; "long"; "ulong"; "llong"; "ullong"; "float";
    "double"; "ptr"; "array";
  ]

let foreign_keywords = [ "foreign"; "foreign_value"; "funptr" ]

