open Coc_extract
open Coc_typing

let with_open_out filename f =
  let fd = open_out filename in
  try f (Format.formatter_of_out_channel fd); flush fd; close_out fd
  with e -> close_out fd; raise e

let fprintf = Format.fprintf
module Print =
struct
  let ocaml_keywords = [
    "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
    "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
    "lazy"; "let"; "match"; "method"; "module"; "mutable"; "new"; "object";
    "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to";
    "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";
  ]

  let correct name =
    let lowered = String.uncapitalize_ascii name in
    if List.mem lowered ocaml_keywords then "kw_" ^ lowered
    else lowered

  let rec typ fmt = function
      Array (typ, e) -> fprintf fmt "array@ %d@ @ %a" e typ_paren typ
    | Unsupported _ -> assert false
    | Enum s | Name s | Structured (_, s) -> fprintf fmt "%s" (correct s)
    | Function (args, rv) -> fn fmt (args, rv)
    | Funptr (args, rv) -> fprintf fmt "static_funptr@ %a" fn_paren (args, rv)
    | Pointer t -> fprintf fmt "ptr@ %a" typ_paren t
  and typ_paren fmt = function
    | Enum _ | Name _ | Structured _ | Unsupported _ as t -> typ fmt t
    | Array _ | Funptr _ | Pointer _ as t -> fprintf fmt "(@[%a)@]" typ t
    | Function (args, rv) -> fn_paren fmt (args, rv)
  and fn fmt = function
      ([], rv) -> fprintf fmt "returning@ %a" typ_paren rv
    | (arg :: args, rv) -> fprintf fmt "%a@ @->@ %a" typ arg fn (args, rv)
  and fn_paren fmt = function
    | ([], rv) -> fprintf fmt "(@[void@ @->@ %a)@]" fn ([], rv)
    | (args, rv) -> fprintf fmt "(@[%a)@]" fn (args, rv)

  let kind = function
      `Struct -> "structure"
    | `Union -> "union"

  let stritem fmt : static_stritem -> unit = function
      Typedef (name, Function (args, r)) ->
      (* typedef doesn't support fn; just rename *)
      fprintf fmt "@[<hov 2>let@ %s@ =@ @[%a@]@]@\n@\n"
        (correct name) fn_paren (args, r)
    | Typedef (name, t) ->
      fprintf fmt "@[<hov 2>let@ %s@ =@ typedef@ @[%a@]@ %S@]@\n@\n"
        (correct name) typ_paren t name
    | Structured (k, tag) ->
      fprintf fmt "@[<hov 2>let@ %s@ : [`%s] %s typ =@ %s@ @[%S@]@]@\n" 
        (correct tag) (correct tag) (kind k) (kind k) tag;
    | Field (`Tag tag, (name, t)) ->
      fprintf fmt "@[<hov 2>let@ %s@ =@ field@ %s@ %S@ @[%a@]@]@\n"
        (correct name) (correct tag) name typ_paren t
    | Field (`Typedef td, (name, t)) ->
      fprintf fmt "@[<hov 2>let@ %s@ =@ field@ %s@ %S@ @[%a@]@]@\n"
        (correct name) (correct td) name typ_paren t
    | Seal tag ->
      fprintf fmt "@[<hov 2>let@ ()@ =@ seal@ @[%s@]@]@\n@\n" (correct tag)
    | Enum (name, None) ->
      (* TODO: this isn't quite right.  If we only have a forward declaration then 
         we can't retrieve the size and alignment.
      *)
      fprintf fmt "enum %s []@\n" name
    | Enum (name, Some items) ->
      List.iter (fun item ->
          fprintf fmt "@[<hov 2>let@ %s@ =@ constant@ %S@ int64_t@]@\n" (correct item) item) items;
      fprintf fmt "@[<hov 2>let@ %s@ :@ [%s]@ typ@ =@ enum@ %S @[[" (correct name)
        (String.concat " | " (List.map (fun n -> "`"^ n) items))
        name;
      List.iter (fun item -> fprintf fmt "(@[`%s, %s);@]" item (correct item)) items;
      fprintf fmt "]@]@]@\n@\n"

  let link_time fmt = function
    | Foreign (name, Function (args, rv)) ->
      fprintf fmt "@[<hov 2>let@ %s@ =@ foreign@ %S@ @[%a@]@]@\n@\n"
        (correct name) name fn_paren (args, rv)
    | Foreign (name, typ) ->
      fprintf fmt "@[<hov 2>let@ %s@ =@ foreign_value@ %S@ @[%a@]@]@\n@\n"
        (correct name) name typ_paren typ
end

let functions_prologue : (_,_,_) format = "
open Ctypes@\n@\n\
module T = %s.Bindings(Generated_types)@\n\
open T@\n@\n\
module Bindings(F: Cstubs.FOREIGN) =@\n\
@[<hov 2>struct@\n\
open F@\n"
and functions_epilogue : (_,_,_) format = "@]@\nend@\n"
and types_prologue : (_, _, _) format = "
open Ctypes@\n@\n\
module Bindings(T: Cstubs.Types.TYPE) =@\n\
@[<hov 2>struct@\n\
open T@\n"
(*"let ssize_t = lift_typ PosixTypes.ssize_t@\n"*) (* why? *)
and types_epilogue : (_,_,_) format = "@]@\nend@\n"

let make_module_name filename =
  match Humane_re.Str.(split (regexp "\\.")) filename with
    [] -> assert false
  | base :: _ -> String.capitalize_ascii base

let run ?(regex=".*") ast = 
  let foreign_regex = Humane_re.Str.regexp regex in
  match Stage1.M.run ast Stage1.initial_state with
    `Result ((), (namespace : Stage1.M.state)) ->
    let map_name_state = function
      | Builtin -> Stage2.Declared
      | Aliases _ -> Stage2.Undeclared
      | Unsupported _ -> Stage2.Undeclarable 
    in
    let transform_state = let open Assoc in function
      | UItem (Name_ n, s, ()) -> UItem (Name_ n, s, map_name_state s)
      | UItem (k, v, ()) -> UItem (k, v, Stage2.Undeclared) 
    in
    let initial_cache_state = 
      Stage2.{
        stritems = []; fstritems = [];
        namespace = List.map transform_state namespace.Stage1.statics 
      } 
    in
    let m = 
      Stage2.(M.(List.fold_left 
        (fun m (name, typ) ->
          if Humane_re.Str.matches foreign_regex name then
          (m >> record_foreign name typ) 
          else m) 
        (return ())
        namespace.Stage1.foreigns)) 
    in
    begin match Stage2.M.run m initial_cache_state with
    | `Result ((),x) -> Ok x
    | `Failure x -> Error [|x|] end
  | `Failure s -> failwith s

let write_types types_fmt {Stage2.stritems; fstritems} = 
  Format.fprintf types_fmt types_prologue;
  List.iter (Print.stritem types_fmt) (List.rev stritems);
  Format.fprintf types_fmt types_epilogue;
  Ok ()

let write_functions functions_fmt {Stage2.stritems; fstritems} =
  let types_module = "FOO" in (* XXX *)
  Format.fprintf functions_fmt functions_prologue types_module;
  List.iter (Print.link_time functions_fmt) (List.rev fstritems);
  Format.fprintf functions_fmt functions_epilogue;
  Ok ()

