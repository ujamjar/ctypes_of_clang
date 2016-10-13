
(* Type definitions *)

(** Equality *)
type (_, _) eql = Refl : ('a, 'a) eql

(** fromMaybe *)
let default_to default = function Some s -> s | None -> default

(** A monad with state and exceptions.  Raising an exception rolls back
    state changes. *)
module Make(X: sig type state end) :
sig
  type state = X.state
  type _ t

  (* Sequencing *)
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : unit t -> 'a t -> 'a t

  (* State *)
  val get : state t
  val put : state -> unit t

  (* Exceptions *)
  val failf : ('a, unit, string, 'b t) format4 -> 'a
  val handle : 'a t -> catch:([`Failure of string] -> 'a t) -> 'a t

  val run : 'a t -> state -> [ `Result of 'a * state | `Failure of string ]
end =
struct
  type state = X.state
  type error = [ `Failure of string ]
  type 'a t = state -> [ `Result of 'a * state | error ]
                       
  let return v s = `Result (v, s)
  let (>>=) m k s =
    match m s with
    | `Result (a, s) -> k a s
    | #error as e -> e

  let (>>) m n = m >>= fun () -> n

  let fail s _ = `Failure s
  let handle m ~catch s =
    match m s with `Result _ as r -> r | #error as e -> catch e s

  let get : state t = fun s -> `Result (s, s)
  let put s : unit t = fun _ -> `Result ((), s)
  let failf fmt = Printf.kprintf (fun s -> fun _ -> `Failure s) fmt

  let run f = f
end

(** Types describing the output. *)
type struct_tag = [`Struct | `Union] * string

type typexpr =
    Array of typexpr * int
  | Unsupported of string
  | Enum of string
  | Function of typexpr list * typexpr
  | Funptr of typexpr list * typexpr
  | Name of string
  | Pointer of typexpr
  | Structured of struct_tag

type static_stritem =
    Enum of string * string list option
  | Structured of struct_tag
  | Typedef of string * typexpr
  | Field of [`Tag of string | `Typedef of string] * (string * typexpr)
  | Seal of string

type foreign_stritem =
    Foreign of string * typexpr

(* [TODO: document] *)
type enum = {
  etypedef: string option;
  eitems : string list;
}
type struct_ = {
  stypedef: string option;
  sfields: (string * typexpr) list;
}
type name = Aliases of typexpr | Unsupported of string | Builtin

type _ key =
    Enum_ : string -> enum key
  | Structured_ : struct_tag -> struct_ key
  | Name_ : string -> name key

let show_key : type a. a key -> string = function
    Enum_ s -> Printf.sprintf "enum %s" s
  | Structured_ (`Struct, tag) ->  Printf.sprintf "struct %s" tag
  | Structured_ (`Union, tag) ->  Printf.sprintf "union %s" tag
  | Name_ n ->  Printf.sprintf "name %s" n

(** Typed association lists *)
module Assoc =
struct
  type 'data uitem = UItem : 'a key * 'a * 'data -> 'data uitem
  type 'data map = 'data uitem list

  let empty = []

  let add k v data map = UItem (k, v, data) :: map

  let rec find : type a. a key -> 'data map -> a * 'data =
    fun k map -> match k, map with
        _, [] -> raise Not_found
      | Enum_ l, UItem (Enum_ r, v, d) :: _ when l = r -> (v, d)
      | Structured_ l, UItem (Structured_ r, v, d) :: _ when l = r -> (v, d)
      | Name_ l, UItem (Name_ r, v, d) :: _ when l = r -> (v, d)
      | _, _ :: map -> find k map
end

(** Stage 1: collate information about types, which may be distributed across
    several declarations.  For example, the following three declarations all
    contain information relating to 'struct struct_t':

       struct struct_t;
       typedef struct struct_t t;
       struct struct_t { int x, y; };
*)
module Stage1 =
struct
  type state' = { statics: unit Assoc.map;
                  foreigns: (string * typexpr) list }
  module M = Make(struct type state = state' end)
  open M

  let initial_state : state' =
    let statics = List.fold_right (fun k -> Assoc.add (Name_ k) Builtin ()) [
      "void"; "char";
      "schar"; "short"; "int"; "long"; "llong";
      "int8_t"; "int16_t"; "int32_t"; "int64_t"; "intptr_t"; "ptrdiff_t";
      "bool"; "uchar"; "ushort"; "uint"; "ulong"; "ullong";
      "uint8_t"; "uint16_t"; "uint32_t"; "uint64_t"; "uintptr_t"; "size_t";
      "float"; "double";
      ] Assoc.empty in
    { statics; foreigns = [] }

  let update key f =
    get >>= fun s ->
    let value = 
      match Assoc.find key s.statics with
        value, _ -> f (Some value)
      | exception Not_found -> f None
    in
    put {s with statics = Assoc.add key value () s.statics }

  let default_struct_status = {stypedef = None; sfields = []}

  let record_struct_tag tag =
    update (Structured_ tag) (default_to default_struct_status)

  let record_struct_typedef tag typedef =
    update (Structured_ tag) @@ function
      Some s -> { s with stypedef = Some typedef }
    | None -> { default_struct_status with stypedef = Some typedef }

  let add_struct_fields tag sfields : _ t =
    update (Structured_ tag) @@ function
      Some s -> { s with sfields = sfields @ s.sfields }
    | None -> { default_struct_status with sfields }

  let default_enum_status = { etypedef = None; eitems = [] }

  let record_enum_tag name =
    update (Enum_ name) (default_to default_enum_status)

  let record_enum_items name items =
    update (Enum_ name) @@ function
      Some status -> Assoc.{ status with eitems = items @ status.eitems }
    | None -> Assoc.{default_enum_status with eitems = items }

  let record_typedef typedef typ =
    update (Name_ typedef) (default_to (Aliases typ)) >>
    match typ with
      Enum name -> (update (Enum_ name) @@ function
        Some s -> { s with etypedef = Some typedef }
      | None -> { default_enum_status with etypedef = Some typedef })
    | Structured tag -> (update (Structured_ tag) @@ function
        Some s -> { s with stypedef = Some typedef }
      | None -> { default_struct_status with stypedef = Some typedef })
    | _ -> return ()

  let record_foreign key ftyp =
    get >>= fun s -> put {s with foreigns = (key, ftyp) :: s.foreigns}
end

(** Stage 2: collect stritems for the output.

    A foreign declaration can be included in the output if all the types
    involved can be declared (or forward declared, as appropriate).

    Types declarations are included in the output only if necessary for some
    foreign declaration. *)
module Stage2 =
struct
  type included = [`Included | `Unsupported of string | `Undetermined]
  type enum = {
    etypedef: string option;
    eitems : string list;
    eincluded: included
  }
  type struct_ = {
    stypedef: string option;
    sfields: (string * typexpr) list;
    sincluded: included
  }
  type name = { nincluded: included }

  type output_status = Undeclared | ForwardDeclared | Declared | Undeclarable

  module Value = struct type 'a t = 'a * output_status end

  type state' = {
    stritems: static_stritem list;
    fstritems: foreign_stritem list;
    namespace: output_status Assoc.map;
  }

  module M = Make(struct type state = state' end)
  open M

  let push_stritem stritem =
    get >>= fun s -> put { s with stritems = stritem :: s.stritems }

  let push_foreign foreign =
    get >>= fun s -> put { s with fstritems = foreign :: s.fstritems }

  let rec sequence = function
      [] -> return ()
    | m :: ms -> m >> (sequence ms)

  let rec set_status : type a. a key -> output_status -> unit t = 
    fun key status ->
      get >>= fun s ->
      let (item, _) = Assoc.find key s.namespace in
      put { s with namespace = Assoc.add key item status s.namespace }
  and declare : type a. a key -> unit t =
    fun key ->
      get >>= fun {namespace; stritems} ->
      let s = match key, Assoc.find key namespace with
        exception Not_found -> failf "Not supported: %s" (show_key key)
        | _, (_, Declared) -> return ()
        | _, (_, Undeclarable) -> failwith "undeclarable"
        | Enum_ name, _ when Humane_re.Str.(matches (regexp "^__anon") name) ->
          failf "Not supported: anonymous enums"
        | Enum_ name, ({etypedef; eitems}, ForwardDeclared) ->
          (* (we only forward-declare enums if they can't be fully declared) *)
          failwith "undeclarable enum"
        | Enum_ name, ({eitems}, Undeclared) -> (* TODO: handle enum typedefs *)
          push_stritem (Enum (name, Some eitems))
        | Structured_ (_, tag), ({sfields=[]}, _) -> failf "Struct with no fields: %s" tag
        | Structured_ (_, tag), ({stypedef = None; sfields}, ForwardDeclared) ->
          (* (attempt to) add the fields *)
          declare_fields (`Tag tag) sfields >>
          push_stritem (Seal tag)
        | Structured_ ((_, tag) as stag), ({stypedef = Some typedef; sfields}, ForwardDeclared) ->
          (* (attempt to) add the fields *)
          push_stritem (Typedef (typedef, Structured stag)) >>
          set_status (Name_ typedef) Declared >>
          declare_fields (`Typedef typedef) sfields >>
          push_stritem (Seal typedef)
        | Structured_ ((_, tag) as stag), ({stypedef = None; sfields}, Undeclared) ->
          (* forward-declare, then attempt to add the fields *) 
          push_stritem (Structured stag) >>
          set_status (Structured_ stag) ForwardDeclared >>
          declare_fields (`Tag tag) sfields >>
          push_stritem (Seal tag)
        | Structured_ ((_, tag) as stag), ({stypedef = Some typedef; sfields}, Undeclared) ->
          (* forward-declare, then attempt to add the fields *) 
          push_stritem (Structured stag) >>
          set_status (Structured_ stag) ForwardDeclared >>
          push_stritem (Typedef (typedef, Structured stag)) >>
          set_status (Name_ typedef) Declared >>
          declare_fields (`Typedef typedef) sfields >>
          push_stritem (Seal typedef)
        | Name_ name, (Aliases typ, _) ->
          declare_typ typ >>
          push_stritem (Typedef (name, typ))
        | Name_ name, (Builtin, _) -> return ()
        | Name_ name, (Unsupported s, _) -> failf "%s" s
      in s >> set_status key Declared
  (* TODO: not quite right; only set status if we've actually declared it; not for builtins etc. *)
  and declare_typ : typexpr -> unit t = function
        Array (typ, _) -> declare_typ typ
      | Unsupported s -> failf "%s" s
      | Enum s -> declare (Enum_ s)
      | Function (args, rv)
      | Funptr (args, rv) ->
        sequence (List.map declare_typ args) >>
        declare_typ rv
      | Name s -> declare (Name_ s)
      | Pointer typ -> forward_declare_typ typ
      | Structured tag -> declare (Structured_ tag)
  and declare_fields tag : (string * typexpr) list -> unit t = function
      [] -> return ()
    | (name, typ) :: fields ->
      declare_typ typ >>
      push_stritem (Field (tag, (name, typ))) >>
      declare_fields tag fields
  and show_tag = function
      `Typedef tag -> "typedef "^ tag
    | `Tag tag -> "struct/union "^ tag
  and show_status : output_status -> string = function
      Undeclared -> "Undeclared"
    | ForwardDeclared -> "ForwardDeclared"
    | Declared -> "Declared"
    | Undeclarable -> "Undeclarable"
  and forward_declare : type a. a key -> unit t =
    fun key ->
      get >>= fun {namespace; stritems} ->
      let s = match key, Assoc.find key namespace with
          _, (_, Declared)
        | _, (_, ForwardDeclared) -> return ()
        | _, (_, Undeclarable) -> failwith "undeclarable"
        | Enum_ name, ({eitems}, Undeclared) -> (* TODO: handle enum typedefs *)
          push_stritem (Enum (name, Some eitems))
        | Structured_ ((_, tag) as stag), ({stypedef = None}, Undeclared) ->
          push_stritem (Structured stag)
        | Structured_ ((_, tag) as stag), ({stypedef = Some typedef}, Undeclared) ->
          push_stritem (Structured stag) >>
          push_stritem (Typedef (typedef, Structured stag)) >>
          set_status (Name_ typedef) ForwardDeclared
        | Name_ name, (Aliases typ, _) ->
          forward_declare_typ typ >>
          push_stritem (Typedef (name, typ))
        | Name_ name, (Builtin, _) -> return ()
        | Name_ name, (Unsupported s, _) -> failf "%s" s
      in s >> set_status key ForwardDeclared
  (* TODO: not quite right; only set status if we've actually forward declared it; not for builtins, already-declared types, etc. *)
  and forward_declare_typ : typexpr -> unit t = function
        Array (typ, _) -> forward_declare_typ typ
      | Unsupported s -> failf "%s" s
      | Enum s -> declare (Enum_ s)
      | Function (args, rv)
      | Funptr (args, rv) ->
        sequence (List.map forward_declare_typ args) >>
        forward_declare_typ rv
      | Name s -> forward_declare (Name_ s)
      | Pointer typ -> forward_declare_typ typ
      | Structured tag -> forward_declare (Structured_ tag)

  let record_foreign name ty =
    handle (declare_typ ty >> push_foreign (Foreign (name, ty)))
      ~catch:(function `Failure s ->
          Printf.fprintf stderr "Couldn't bind %s: %s\n" name s;
          return ())
end
(*
let struct_kind = function
    true -> `Struct
  | false -> `Union

let ikind_name = let open Cil in function
      IBool -> "bool"
    | IChar -> "char" 
    | ISChar    -> "schar" | IUChar     -> "uchar"
    | IShort    -> "short" | IUShort    -> "ushort"
    | IInt      -> "int"   | IUInt      -> "uint"
    | ILong     -> "long"  | IULong     -> "ulong"
    | ILongLong -> "llong" | IULongLong -> "ullong"

let fkind_name = let open Cil in function
      FFloat -> "float"
    | FDouble -> "double"
    | FLongDouble  -> "longdouble"

let int_of_expr = let open Cil in function
      Const (CInt64 (i, _, _)) when i < Int64.of_int max_int ->
      Int64.to_int i
    | Const (CInt64 (i, _, _)) ->
      Printf.kprintf failwith "Unrepresentable constant: %Ld" i
    | Const _ ->
      Printf.kprintf failwith "Unsupported constant"
    | _ ->
      Printf.kprintf failwith "Unsupported expression"

let typ_expr_of_ciltype : Cil.typ -> typexpr =
  let open Cil in
  let rec tyexpr = function
      TVoid _ -> Name "void"
    | TInt (ikind, _) -> Name (ikind_name ikind)
    | TFloat (fkind, _) -> Name (fkind_name fkind)
    | TPtr (TFun (rtype, Some args, _, _), _) ->
      Funptr (List.map arg args, tyexpr rtype)
    | TPtr (t, _) -> Pointer (tyexpr t)
    | TNamed ({tname}, _) -> Name tname
    | TComp ({cname; cstruct}, _) -> Structured (struct_kind cstruct, cname)
    | TEnum ({ename}, _) -> Enum ename
    | TBuiltin_va_list _ -> Unsupported "va_list is not yet supported"
    | TArray (ty, Some e, _) -> Array (tyexpr ty, int_of_expr e)
    | TArray (ty, None, _) ->
      (* TODO: arrays of unspecified dimensions can't always be treated as pointers
         (e.g. C99's "flexible array members", or extern values) *)
      Pointer (tyexpr ty)
    | TFun (_, _, true, _) -> Unsupported "variadic functions are not supported"
    | TFun (rtype, Some args, _, _) -> Function (List.map arg args, tyexpr rtype)
    | TFun (rtype, None, _, _) -> Unsupported "functions with unspecified arguments are not supported"
  and arg (_name, typ, _) = tyexpr typ 
  in tyexpr

let process_one = let open Stage1.M in let open Stage1 in let open Cil in function
      GType ({tname; ttype}, _) -> 
      (* typedef *)
      record_typedef tname (typ_expr_of_ciltype ttype)
    | GCompTag ({cname; cfields; cstruct}, _) ->
      (* struct/union declaration *)
      let fields = List.map (fun {fname; ftype} -> (fname, typ_expr_of_ciltype ftype)) cfields in
      record_struct_tag (struct_kind cstruct, cname) >>
      add_struct_fields (struct_kind cstruct, cname) fields
    | GCompTagDecl ({cname; cstruct}, _) ->
      (* Structure or union forward declaration *)
      record_struct_tag (struct_kind cstruct, cname)
    | GEnumTag ({ename; eitems}, _) ->
      (* Enum declaration *)
      record_enum_tag ename >>
      record_enum_items ename (List.map (fun (i, _, _) -> i) eitems)
    | GEnumTagDecl ({ename}, _) ->
      (* Enum forward declaration *)
      record_enum_tag ename
    | GVarDecl ({vname; vtype}, _)
    | GVar ({vname; vtype}, _, _)
    | GFun ({svar = {vname; vtype}}, _) ->
      record_foreign vname (typ_expr_of_ciltype vtype)
    | GAsm _ (* Global asm statement *)
    | GPragma _ (* Top-level pragma *)
    | GText _ -> 
      return () (* Top-level text (e.g. a comment) *)

let one_passm decls : unit Stage1.M.t =
  let open Stage1.M in
  Cil.foldGlobals decls (fun m g -> m >>
                          try process_one g with Failure _ -> return ())
    (return ())
*)

let one_passm decls : unit Stage1.M.t = 
  let open Stage1.M in
  return ()

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
open T@\n\
let ssize_t = lift_typ PosixTypes.ssize_t@\n"
and types_epilogue : (_,_,_) format = "@]@\nend@\n"

let make_module_name filename =
  match Humane_re.Str.(split (regexp "\\.")) filename with
    [] -> assert false
  | base :: _ -> String.capitalize_ascii base

let main regex filename types_file functions_file =
  let foreign_regex = Humane_re.Str.regexp regex in
  match types_file with None -> failwith "types file not specified" | Some types_file ->
    match functions_file with None -> failwith "functions file not specified" | Some functions_file ->
      let types_module = make_module_name types_file in
      with_open_out types_file @@ fun types_fmt ->
      with_open_out functions_file @@ fun functions_fmt ->
      match Stage1.M.run (one_passm []) Stage1.initial_state with
        `Result ((), (namespace : Stage1.M.state)) ->
        let map_name_state = function
          | Builtin -> Stage2.Declared
          | Aliases _ -> Stage2.Undeclared
          | Unsupported _ -> Stage2.Undeclarable in
        let transform_state = let open Assoc in function
          | UItem (Name_ n, s, ()) -> UItem (Name_ n, s, map_name_state s)
          | UItem (k, v, ()) -> UItem (k, v, Stage2.Undeclared) in
        let initial_cache_state = Stage2.{
            stritems = []; fstritems = [];
            namespace = List.map transform_state namespace.Stage1.statics } in
        let m = Stage2.(M.(List.fold_left (fun m (name, typ) ->
            if Humane_re.Str.matches foreign_regex name then
              (m >> record_foreign name typ) else m) (return ())
            namespace.Stage1.foreigns)) in
        begin match Stage2.M.run m initial_cache_state with
          | `Failure msg -> failwith msg
          | `Result ((), {Stage2.stritems; fstritems }) ->
            Format.fprintf functions_fmt functions_prologue types_module;
            Format.fprintf types_fmt types_prologue;
            List.iter (Print.link_time functions_fmt) (List.rev fstritems);
            List.iter (Print.stritem types_fmt) (List.rev stritems);
            Format.fprintf functions_fmt functions_epilogue;
            Format.fprintf types_fmt types_epilogue;
        end
      | `Failure s -> failwith s
