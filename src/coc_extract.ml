open Coc_typing

(* Type definitions *)

(** Equality *)
type (_, _) eql = Refl : ('a, 'a) eql

(** fromMaybe *)
let default_to default = function Some s -> s | None -> default

module type Monad = sig
  type state 
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
end

(** A monad with state and exceptions.  Raising an exception rolls back
    state changes. *)
module Make(X: sig type state end) = (struct
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
end : Monad with type state = X.state)

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

  let default_enum_status = 
    { etypedef = None; eitems = [] }

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

