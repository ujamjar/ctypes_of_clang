open Coc_typing

type (_, _) eql = Refl : ('a, 'a) eql

val default_to : 'a -> 'a option -> 'a

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

module Make(X : sig type state end) : Monad with type state = X.state

type static_stritem =
    Enum of string * string list option
  | Structured of struct_tag
  | Typedef of string * typexpr
  | Field of [`Tag of string | `Typedef of string] * (string * typexpr)
  | Seal of string

type foreign_stritem =
    Foreign of string * typexpr

type enum = { etypedef : string option; eitems : string list; }

type struct_ = {
  stypedef : string option;
  sfields : (string * typexpr) list;
}

type name = Aliases of typexpr | Unsupported of string | Builtin

type _ key =
    Enum_ : string -> enum key
  | Structured_ : struct_tag -> struct_ key
  | Name_ : string -> name key

val show_key : 'a key -> string

module Assoc : sig
  type 'data uitem = UItem : 'a key * 'a * 'data -> 'data uitem
  type 'data map = 'data uitem list
  val empty : 'a list
  val add : 'a key -> 'a -> 'b -> 'b uitem list -> 'b uitem list
  val find : 'a key -> 'data map -> 'a * 'data
end

module Stage1 : sig
  type state' = {
    statics : unit Assoc.map;
    foreigns : (string * typexpr) list;
  }
  
  module M : Monad with type state = state'
  
  val initial_state : state'
  
  val update : 'a key -> ('a option -> 'a) -> unit M.t
  
  val default_struct_status : struct_
  
  val record_struct_tag : struct_tag -> unit M.t
  
  val record_struct_typedef : struct_tag -> string -> unit M.t
  
  val add_struct_fields : struct_tag -> (string * typexpr) list -> unit M.t
  
  val default_enum_status : enum
  
  val record_enum_tag : string -> unit M.t
  
  val record_enum_items : string -> string list -> unit M.t
  
  val record_typedef : string -> typexpr -> unit M.t
  
  val record_foreign : string -> typexpr -> unit M.t
end

module Stage2 : sig
  type included = [ `Included | `Undetermined | `Unsupported of string ]
  
  type enum = {
    etypedef : string option;
    eitems : string list;
    eincluded : included;
  }
  
  type struct_ = {
    stypedef : string option;
    sfields : (string * typexpr) list;
    sincluded : included;
  }
  
  type name = { nincluded : included; }
  
  type output_status =
      Undeclared
    | ForwardDeclared
    | Declared
    | Undeclarable
  
  module Value : sig type 'a t = 'a * output_status end
  
  type state' = {
    stritems : static_stritem list;
    fstritems : foreign_stritem list;
    namespace : output_status Assoc.map;
  }
  
  module M : Monad with type state = state'
  
  val push_stritem : static_stritem -> unit M.t
  
  val push_foreign : foreign_stritem -> unit M.t
  
  val sequence : unit M.t list -> unit M.t
  
  val set_status : 'a key -> output_status -> unit M.t
  
  val declare : 'a key -> unit M.t
  
  val declare_typ : typexpr -> unit M.t
  
  val declare_fields :
    [ `Tag of string | `Typedef of string ] ->
    (string * typexpr) list -> unit M.t
  
  val show_tag : [< `Tag of string | `Typedef of string ] -> string
  
  val show_status : output_status -> string
  
  val forward_declare : 'a key -> unit M.t
  
  val forward_declare_typ : typexpr -> unit M.t
  
  val record_foreign : string -> typexpr -> unit M.t
end

