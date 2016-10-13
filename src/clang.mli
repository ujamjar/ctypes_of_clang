open Ctypes
open Enums

type str
val str : str typ

module Str : sig
  val getC : str -> string
  val dispose : str -> unit
  val to_string : ?dispose:bool -> str -> string
end

type idx
val idx : idx typ

module Index : sig
  val create : int -> int -> idx
  val dispose : idx -> unit
end

type file
val file : file typ

module File : sig
  val name : file -> string
end

type loc
val loc : loc typ

module Loc : sig
  val location : loc -> (file * int * int * int)
end

type ctyp
val ctyp : ctyp typ
type cursor
val cursor : cursor typ

module Type : sig
  val name : ctyp -> string
  val kind : ctyp -> CXTypeKind.t
  val kind_name : CXTypeKind.t -> string
  val declaration : ctyp -> cursor
  val is_const : ctyp -> bool
  val size : ctyp -> int64
  val align : ctyp -> int64
  val pointee_type : ctyp -> ctyp
  val elem_type : ctyp -> ctyp
  val array_size : ctyp -> int64
  val canonical_type : ctyp -> ctyp
  val is_variadic : ctyp -> bool
  val num_args : ctyp -> int
  val arg_types : ctyp -> ctyp array
  val ret_type : ctyp -> ctyp
  val calling_conv : ctyp -> CXCallingConv.t
end

module Cursor : sig
  val spelling : cursor -> string
  val kind : cursor -> CXCursorKind.t
  val location : cursor -> loc
  val cur_type : cursor -> ctyp
  val definition : cursor -> cursor
  val canonical : cursor -> cursor
  val bit_width : cursor -> int option
  val enum_type : cursor -> ctyp
  val enum_val : cursor -> int64
  val typedef_type : cursor -> ctyp
  val linkage : cursor -> CXLinkageKind.t
  val num_args : cursor -> int
  val ret_type : cursor -> ctyp
  val args : cursor -> cursor array
  val visit : cursor -> (cursor -> cursor -> 'a -> CXChildVisitResult.t * 'a) -> 'a -> 'a
end

type tu
val tu : tu typ

module TU : sig
  val parse : ?options:CXTranslationUnit_Flags.t list -> index:idx -> string list -> tu
  val cursor : tu -> cursor
  val dispose : tu -> unit
end

type diag
val diag : diag typ

module Diag : sig
  val num : tu -> int
  val get : tu -> int -> diag
  val to_string : diag -> string
  val diags : tu -> string array
end

val run : ?options:CXTranslationUnit_Flags.t list -> string list -> (tu -> 'a -> 'b) -> 'a -> 
  ('b, string array) result

