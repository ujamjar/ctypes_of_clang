open Ctypes
open Ctypes_static

let rec field : type t a. offset:int -> t typ -> string -> a typ -> (a, t) field =
  fun ~offset structured label ftype ->
  match structured with
  | Struct ({ spec = Incomplete spec } as s) ->
    let field = { ftype; foffset=offset; fname = label } in
    begin
      s.fields <- BoxedField field :: s.fields;
      field
    end
  | Union ({ uspec = None } as u) ->
    let field = { ftype; foffset = 0; fname = label } in
    u.ufields <- BoxedField field :: u.ufields;
    field
  | Struct { tag; spec = Complete _ } -> raise (ModifyingSealedType tag)
  | Union { utag } -> raise (ModifyingSealedType utag)
  | View { ty } ->
     let { ftype; foffset; fname } = field ~offset ty label ftype in
     { ftype; foffset; fname }
  | _ -> raise (Unsupported "Adding a field to non-structured type")

let rec seal : type a. size:int -> align:int -> a typ -> unit = 
  fun ~size ~align typ ->
  match typ with
  | Struct { fields = [] } -> raise (Unsupported "struct with no fields")
  | Struct { spec = Complete _; tag } -> raise (ModifyingSealedType tag)
  | Struct ({ spec = Incomplete { isize } } as s) ->
    s.fields <- List.rev s.fields;
    s.spec <- Complete { size; align }
  | Union { utag; uspec = Some _ } ->
    raise (ModifyingSealedType utag)
  | Union { ufields = [] } ->
    raise (Unsupported "union with no fields")
  | Union u -> begin
    u.ufields <- List.rev u.ufields;
    u.uspec <- Some { align; size }
  end
  | View { ty } -> seal ~size ~align ty
  | _ -> raise (Unsupported "Sealing a non-structured type")

module type Foreign_options = sig
  val from : Dl.library option
  val stub : bool
end

module Make_foreign(Options : Foreign_options) : Cstubs.FOREIGN
  with type 'a result = 'a
   and type 'a return = 'a =
struct
  open Options
  type 'a fn = 'a Ctypes.fn
  type 'a return = 'a
  let (@->) = Ctypes.(@->)
  let returning = Ctypes.returning

  type 'a result = 'a
  let foreign name fn = Foreign.foreign ~stub ?from name fn
  let foreign_value name fn = Foreign.foreign_value ?from name fn
end

module Ffi_foreign = Make_foreign(struct let from = None let stub = true end)

