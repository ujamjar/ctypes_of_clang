type ('a,'b) rt_structured = 
  {
    ctype : 'a;
    members : 'b;
  }

type ('e,'p) rt_enum = 
  {
    ctype : 'e;
    to_int : 'p -> int64;
    of_int : int64 -> 'p;
  }

open Ctypes

(* my understanding of how it works on linux *)
let __builtin_va_list = 
  let x = structure "__builtin_va_list" in
  let _ = field x "f0" int in
  let _ = field x "f1" int in
  let _ = field x "f2" (ptr void) in
  let _ = field x "f3" (ptr void) in
  let () = seal x in
  x

let ldouble = 
  let x = structure "ldouble" in
  let _ = field x "f0" llong in
  let _ = field x "f1" llong in
  let () = seal x in
  x

let rec cstr_length p = 
  let c = !@ p in 
  if Char.code c = 0 then 0 else 1 + cstr_length (p +@ 1)

let rec cstr p = 
  let l = cstr_length p in
  let s = Bytes.make l (Char.chr 0) in
  for i=0 to l-1 do
    Bytes.set s i @@  (!@ (p +@ i))
  done;
  s

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
