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

(* a bit rough... *)
let rec string_of_typexpr t = 
  match t with
  | Array(t,n) -> string_of_typexpr t ^ "[" ^ string_of_int n ^ "]"
  | Unsupported(s) -> s
  | Enum(s) -> "enum " ^ s
  | Function(args,ret) -> string_of_typexpr ret ^ "_(" ^ (String.concat "," (List.map string_of_typexpr args)) ^ ")"
  | Funptr(args,ret) -> string_of_typexpr ret ^ "*(" ^ (String.concat "," (List.map string_of_typexpr args)) ^ ")"
  | Name(s) -> s 
  | Pointer(t) -> string_of_typexpr t ^ " *"
  | Structured (t,s) -> (match t with `Struct -> "struct" | `Union -> "union") ^ " " ^ s

module Make(Clang : Coc_clang.S) = struct
  module C = Coc_enums.CXTypeKind 
  open Clang

  let rec conv_ty typ = 
    let kind = Type.kind typ in
    match kind with
    | C.Void -> Name "void"
    | C.Bool -> Name "bool"
    | C.Char_U -> Name "uchar" (*???*)
    | C.UChar -> Name "uchar"
    | C.UShort -> Name "ushort"
    | C.UInt -> Name "uint"
    | C.ULong -> Name "ulong"
    | C.ULongLong -> Name "ullong"
    | C.Char_S -> Name "schar"
    | C.SChar -> Name "schar"
    | C.Short -> Name "short"
    | C.Int -> Name "int"
    | C.Long -> Name "long"
    | C.LongLong -> Name "llong"
    | C.Float -> Name "float"
    | C.Double -> Name "double"
    | C.Pointer -> Pointer (conv_ty (Type.pointee_type typ)) (* XXX funptr *)
    (*| C.Record -> Structured
    | C.Enum ->
    | C.Typedef ->*)
    | _ -> Unsupported (Type.name typ)

(*
  let rec conv_ty2 typ ctx cursor = 
    match ty ctx with
    | C.Void -> Name "void"
    | C.Bool -> Name "bool"
    | C.Char_U -> Name "uchar" (*???*)
    | C.UChar -> Name "uchar"
    | C.UShort -> Name "ushort"
    | C.UInt -> Name "uint"
    | C.ULong -> Name "ulong"
    | C.ULongLong -> Name "ullong"
    | C.Char_S -> Name "schar"
    | C.SChar -> Name "schar"
    | C.Short -> Name "short"
    | C.Int -> Name "int"
    | C.Long -> Name "long"
    | C.LongLong -> Name "llong"
    | C.Float -> Name "float"
    | C.Double -> Name "double"
    | C.Pointer -> conv_ptr_ty (Type.pointee_type typ) cursor
    | C.VariableArray -> unreachable (* XXX *)

    | CXType_DependentSizedArray | CXType_IncompleteArray ->
        TArray(Box::new(conv_ty(ctx, &ty.elem_type(), cursor)), 0, layout)
    }
    CXType_FunctionProto | CXType_FunctionNoProto => {
      TFuncProto(mk_fn_sig(ctx, ty, cursor), layout)
    }
    CXType_Record |
    CXType_Typedef |
    CXType_Unexposed |
    CXType_Enum => conv_decl_ty(ctx, &ty.declaration()),
    CXType_ConstantArray => {
      TArray(Box::new(conv_ty(ctx, &ty.elem_type(), cursor)),
             ty.array_size(),
             layout)
    }
    CXType_Vector => {
      TVoid
    }
    CXType_Int128 | CXType_UInt128 => {
      TVoid
    }
    _ => {
      let fail = ctx.options.fail_on_unknown_type;
      log_err_warn(ctx,
 *)                 
end

