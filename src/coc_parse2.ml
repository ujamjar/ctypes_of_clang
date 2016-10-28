module L = Log.Make(struct let section = "parse" end)

module Make(Clang : Coc_clang.S) = struct
  open Coc_enums
  open Clang

  module R = CXChildVisitResult
  module K = CXCursorKind
  module T = Coc_enums.CXTypeKind

  module CHash = Hashtbl.Make(struct
      type t = cursor
      let equal = Cursor.equal
      let hash = Cursor.hash
  end)

  exception Unhandled_composite_member
  exception Global_not_found 
  exception Invalid_composite_kind
  exception Unsupported_type of string
  exception Fixme of string
  exception Expecting_composite

  let anon s = "_anon_" ^ s
  let anon_comp = anon "comp"
  let anon_field = anon "field"
  let anon_enum = anon "enum"
  let anon_map = Coc_ident.init [anon_comp; anon_field; anon_enum]

  type global = 
    | GComp of { loc : Loc.t; name : string; kind : kind }
    | GEnum of { loc : Loc.t; name : string }
    | GTypedef of { loc : Loc.t; name : string; typ : typ }
    | GVar of { loc : Loc.t; name : string; typ : typ; is_const : bool }
    | GFunc of { loc : Loc.t; name : string; typ : typ }
    
  and kind = Struct | Union

  and typ = 
    | TVoid
    | TBase of string
    | TGlobal of global 
    | TArray of typ * int64
    | TPtr of typ 
    | TFuncPtr of { ret : typ; args : typ list; variadic : bool }
    | TEnum of { global : global; items : (string * int64) list; (*kind : typ*) }
    | TComp of { global : global; members : (string * typ) list }

  let name_of_global = function
    | GComp {name} 
    | GEnum {name} 
    | GTypedef {name} 
    | GVar {name} 
    | GFunc {name} -> name

  let string_of_loc l = Printf.sprintf "%s:%i:%i" l.Loc.file l.Loc.line l.Loc.col
  let sloc = string_of_loc

  let string_of_kind = function Union -> "union" | Struct -> "struct"

  let rec string_of_global =
    let open Printf in
    function
    | GComp { loc; name; kind } -> 
      sprintf "%s %s [%s]" (string_of_kind kind) name (string_of_loc loc)
    | GEnum { loc; name } ->
      sprintf "enum %s [%s]" name (string_of_loc loc)
    | GTypedef { loc; name; typ } ->
      sprintf "typedef %s : %s [%s]" name (string_of_typ typ)  (string_of_loc loc)
    | GVar { loc; name; typ } ->
      sprintf "var %s : %s [%s]" name (string_of_typ typ) (string_of_loc loc)
    | GFunc { loc; name; typ } ->
      sprintf "fun %s : %s [%s]" name (string_of_typ typ) (string_of_loc loc)

  and string_of_typ = function
    | TVoid -> "void"
    | TBase s -> s
    | TGlobal g -> name_of_global g
    | TArray(typ,size) -> string_of_typ typ ^ "[" ^ string_of_int (Int64.to_int size) ^ "]"
    | TPtr typ -> string_of_typ typ ^ "*"
    | TFuncPtr {ret; args} -> 
      string_of_typ ret ^ " (*)(" ^
        (String.concat ", " (List.map string_of_typ args)) ^ ")"
    | TEnum {global} -> "enum " ^ name_of_global global
    | TComp {global} -> "struct/union " ^ name_of_global global

  module TypeMap = Map.Make(struct
    type t = global
    let compare = compare
  end)

  type ctx = 
    {
      decls : (cursor * global) list;
      comp_members_map : (string * typ) list TypeMap.t;
      enum_items_map : (string * int64) list TypeMap.t;
      mangler : int Coc_ident.M.t;
    }

  let rec conv_typ ctx typ cursor = 
    let kind = Type.kind typ in
    match kind with
    | T.Void | T.Invalid -> TVoid
    | T.SChar -> TBase("schar")
    | T.Char_S -> TBase("char")
    | T.UChar -> TBase("uchar")
    | T.Char_U -> TBase("char")
    | T.UShort -> TBase("ushort")
    | T.UInt -> TBase("uint")
    | T.ULong -> TBase("ulong")
    | T.ULongLong -> TBase("ullong")
    | T.Short -> TBase("short")
    | T.Int -> TBase("int")
    | T.Long -> TBase("long")
    | T.LongLong -> TBase("llong")
    | T.Float -> TBase("float")
    | T.Double -> TBase("double")
  
    | T.FunctionProto | T.FunctionNoProto -> 
      let ret, args, variadic = conv_func_sig ctx typ cursor in
      TFuncPtr{ret;args;variadic}

    | T.Pointer -> conv_ptr_typ ctx (Type.pointee_type typ) cursor

    | T.Record | T.Unexposed | T.Enum | T.Typedef -> conv_decl_typ ctx (Type.declaration typ) 

    | T.DependentSizedArray | T.IncompleteArray ->
      TArray(conv_typ ctx (Type.elem_type typ) cursor, 0L)

    | T.ConstantArray -> 
      TArray(conv_typ ctx (Type.elem_type typ) cursor, Type.array_size typ)

    (* VariableArray, Vector, Int128, UInt128 *)
    (* Bool, WChar, LongDouble *)
    | _ as k -> raise (Unsupported_type (T.to_string k))

  and conv_decl_typ ctx cursor = 
    let get_global () = 
      try global_of_cursor ctx cursor 
      with e -> L.error "conv_decl_typ [%s]" (sloc @@ Loc.location @@ Cursor.location cursor); 
                raise e 
    in
    match Cursor.kind cursor with
    | K.StructDecl | K.UnionDecl -> 
      let global = get_global () in
      TComp{global; members=try TypeMap.find global ctx.comp_members_map with Not_found -> []}
    | K.EnumDecl ->
      let global = get_global () in
      TEnum{global; items=try TypeMap.find global ctx.enum_items_map with Not_found -> []}
    | _ -> raise Expecting_composite

  and conv_ptr_typ ctx typ cursor = 
    let kind = Type.kind typ in
    match kind with
    | T.Unexposed | T.FunctionProto | T.FunctionNoProto ->
      let ret = Type.ret_type typ in
      let decl = Type.declaration typ in
      L.info "conv_ptr_typ: %s %s %s" 
                      (T.to_string kind) 
                      (T.to_string (Type.kind ret)) 
                      (K.to_string (Cursor.kind decl));
      if Type.kind ret <> T.Invalid then
        let ret, args, variadic = conv_func_sig ctx typ cursor in
        TFuncPtr{ret;args;variadic}
      else if Cursor.kind decl <> K.NoDeclFound then
        TPtr(conv_decl_typ ctx (Cursor.canonical decl))
      else if Cursor.kind cursor = K.VarDecl then
        conv_typ ctx (Type.canonical_type typ) cursor
      else
        TPtr(TVoid)
    | _ -> TPtr (conv_typ ctx typ cursor)

  and conv_func_sig ctx typ cursor =
    let args = 
      List.map (fun c -> (*Cursor.spelling c,*) conv_typ ctx (Cursor.cur_type c) c) @@
        match Cursor.kind cursor with
        | K.FunctionDecl -> Array.to_list @@ Cursor.args cursor 
        | _ ->
          List.rev @@ Cursor.visit cursor 
            (fun c _ l -> 
              match Cursor.kind c with
              | K.ParmDecl -> R.Continue, c::l
              | _ -> R.Continue, l) []
    in
    let ret = conv_typ ctx (Type.ret_type typ) cursor in
    ret, args, Type.is_variadic typ 

  (* find global declaration from cursor *)
  and global_of_cursor ctx c = 
    let rec f = function [] -> raise Global_not_found 
                       | (c',g)::t when Cursor.equal c c' -> g 
                       | _ :: t -> f t 
    in
    f ctx.decls

  let to_kind = function K.StructDecl -> Struct | K.UnionDecl -> Union 
                       | _ -> raise Invalid_composite_kind

  let mangle_anon ctx a n = 
    let mangler, n = Coc_ident.mangle ctx.mangler (if n="" then a else n) in
    { ctx with mangler }, n

  let declare ~ctx ~global ~cnn_cursor ~cursor = 
    if Cursor.equal cursor cnn_cursor then 
      { ctx with decls = (cursor, global) :: ctx.decls }, global
    else 
      ctx, global_of_cursor ctx cnn_cursor

  let add_composite_members ctx global members = 
    if members = [] then ctx
    else { ctx with comp_members_map = TypeMap.add global members ctx.comp_members_map} 

  let add_enum_items ctx global items = 
    if items = [] then ctx
    else { ctx with enum_items_map = TypeMap.add global items ctx.enum_items_map} 

  let rec visit_composite cursor _ (ctx, members, prefix) = 

    let name = Cursor.spelling cursor in
    let loc = Loc.location @@ Cursor.location cursor in

    match Cursor.kind cursor with

    (* field - need to detect nested composites/enums *)
    | K.FieldDecl -> 
      let typ = Cursor.cur_type cursor in
      L.info "field '%s' [%s]" name (sloc loc);
      R.Continue, (ctx, (name, conv_typ ctx typ cursor)::members, prefix)

    (* nested composite declaration *)
    | K.StructDecl | K.UnionDecl as kind -> 
      let kind = to_kind kind in
      let cnn_cursor = Cursor.canonical cursor in
      let cnn_loc = Loc.location @@ Cursor.location cnn_cursor in
      L.info "nested composite '%s' [%s] [%s]" name (sloc loc) (sloc cnn_loc);
      (* declare nested global *)
      let ctx, name = mangle_anon ctx anon_comp (prefix ^ "_" ^ name) in
      let ctx, global = declare ~ctx ~global:(GComp{loc;name;kind}) ~cnn_cursor ~cursor in
      (* check for members *)
      let ctx = visit_nested_composite ctx global cursor name in
      R.Continue, (ctx, members, prefix)

    (* nested enumeration *)
    | K.EnumDecl -> 
      let cnn_cursor = Cursor.canonical cursor in
      let cnn_loc = Loc.location @@ Cursor.location cnn_cursor in
      L.info "nested enum '%s' [%s] [%s]" name (sloc loc) (sloc cnn_loc);
      let ctx, name = mangle_anon ctx anon_enum (prefix ^ "_" ^ name) in
      let ctx, global = declare ~ctx ~global:(GEnum{loc;name}) ~cnn_cursor ~cursor in
      (* check for members *)
      let ctx = visit_enum ctx global cursor name in
      R.Continue, (ctx, members, prefix)

    | _ as k -> 
      L.error "unhandled composite member type '%s'" (K.to_string k);
      raise Unhandled_composite_member

  and visit_nested_composite ctx global cursor prefix = 
    let ctx, members, _ = Cursor.visit cursor visit_composite (ctx,[],prefix) in
    add_composite_members ctx global (List.rev members)

  and visit_enum ctx global cursor name =  
    let items = 
      Cursor.visit cursor 
        (fun cursor _ items ->
           match Cursor.kind cursor with
           | K.EnumConstantDecl ->
             let name = Cursor.spelling cursor in
             let item = Cursor.enum_val cursor in
             R.Continue, ((name,item)::items)
           | _ ->
             R.Continue, items)
        []
    in
    add_enum_items ctx global (List.rev items)

  let visit_top tunit cursor parent ctx = 

    let name = Cursor.spelling cursor in
    let loc = Loc.location @@ Cursor.location cursor in
    let cnn_cursor = Cursor.canonical cursor in
    let cnn_loc = Loc.location @@ Cursor.location cnn_cursor in

    match Cursor.kind cursor with
    | K.UnexposedDecl -> 
      L.warn "unexposed declaration [%s] - why does this happen?" (sloc loc);
      R.Recurse, ctx

    | K.StructDecl | K.UnionDecl as kind ->
      let kind = to_kind kind in
      L.info "comp '%s' [%s]->[%s]" name (sloc loc) (sloc cnn_loc);
      (* declare the (top-level) global *)
      let ctx, name = mangle_anon ctx anon_comp name in
      let ctx, global = declare ~ctx ~global:(GComp{loc;name;kind}) ~cnn_cursor ~cursor in
      (* check for members *)
      let ctx = visit_nested_composite ctx global cursor name in
      R.Continue, ctx
   
    | K.EnumDecl ->
      L.info "enum '%s' [%s]->[%s]" name (sloc loc) (sloc cnn_loc);
      (* declare the (top-level) global *)
      let ctx, name = mangle_anon ctx anon_enum name in
      let ctx, global = declare ~ctx ~global:(GEnum{loc;name}) ~cnn_cursor ~cursor in
      (* check for members *)
      let ctx = visit_enum ctx global cursor name in
      R.Continue, ctx

    | K.FunctionDecl -> begin
      let open CXLinkageKind in
      match Cursor.linkage cursor with
      | External | UniqueExternal ->
        L.info "func '%s' [%s]->[%s]" name (sloc loc) (sloc cnn_loc);
        let typ = Cursor.cur_type cursor in
        let typ = conv_typ ctx typ cursor in
        let ctx, _ = declare ~ctx ~global:(GFunc{loc;name;typ}) 
            ~cnn_cursor:cursor ~cursor:cursor
        in
        R.Continue, ctx
      | _ -> 
        L.info "static func '%s' [%s]->[%s]" name (sloc loc) (sloc cnn_loc);
        R.Continue, ctx
    end

    | K.VarDecl -> begin
      let open CXLinkageKind in
      match Cursor.linkage cursor with
      | External | UniqueExternal ->
        L.info "var '%s' [%s]->[%s]" name (sloc loc) (sloc cnn_loc);
        let typ = Cursor.cur_type cursor in
        let is_const = Type.is_const typ in
        let typ = conv_typ ctx typ cursor in
        let ctx,_ = declare ~ctx ~global:(GVar{loc;name;typ;is_const}) ~cnn_cursor:cursor ~cursor in
        R.Continue, ctx
      | _ ->
        L.info "static var '%s' [%s]->[%s]" name (sloc loc) (sloc cnn_loc);
        R.Continue, ctx
    end

    | K.TypedefDecl ->
      let under_typ = Cursor.typedef_type cursor in
      let under_typ = 
        match Type.kind under_typ with
        | T.Unexposed -> Type.canonical_type under_typ
        | _ -> under_typ
      in
      let typ = conv_typ ctx under_typ cursor in
      let ctx, _ = declare ~ctx ~global:(GTypedef{loc;name;typ}) ~cnn_cursor:cursor ~cursor in
      L.info "typedef '%s' = %s [%s]" name (string_of_typ typ) (sloc loc);
      R.Continue, ctx

    | _ as kind -> 
      L.warn "unknown cursor kind '%s'" (K.to_string kind);
      R.Continue, ctx

  let run ?log ?pedantic ?unsaved args = 
    let ctx = 
      { decls=[]; 
        comp_members_map=TypeMap.empty; 
        enum_items_map=TypeMap.empty;
        mangler=anon_map }
    in
    run ?log ?pedantic ?unsaved ~args 
      (fun tu ctx -> Cursor.visit (TU.cursor tu) (visit_top tu) ctx) ctx

end


