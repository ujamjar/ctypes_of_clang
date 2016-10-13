open Enums
open Clang

module Simple = struct

  (* simple parsing.  just grabing idents for various top level fields *)

  let func_cb cursor parent data = 
    match Cursor.kind cursor with
    | CXCursorKind.ParmDecl ->
      (*let usr = CXCursor.getUSR cursor in*)
      let typ = Cursor.cur_type cursor in
      let str = Type.name typ in
      CXChildVisitResult.Continue, (str :: data)
    | _ ->
      CXChildVisitResult.Continue, data

  let struct_cb cursor parent data = 
    match Cursor.kind cursor with
    | CXCursorKind.FieldDecl ->
      let name = Cursor.spelling cursor in
      let typ = Type.name @@ Cursor.cur_type cursor in
      CXChildVisitResult.Continue, ((name,typ)::data)
    | _ ->
      CXChildVisitResult.Continue, data

  let enum_cb cursor parent data = 
    match Cursor.kind cursor with
    | CXCursorKind.EnumConstantDecl ->
      let name = Cursor.spelling cursor in
      let value = Cursor.enum_val cursor in
      CXChildVisitResult.Continue, ((name,value)::data)
    | _ ->
      CXChildVisitResult.Continue, data

  let visit_cb cursor parent data = 
    match Cursor.kind cursor with
    | CXCursorKind.FunctionDecl ->
      let fn = Cursor.spelling cursor in
      let ret = Type.name @@ Type.ret_type @@ Cursor.cur_type cursor in
      let args = List.rev @@ Cursor.visit cursor func_cb [] in
      CXChildVisitResult.Continue, `func(fn, ret, args)::data

    | CXCursorKind.StructDecl ->
      let name = Cursor.spelling cursor in
      let fields = List.rev @@ Cursor.visit cursor struct_cb [] in
      CXChildVisitResult.Continue, `struc(name, fields) :: data

    | CXCursorKind.UnionDecl ->
      let name = Cursor.spelling cursor in
      let fields = List.rev @@ Cursor.visit cursor struct_cb [] in
      CXChildVisitResult.Continue, `union(name, fields) :: data

    | CXCursorKind.TypedefDecl ->
      let name = Cursor.spelling cursor in
      let alias = Type.name @@ Cursor.typedef_type cursor in
      CXChildVisitResult.Continue, `typedef(name,alias) :: data

    | CXCursorKind.EnumDecl ->
      let name = Cursor.spelling cursor in
      let fields = List.rev @@ Cursor.visit cursor enum_cb [] in
      CXChildVisitResult.Continue, `enum(name,fields) :: data
    | _ ->
      CXChildVisitResult.Continue, data

  (* simple parsing *)

  let run args = 
    run args (fun tu -> Cursor.visit (TU.cursor tu) visit_cb) []

end

