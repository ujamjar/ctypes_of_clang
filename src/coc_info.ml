open Coc_enums

let rev_ok = function
  | Ok l -> Ok (List.rev l)
  | Error e -> Error e

module Make(Clang : Coc_clang.S) = struct
  open Clang

  type enum_field = string * int64
  
  type loc = 
    {
      file : string;
      line : int;
      col : int;
      offset : int;
    }

  type t = 
    | Enum of 
      { 
        loc : loc;
        name : string; 
        int_type : string; 
        fields : enum_field list;
        kindname : string;
        typename : string;
      } 

    | Function of 
      {
        loc : loc;
        name : string; 
        returns : string;
        args : string list;
        kindname : string;
        typename : string;
      }

    | Struct of 
      {
        loc : loc;
        name : string; 
        fields : (string * string) list;
        kindname : string;
        typename : string;
      }

    | Union of 
      {
        loc : loc;
        name : string; 
        fields : (string * string) list;
        kindname : string;
        typename : string;
      }
    
    | Typedef of
      {
        loc : loc;
        name : string; 
        aliases : string;
        kindname : string;
        typename : string;
      }

  open CXChildVisitResult
  open CXCursorKind

  let get_loc cursor = 
    let file, line, col, offset = Loc.location (Cursor.location cursor) in
    let file = File.name file in
    { file; line; col; offset }

  let enum_cb cursor parent data = 
    match Cursor.kind cursor with
    | EnumConstantDecl -> 
      let name = Cursor.spelling cursor in
      let value = Cursor.enum_val cursor in
      Continue, ((name,value)::data)
    | _ -> 
      Continue, data

  let struct_cb cursor parent data = 
    match Cursor.kind cursor with
    | FieldDecl ->
      let name = Cursor.spelling cursor in
      let typ = Type.name @@ Cursor.cur_type cursor in
      Continue, ((name,typ)::data)
    | _ ->
      Continue, data

  let func_cb cursor parent data = 
    match Cursor.kind cursor with
      | ParmDecl ->
        let typ = Cursor.cur_type cursor in
        let str = Type.name typ in
        Continue, (str :: data)
      | _ ->
        Continue, data

  let visit_cb cursor parent data = 
    let kindname = CXCursorKind.to_string @@ Cursor.kind cursor in
    match Cursor.kind cursor with
    | FunctionDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let cur_type = Cursor.cur_type cursor in
      let typename = Type.name @@ cur_type in
      let returns = Type.name @@ Type.ret_type cur_type in
      let args = List.rev @@ Cursor.visit cursor func_cb [] in
      Continue, Function{loc;name;returns;args;kindname;typename}::data

    | StructDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let typename = Type.name @@ Cursor.cur_type cursor in
      let fields = List.rev @@ Cursor.visit cursor struct_cb [] in
      Continue, Struct{loc;name;fields;kindname;typename} :: data

    | UnionDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let typename = Type.name @@ Cursor.cur_type cursor in
      let fields = List.rev @@ Cursor.visit cursor struct_cb [] in
      Continue, Union{loc;name;fields;kindname;typename} :: data

    | TypedefDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let typename = Type.name @@ Cursor.cur_type cursor in
      let aliases = Type.name @@ Cursor.typedef_type cursor in
      Continue, Typedef{loc;name;aliases;kindname;typename} :: data

    | EnumDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let int_type = Type.name @@ Cursor.enum_type cursor in
      let fields = List.rev @@ Cursor.visit cursor enum_cb [] in
      let typename = Type.name @@ Cursor.cur_type cursor in
      Continue, Enum{loc;name;int_type;fields;kindname;typename} :: data

  | _ ->
    Continue, data

  let run ?unsaved args = 
    rev_ok @@ run ?unsaved ~args (fun tu -> Cursor.visit (TU.cursor tu) visit_cb) []

end

