open Enums
open Clang

let ocaml_keywords = 
  [
    "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
    "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
    "lazy"; "let"; "match"; "method"; "module"; "mutable"; "new"; "object";
    "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to";
    "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";
  ]

let rev_ok = function
  | Ok l -> Ok (List.rev l)
  | Error e -> Error e

module Simple = struct

  (* simple parsing.  just grabing idents for various top level fields *)
  
  open CXChildVisitResult
  open CXCursorKind

  type t = [ `enum of string * (string * int64) list
           | `func of string * string * string list
           | `union of string * (string * string) list
           | `struc of string * (string * string) list
           | `typedef of string * string ]

  let func_cb cursor parent data = 
    match Cursor.kind cursor with
      | ParmDecl ->
        let typ = Cursor.cur_type cursor in
        let str = Type.name typ in
        Continue, (str :: data)
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

  let enum_cb cursor parent data = 
    match Cursor.kind cursor with
    | EnumConstantDecl ->
      let name = Cursor.spelling cursor in
      let value = Cursor.enum_val cursor in
      Continue, ((name,value)::data)
    | _ ->
      Continue, data

  let visit_cb cursor parent data = 
    match Cursor.kind cursor with
    | FunctionDecl ->
      let fn = Cursor.spelling cursor in
      let ret = Type.name @@ Type.ret_type @@ Cursor.cur_type cursor in
      let args = List.rev @@ Cursor.visit cursor func_cb [] in
      Continue, `func(fn, ret, args)::data

    | StructDecl ->
      let name = Cursor.spelling cursor in
      let fields = List.rev @@ Cursor.visit cursor struct_cb [] in
      Continue, `struc(name, fields) :: data

    | UnionDecl ->
      let name = Cursor.spelling cursor in
      let fields = List.rev @@ Cursor.visit cursor struct_cb [] in
      Continue, `union(name, fields) :: data

    | TypedefDecl ->
      let name = Cursor.spelling cursor in
      let alias = Type.name @@ Cursor.typedef_type cursor in
      Continue, `typedef(name,alias) :: data

    | EnumDecl ->
      let name = Cursor.spelling cursor in
      let fields = List.rev @@ Cursor.visit cursor enum_cb [] in
      Continue, `enum(name,fields) :: data
    | _ ->
      Continue, data

  (* simple parsing *)

  let run args = 
    rev_ok @@ run args (fun tu -> Cursor.visit (TU.cursor tu) visit_cb) []

end

module LessSimple = struct

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
        int_type : Ctyping.builtin_int_type option; 
        fields : enum_field list;
        kindname : string;
        typename : string;
      } 

    | Function of 
      {
        loc : loc;
        name : string; 
        kindname : string;
        typename : string;
      }

    | Struct of 
      {
        loc : loc;
        name : string; 
        kindname : string;
        typename : string;
      }

    | Union of 
      {
        loc : loc;
        name : string; 
        kindname : string;
        typename : string;
      }
    
    | Typedef of
      {
        loc : loc;
        name : string; 
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

  let visit_cb cursor parent data = 
    let kindname = CXCursorKind.to_string @@ Cursor.kind cursor in
    match Cursor.kind cursor with
    | FunctionDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let typename = Type.name @@ Cursor.cur_type cursor in
      Continue, Function{loc;name;kindname;typename}::data

    | StructDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let typename = Type.name @@ Cursor.cur_type cursor in
      Continue, Struct{loc;name;kindname;typename} :: data

    | UnionDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let typename = Type.name @@ Cursor.cur_type cursor in
      Continue, Union{loc;name;kindname;typename} :: data

    | TypedefDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let typename = Type.name @@ Cursor.cur_type cursor in
      Continue, Typedef{loc;name;kindname;typename} :: data

    | EnumDecl ->
      let name = Cursor.spelling cursor in
      let loc = get_loc cursor in
      let int_type = Ctyping.get_builtin_int_type @@ Type.kind @@ Cursor.enum_type cursor in
      let fields = List.rev @@ Cursor.visit cursor enum_cb [] in
      let typename = Type.name @@ Cursor.cur_type cursor in
      Continue, Enum{loc;name;int_type;fields;kindname;typename} :: data

  | _ ->
    Continue, data

  let run args = 
    rev_ok @@ run args (fun tu -> Cursor.visit (TU.cursor tu) visit_cb) []

end

