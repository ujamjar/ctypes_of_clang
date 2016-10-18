module L = Log.Make(struct let section = "gen" end)

module M = Map.Make(String)

(* create ocaml compatible lower case identifier from C-ident. *)
let lid s = 
  if s = "" then "_anonymous"
  else if Char.uppercase_ascii s.[0] = s.[0] then String.uncapitalize_ascii s
  else s

(* name mangler - add numeric suffixes for repeated names *)
let mk_mangler () = 
  let mangler = ref M.empty in
  let rec mangle s =
    let m = !mangler in
    let suffix n = s ^ "_" ^ string_of_int n in
    match M.find s m with
    | n -> 
      let m = M.add s (n+1) m in
      let s = suffix n in
      mangler := m;
      mangle s
    | exception Not_found -> 
      let m = M.add s 0 m in
      mangler := m;
      s 
  in
  mangler, mangle

module Make(Clang : Coc_clang.S) = struct

  module Cparse = Coc_parse.Make(Clang)
  open Cparse

  type ctx = 
    {
      mangle : string -> string;
      globals : global list;
    }

  let init_ctx g = 
    let _, mangle = mk_mangler () in
    {
      mangle; 
      globals = g;
    }

  let is_typ = function
    | GType _ 
    | GComp _ | GCompDecl _ 
    | GEnum _ | GEnumDecl _ -> true
    | _ -> false

  let typs ctx = List.filter is_typ ctx.globals
  let funcs ctx = List.filter (fun f -> not (is_typ f)) ctx.globals

  module Ppx = struct

    open Ast_mapper
    open Ast_helper
    open Asttypes
    open Parsetree
    open Longident
    open Ast_convenience

    let ctypes_opened = true

    let error loc str = raise (Location.Error(Location.error ~loc str))

    let clid ?(m="Ctypes") s = 
      if ctypes_opened then evar s 
      else evar (m ^ s)

    let rec ctype loc t = 
      match t with
      | TVoid -> clid "void"
      (*| TInt(IBool) ->*)
      | TInt(ISChar) -> clid "schar"
      | TInt(IUChar) -> clid "uchar"
      | TInt(IShort) -> clid "sshort"
      | TInt(IUShort) -> clid "ushort"
      | TInt(IInt) -> clid "int32_t"
      | TInt(IUInt) -> clid "uint32_t"
      | TInt(ILong) -> clid "long"
      | TInt(IULong) -> clid "ulong"
      | TInt(ILongLong) -> clid "llong"
      | TInt(IULongLong) -> clid "ullong"
      | TFloat(FFloat) -> clid "float"
      | TFloat(FDouble) -> clid "double"
      | TPtr(t,_) -> [%expr [%e clid "ptr"] [%e ctype loc t]]
      | _ -> error loc "FIXME unsupported type."

    let carrow a b = 
      if ctypes_opened then [%expr ([%e a] @-> [%e b])]
      else [%expr (Ctypes.(@->) [%e a] [%e b])]

    let func_ctype loc ret args = 
      let args = if args = [] then [TVoid] else (List.map snd args) in
      let fsig = 
        List.fold_right (fun a r -> carrow (ctype loc a) r) args 
          [%expr returning [%e ctype loc ret]]
      in
      fsig

    let coc_mapper argv = 
      { default_mapper with
        expr = fun mapper expr ->
          match expr with
          | { pexp_desc = Pexp_extension ({ txt="cfn"; loc }, pstr); _ } -> begin
              match pstr with
              | PStr [{ pstr_desc = 
                Pstr_eval ({ 
                  pexp_loc = loc; 
                  pexp_desc = Pexp_constant (Pconst_string (sym,_)) 
                }, _) }] -> begin
                  let open Cparse in

                  match run ~unsaved:["coc_ppx.c",sym] ["coc_ppx.c"] with
                  | Ok { globals=[GFunc {vi_name; 
                                         vi_typ=TFuncPtr
                                           { fs_args; fs_ret; fs_variadic=false };
                                         vi_val=None; vi_is_const=false}]; _ } -> begin
                      [%expr [%e clid ~m:"Foreign" "foreign"] 
                          [%e Exp.constant (Pconst_string(vi_name,None))] 
                          [%e func_ctype loc fs_ret fs_args] ]
                  end
                  | Ok _ -> 
                    error loc "[cfn] expecting single function defn"
                  | Error () -> 
                    error loc "[cfn] parse error"
              end
              | _ -> error loc "[cfn] bad payload"
          end
          | _ -> default_mapper.expr mapper expr
      }

    let register () = register "cfn" coc_mapper

  end

end

