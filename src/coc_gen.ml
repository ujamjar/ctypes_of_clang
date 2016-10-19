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

    let error ?loc str = 
      let loc = match loc with Some(loc) -> loc | None -> !default_loc in
      raise (Location.Error(Location.error ~loc str))

    let clid ?(m="Ctypes") s = 
      if ctypes_opened then evar s 
      else evar (m ^ s)

    let rec ctype ~loc t = 
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
      | TPtr(t,_) -> [%expr [%e clid "ptr"] [%e ctype ~loc t]]
      | _ -> error ~loc "FIXME unsupported type."

    let carrow a b = 
      if ctypes_opened then [%expr ([%e a] @-> [%e b])]
      else [%expr (Ctypes.(@->) [%e a] [%e b])]

    let func_ctype ~loc ret args = 
      let args = if args = [] then [TVoid] else (List.map snd args) in
      let fsig = 
        List.fold_right (fun a r -> carrow (ctype ~loc a) r) args 
          [%expr returning [%e ctype ~loc ret]]
      in
      fsig

    let coc_mapper argv = 
      let open Cparse in
      let run loc code f = 
        match run ~unsaved:["coc_ppx.c",code] ["coc_ppx.c"] with
        | Ok g -> f g
        | Error () -> error ~loc "parse error"
      in
      { default_mapper with
        expr = fun mapper expr ->
          (* let f = [%cfn "ret_type fn(args,...);"] *)
          let continue () = default_mapper.expr mapper expr in
          match expr with
          | [%expr [%cfn [%e? 
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]]] -> begin
            run loc code @@ function
            | { globals=[GFunc {vi_name; 
                                   vi_typ=TFuncPtr
                                     { fs_args; fs_ret; fs_variadic=false };
                                   vi_val=None; vi_is_const=false}]; _ } -> begin
                [%expr [%e clid ~m:"Foreign" "foreign"] 
                    [%e Exp.constant (Pconst_string(vi_name,None))] 
                    [%e func_ctype ~loc fs_ret fs_args] ]
            end
            | _ -> continue ()
          end

          (* let s = [%cstruct "struct s {...};"] *)
          | [%expr [%cstruct [%e? 
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]]] -> begin
            run loc code @@ function
            | { globals=[GComp{ci_kind=Struct; ci_name; ci_members }]; _ } -> 
              let typ = tconstr ci_name [] in
              let ename = str ci_name in
              let method_ loc txt exp = 
                Cf.method_ {txt;loc} Public (Cfk_concrete(Fresh, exp))
              in
              
              let map_fields f = List.map (function
                  | Field(fi) -> f fi
                  | _ -> error ~loc "unsupported structure field") (List.rev ci_members)
              in
              
              let methods = map_fields (fun fi -> method_ loc fi.fi_name (evar fi.fi_name)) in
              let decl_field fi l = 
                [%expr let [%p pvar fi.fi_name] = 
                         field [%e evar "_struct"] [%e str fi.fi_name] [%e ctype loc fi.fi_typ] in [%e l]]
              in
              let obj = 
                let _struct = 
                  method_ loc "_struct" 
                    [%expr object 
                      method typ = _struct
                      method alloc ?finalise def = allocate ?finalise _struct def
                      method make ?finalise () = make ?finalise _struct
                    end]
                in
                Exp.object_ (Cstr.mk (Pat.any()) 
                  (_struct :: methods))
              in
              [%expr let _struct : [%t typ] structure typ = structure [%e ename] in 
                [%e 
                  List.fold_right decl_field
                    (map_fields (fun fi -> fi))
                    [%expr let () = seal _struct in 
                           [%e obj] ]
                ]
              ]
            | _ -> continue ()
          end
          | _ -> continue ()
      }

    let register () = register "cfn" coc_mapper

  end

end

