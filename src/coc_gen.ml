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
      Printf.kprintf (fun str -> raise (Location.Error(Location.error ~loc str))) str

    let cerror ?loc errors = 
      error ?loc
        "%s" (String.concat "\n" (List.map snd errors))

    let carrow a b = [%expr ([%e a] @-> [%e b])]

    let rec ctype ~loc t = 
      match t with
      | TVoid -> evar "void"
      (*| TInt(IBool) ->*)
      | TInt(ISChar) -> evar "schar"
      | TInt(IUChar) -> evar "uchar"
      | TInt(IShort) -> evar "sshort"
      | TInt(IUShort) -> evar "ushort"
      | TInt(IInt) -> evar "int32_t"
      | TInt(IUInt) -> evar "uint32_t"
      | TInt(ILong) -> evar "long"
      | TInt(IULong) -> evar "ulong"
      | TInt(ILongLong) -> evar "llong"
      | TInt(IULongLong) -> evar "ullong"
      | TFloat(FFloat) -> evar "float"
      | TFloat(FDouble) -> evar "double"
      | TPtr(t,_) -> [%expr [%e evar "ptr"] [%e ctype ~loc t]]
      | TNamed(t) -> evar t.ti_name
      | TArray(t,s) -> 
        if s = 0L then ctype ~loc (TPtr(t,false))
        else [%expr [%e evar "array"] 
                [%e Ast_convenience.int (Int64.to_int s)] 
                [%e ctype ~loc t]]
      | TComp(ci) -> evar ci.ci_name
      | TEnum(ei) -> ctype ~loc (TInt(ei.ei_kind))
      | TFuncPtr(fs) -> 
        if fs.fs_variadic then error ~loc "no support for variadic functions"
        else [%expr funptr [%e func_ctype ~loc fs.fs_ret fs.fs_args]]
      | TFuncProto _ -> error ~loc "FIXME: TFuncProto?"
      | _ -> error ~loc "unsupported type '%s'" (show_typ t)

    and func_ctype ~loc ret args = 
      let args = if args = [] then [TVoid] else (List.map snd args) in
      let fsig = 
        List.fold_right (fun a r -> carrow (ctype ~loc a) r) args 
          [%expr returning [%e ctype ~loc ret]]
      in
      fsig

    let run loc code f = 
      match Cparse.run ~unsaved:["coc_ppx.c",code] ["coc_ppx.c"] with
      | Ok (g) -> f g
      | Error (errs) -> cerror ~loc errs

    let gen_cfn loc vi_name fs_args fs_ret = 
      [%expr [%e evar "foreign"] 
          [%e Exp.constant (Pconst_string(vi_name,None))] 
          [%e func_ctype ~loc fs_ret fs_args] ]
      
    let cfn loc code = 
      run loc code @@ function
      | { globals=(GFunc {vi_name; 
                          vi_typ=TFuncPtr { fs_args; fs_ret; fs_variadic=false };
                          vi_val=None; vi_is_const=false})::_; _ } -> 
        gen_cfn loc vi_name fs_args fs_ret 
      | _ -> error "expecting function definition"

    let pvar loc s = Pat.var (Location.mkloc s loc)

    let gen_cstruct loc ci_name ci_members = 
      let typ = Ast_helper.Typ.variant 
          [Parsetree.Rtag(ci_name, [], true, [])] 
          Asttypes.Closed None
      in
      let sstruct = "_struct" in
      let pstruct = pvar loc sstruct in
      let ename = str ci_name in
      let method_ loc txt exp = 
        Cf.method_ {txt;loc} Public (Cfk_concrete(Fresh, exp))
      in
      
      let map_fields f = List.map (function
          | Field(fi) -> f fi
          | _ -> error ~loc "unsupported structure field") (List.rev ci_members)
      in
      
      let methods = map_fields 
          (fun fi -> method_ loc fi.fi_name (evar fi.fi_name)) 
      in
      let decl_field fi l = 
        [%expr 
          let [%p pvar loc fi.fi_name] = 
            field [%e evar sstruct] [%e str fi.fi_name] [%e ctype loc fi.fi_typ] in [%e l]]
      in
      let obj = Exp.object_ (Cstr.mk (Pat.any()) methods) in

      [%expr let [%p pstruct] : [%t typ] structure typ = structure [%e ename] in 
        [%e 
          List.fold_right decl_field
            (map_fields (fun fi -> fi))
            [%expr let () = seal _struct in 
              [%e evar sstruct], [%e obj] ]
        ]
      ]

    let cstruct loc code = 
      run loc code @@ function
      | { globals=(GComp{ci_kind=Struct; ci_name; ci_members })::_; _ } -> 
        gen_cstruct loc ci_name ci_members
      | _ -> error "expecting structure definiton"

    let ccode loc code = 
      let gen = function

        | GFunc {vi_name; 
                 vi_typ=TFuncPtr { fs_args; fs_ret; fs_variadic=false };
                 vi_val=None; vi_is_const=false} -> 
          [%stri
            let [%p pvar loc vi_name] = [%e gen_cfn loc vi_name fs_args fs_ret]
          ]

        | GComp{ci_kind=Struct; ci_name; ci_members } ->
          [%stri 
            let ([%p pvar loc ci_name], [%p pvar loc (ci_name ^ "_members")]) = 
                [%e gen_cstruct loc ci_name ci_members]
          ]

        | _ -> error "unsupported c global"

      in
      run loc code @@ fun ctx -> List.map gen (List.rev ctx.globals)

    let coc_mapper argv = 

      { default_mapper with

        structure_item = begin fun mapper stri -> 
          match stri with

          (* [%cfn ...] *)
          | [%stri [%cfn let [%p? binding] = [%e?
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]]] -> 
            [%stri let [%p binding] = [%e cfn loc code] ]

          | [%stri let%cfn [%p? binding] = [%e?
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]] -> 
            [%stri let [%p binding] = [%e cfn loc code] ]

          (* [%cstruct ...] *)
          | [%stri [%cstruct let [%p? binding] = [%e?
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]]] -> 
            [%stri let [%p binding] = [%e cstruct loc code] ]

          | [%stri let%cstruct [%p? binding] = [%e?
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]] -> 
            [%stri let [%p binding] = [%e cstruct loc code] ]

          | _ -> default_mapper.structure_item mapper stri 

        end;
          
        expr = begin fun mapper expr -> 

          match expr with

          (* [%cfn ...] *)
          | [%expr [%cfn [%e? 
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]]] -> 
              cfn loc code 

          | [%expr [%cfn let [%p? binding] = [%e?
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}] in 
              [%e? expr]]] -> 
            [%expr let [%p binding] = [%e cfn loc code] in [%e expr]]

          (* [%cstruct ...] *)
          | [%expr [%cstruct [%e? 
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]]] -> 
            cstruct loc code
          
          | [%expr [%cstruct let [%p? binding] = [%e?
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}] in 
              [%e? expr]]] -> 
            [%expr let [%p binding] = [%e cstruct loc code] in [%e expr]]

          | _ -> default_mapper.expr mapper expr 

        end;

        structure = begin fun mapper items ->
          match items with

          (* [%ccode ...] *)
          | [%stri [%ccode [%e? 
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]]] :: rest -> 
            ccode loc code @ mapper.structure mapper rest

          (* module%ccode ...] *)
          | {pstr_desc =
            Pstr_extension
             (({txt = "ccode" }, PStr [ { pstr_desc = Pstr_module {
                pmb_name = {txt = mod_name; loc; };
                pmb_expr = {
                  pmod_desc = Pmod_structure [ { 
                    pstr_desc = Pstr_eval ( { 
                      pexp_desc = Pexp_constant (Pconst_string (code, _)); }, []);
                    } ];
                }; }; } ]), [])} :: rest -> begin
              Str.module_ (Mb.mk (Location.mkloc mod_name loc) 
                (Mod.structure @@ ccode loc code)) :: mapper.structure mapper rest
            end

          
          | h::rest -> mapper.structure_item mapper h :: mapper.structure mapper rest
          | [] -> []
        end

      }

    let register () = register "cfn" coc_mapper

  end

end

