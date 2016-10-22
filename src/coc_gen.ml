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
      | TInt(IShort) -> evar "short"
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
      | TComp(ci) -> [%expr [%e evar ci.ci_name].ctype]
      | TEnum(ei) -> [%expr [%e evar ei.ei_name].ctype]
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

    let pvar loc s = Pat.var (Location.mkloc s loc)

    let rec gen_cstruct loc ci_name ci_members = 
      let typ = Ast_helper.Typ.variant 
          [Parsetree.Rtag("struct_"^ci_name, [], true, [])] 
          Asttypes.Closed None
      in
      let sstruct = "_struct" in
      let pstruct = pvar loc sstruct in
      let ename = str ci_name in
      let ci_members = List.rev ci_members in
      let method_ loc txt exp = 
        Cf.method_ {txt;loc} Public (Cfk_concrete(Fresh, exp))
      in
      
      let gen_field = 

        let estruct = evar sstruct in
        function
        | Field(fi) ->
          fi.fi_name,
          [%expr field [%e estruct] [%e str fi.fi_name] [%e ctype loc fi.fi_typ]],
          method_ loc fi.fi_name (evar fi.fi_name)

        | CompField(ci,fi) ->
          let substruct = gen_cstruct loc ci.ci_name ci.ci_members in
          fi.fi_name,
          [%expr let [%p pvar loc fi.fi_name] = [%e substruct] in
            { Coc_runtime.field = 
                field [%e estruct] [%e str fi.fi_name] [%e evar fi.fi_name].ctype; 
              structure = [%e evar fi.fi_name] }], 
          method_ loc fi.fi_name (evar fi.fi_name)

        | _ as x -> error ~loc "unsupported structure field %s" 
                                    (Cparse.show_comp_member x)
      in

      let fields = List.map gen_field ci_members in
      let obj = Exp.object_ (Cstr.mk (Pat.any()) (List.map (fun (_,_,m) -> m) fields)) in

      [%expr let [%p pstruct] : [%t typ] structure typ = structure [%e ename] in 
        [%e 
          List.fold_right 
            (fun (n,f,_) l -> [%expr let [%p pvar loc n] = [%e f] in [%e l]])
            fields
            [%expr let () = seal _struct in 
              { Coc_runtime.ctype=[%e evar sstruct]; members=[%e obj]; } ]
        ]
      ]

    (* XXX phantom type no longer really makes sense 
       XXX of_int not necessarily unique
       XXX consider implementation via assoc lists or constants
    *)
    let gen_enum loc name items = 
      let to_int = 
        Exp.function_ ~loc @@
        List.rev @@ List.map (fun i -> 
          Exp.case 
            (Pat.variant ~loc i.eit_name None) 
            (int ~loc (Int64.to_int i.eit_val))) items
      in
      let of_int =
        let default = Exp.case (Pat.any ()) [%expr failwith "enum to_int"] in
        (* XXX integer values ought to be unique - warn/error? *)
        Exp.function_ ~loc @@
        List.rev @@ default :: List.map (fun i -> 
          Exp.case 
            (Pat.constant ~loc (Const.int (Int64.to_int i.eit_val)))
            (Exp.variant ~loc i.eit_name None))
            items
      in
      let typ = Ast_helper.Typ.variant 
          [Parsetree.Rtag("enum_"^name, [], true, [])] 
          Asttypes.Closed None
      in
      [%expr
        let to_int, of_int = [%e to_int], [%e of_int] in
        ({ Coc_runtime.ctype = view ~read:[%e of_int] ~write:[%e to_int] int;
           to_int; of_int } : ([%t typ],'b) Coc_runtime.enum)]

    let cfn loc code = 
      run loc code @@ function
      | { globals=(GFunc {vi_name; 
                          vi_typ=TFuncPtr { fs_args; fs_ret; fs_variadic=false };
                          vi_val=None; vi_is_const=false})::_; _ } -> 
        gen_cfn loc vi_name fs_args fs_ret 
      | _ -> error ~loc "expecting function definition"

    let cstruct loc code = 
      run loc code @@ function
      | { globals=(GComp{ci_kind=Struct; ci_name; ci_members })::_; _ } -> 
        gen_cstruct loc ci_name ci_members
      | _ -> error ~loc "expecting structure definiton"

    let cenum loc code = 
      run loc code @@ function
      | { globals=(GEnum{ei_name; ei_items; ei_kind})::_; _} ->
        gen_enum loc ei_name ei_items
      | _ -> error ~loc "expecting enum definition"

    let ccode loc code = 
      let gen = function

        | GFunc {vi_name; 
                 vi_typ=TFuncPtr { fs_args; fs_ret; fs_variadic=false };
                 vi_val=None; vi_is_const=false} -> 
          [%stri
            let [%p pvar loc vi_name] = [%e gen_cfn loc vi_name fs_args fs_ret]
          ]

        | GComp{ci_kind=Struct; ci_name; ci_members } ->
          [%stri let [%p pvar loc ci_name] = [%e gen_cstruct loc ci_name ci_members] ]

        | GEnum{ei_name; ei_items; ei_kind} ->
          [%stri let [%p pvar loc ei_name] = [%e gen_enum loc ei_name ei_items] ]

        | GType{ti_name; ti_typ} ->
          [%stri let [%p pvar loc ti_name] = [%e ctype loc ti_typ]]
          
        | _ as g -> error ~loc "unsupported c global [%s]" (Cparse.show_global g)

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

          (* [%cenum ...] *)
          | [%stri [%cenum let [%p? binding] = [%e?
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]]] -> 
            [%stri let [%p binding] = [%e cenum loc code] ]

          | [%stri let%cenum [%p? binding] = [%e?
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]] -> 
            [%stri let [%p binding] = [%e cenum loc code] ]

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

          (* [%cenum ...] *)
          | [%expr [%cenum [%e? 
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}]]] -> 
            cenum loc code
          
          | [%expr [%cenum let [%p? binding] = [%e?
              {pexp_desc=Pexp_constant(Pconst_string(code,_)); pexp_loc=loc}] in 
              [%e? expr]]] -> 
            [%expr let [%p binding] = [%e cenum loc code] in [%e expr]]

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
                }; }; } ]), [])} :: rest -> 
              Str.module_ (Mb.mk (Location.mkloc mod_name loc) 
                (Mod.structure @@ ccode loc code)) :: mapper.structure mapper rest
          
          | h::rest -> mapper.structure_item mapper h :: mapper.structure mapper rest
          | [] -> []
        end;

      }

    let register () = register "cfn" coc_mapper

  end

end

