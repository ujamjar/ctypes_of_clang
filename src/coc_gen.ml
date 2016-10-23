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

  module Attrs = struct

    type t = 
      [ `clangargs of string list ]

    let rec get = function
      | [] -> []
      | ({txt="clangargs";loc}, PStr [ [%stri [%e? args]] ]) :: tl -> begin
        let rec f args = 
          match args with
          | [%expr []] -> []
          | [%expr [%e? {pexp_desc=Pexp_constant(Pconst_string(h,_))}] :: [%e? t]] -> h :: f t
          | _ -> error "clangargs: expecting list of strings"
        in
        `clangargs(f args) :: get tl
      end
      | _ :: tl -> get tl

    let rec clangargs attrs = 
      let rec f = function
        | [] -> []
        | `clangargs(args) :: t -> args :: f t
        | _ :: t -> f t
      in
      List.concat (f attrs)

  end

  let carrow a b = [%expr ([%e a] @-> [%e b])]

  let rec ctype ~loc t =
    let evar n = if n="" then evar "_anon" else evar n in
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

  let run ~loc ~attrs ~code f = 
    let clangargs = Attrs.clangargs attrs in
    match Cparse.run ~unsaved:["coc_ppx.c",code] ("coc_ppx.c"::clangargs) with
    | Ok (g) -> f g
    | Error (errs) -> cerror ~loc errs

  let gen_cfn loc vi_name fs_args fs_ret = 
    [%expr [%e evar "foreign"] 
        [%e Exp.constant (Pconst_string(vi_name,None))] 
        [%e func_ctype ~loc fs_ret fs_args] ]

  let pvar loc s = Pat.var (Location.mkloc s loc)

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


  let rec gen_cstruct loc ci =
    let ci = 
      if ci.ci_name="" then 
        { ci with ci_name="_anon"; ci_members=List.rev ci.ci_members } 
      else 
        { ci with ci_members=List.rev ci.ci_members }
    in
    let typ = Ast_helper.Typ.variant 
        [Parsetree.Rtag((if ci.ci_kind=Struct then "struct_" else "union_") ^ ci.ci_name, 
                        [], true, [])] 
        Asttypes.Closed None
    in
    let pstruct = pvar loc ci.ci_name in
    let ename = str ci.ci_name in
    let method_ loc txt exp = 
      Cf.method_ {txt;loc} Public (Cfk_concrete(Fresh, exp))
    in
    
    let gen_field = 
      let rec subctype ~loc ~sub = function 
        | TPtr(t,_) -> [%expr [%e evar "ptr"] [%e subctype ~loc ~sub t]]
        | TArray(t,s) -> 
          if s = 0L then subctype ~loc ~sub (TPtr(t,false))
          else [%expr [%e evar "array"] 
                  [%e Ast_convenience.int (Int64.to_int s)] 
                  [%e subctype ~loc ~sub t]]
        | TComp(ci) -> [%expr [%e evar sub].ctype]
        | TEnum(ei) -> [%expr [%e evar sub].ctype]
        | _ -> 
          error ~loc "anonymous sub-structure fields may only have pointer or array types"
      in
      let estruct = evar ci.ci_name in
      function
      | Field(fi) ->
        fi.fi_name,
        [%expr field [%e estruct] [%e str fi.fi_name] [%e ctype loc fi.fi_typ]],
        method_ loc fi.fi_name (evar fi.fi_name)

      | CompField(ci,fi) ->
        let substruct = gen_cstruct loc ci in
        fi.fi_name,
        [%expr let [%p pvar loc fi.fi_name] = [%e substruct] in
          { Coc_runtime.field = 
              field [%e estruct] [%e str fi.fi_name] 
                [%e subctype ~loc ~sub:fi.fi_name fi.fi_typ]; 
            structure = [%e evar fi.fi_name] }], 
        method_ loc fi.fi_name (evar fi.fi_name)

      | EnumField(ei,fi) ->
        let subenum = gen_enum loc ei.ei_name ei.ei_items in
        fi.fi_name,
        [%expr let [%p pvar loc fi.fi_name] = [%e subenum] in
          { Coc_runtime.field = 
              field [%e estruct] [%e str fi.fi_name] 
                [%e subctype ~loc ~sub:fi.fi_name fi.fi_typ]; 
            enum = [%e evar fi.fi_name] }], 
        method_ loc fi.fi_name (evar fi.fi_name)

      | _ as x -> error ~loc "unsupported struct/union field %s" 
                                  (Cparse.show_comp_member x)
    in

    let fields = List.map gen_field ci.ci_members in
    let obj = Exp.object_ (Cstr.mk (Pat.any()) (List.map (fun (_,_,m) -> m) fields)) in

    let fields_and_obj = 
      List.fold_right 
        (fun (n,f,_) l -> [%expr let [%p pvar loc n] = [%e f] in [%e l]])
        fields
        [%expr let () = seal [%e evar ci.ci_name] in 
          { Coc_runtime.ctype=[%e evar ci.ci_name]; members=[%e obj]; } ] 
    in
    
    match ci.ci_kind with
    | Struct -> 
      [%expr let [%p pstruct] : [%t typ] structure typ = structure [%e ename] in 
        [%e fields_and_obj ] ]
    | Union -> 
      [%expr let [%p pstruct] : [%t typ] union typ = union [%e ename] in 
        [%e fields_and_obj ] ]

  let gen_ccode ~loc ~attrs ~code = 
    let gen = function

      | GFunc {vi_name; 
               vi_typ=TFuncPtr { fs_args; fs_ret; fs_variadic=false };
               vi_val=None; vi_is_const=false} -> 
        vi_name, gen_cfn loc vi_name fs_args fs_ret

      | GComp ci ->
        ci.ci_name, gen_cstruct loc ci

      | GEnum{ei_name; ei_items; ei_kind} ->
        ei_name, gen_enum loc ei_name ei_items

      | GType{ti_name; ti_typ} ->
        ti_name, ctype loc ti_typ
        
      | _ as g -> error ~loc "unsupported c global [%s]" (Cparse.show_global g)

    in
    run ~loc ~attrs ~code @@ fun ctx -> List.map gen (List.rev ctx.globals)

  let ccode ~loc ~attrs ~code = 
    let code = gen_ccode ~loc ~attrs ~code in
    List.map (fun (name,expr) -> [%stri let [%p pvar loc name] = [%e expr]]) code

  let coc_mapper argv = 

    { default_mapper with

      expr = begin fun mapper expr -> 

        match expr with

        | {pexp_desc=Pexp_constant(Pconst_string(code,Some("ccode")));
                       pexp_loc=loc; pexp_attributes=attrs} ->
          let attrs = Attrs.get attrs in
          snd @@ List.hd @@ List.rev @@ gen_ccode ~loc ~attrs ~code 

        | _ -> default_mapper.expr mapper expr 

      end;

      structure = begin fun mapper items ->
        match items with

        | [%stri [%e? {pexp_desc=Pexp_constant(Pconst_string(code,Some("ccode")));
                       pexp_loc=loc; pexp_attributes=attrs}]] :: rest ->
          let attrs = Attrs.get attrs in
          ccode ~loc ~attrs ~code @ mapper.structure mapper rest

        | h::rest -> mapper.structure_item mapper h :: mapper.structure mapper rest
        | [] -> []
      end;

    }

  let register () = register "cfn" coc_mapper

end


