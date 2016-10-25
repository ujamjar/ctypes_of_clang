module L = Log.Make(struct let section = "gen" end)

module M = Map.Make(String)

(* create ocaml compatible lower case identifier from C-ident. *)
let ocaml_lid s = 
  if s = "" then "_anonymous"
  else if Char.uppercase_ascii s.[0] = s.[0] then 
    let has_lowercase s = 
      let b = ref false in
      String.iter (function 'a'..'z' -> b := true | _ -> ()) s;
      !b
    in
    if String.length s = 1 || has_lowercase s then String.uncapitalize_ascii s
    else "_" ^ s
  else s

(* name mangler - add numeric suffixes for repeated names *)
let mk_mangler init = 
  let mangler = ref init in
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
  mangle

let ocaml_keywords = 
  [
    "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
    "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
    "lazy"; "let"; "match"; "method"; "module"; "mutable"; "new"; "object";
    "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to";
    "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";
  ]

let ctypes_keywords = 
  [
    "structure"; "union"; "field"; "returning";
    "void"; "schar"; "uchar"; "short"; "ushort"; "int32_t";
    "uint32_t"; "long"; "ulong"; "llong"; "ullong"; "float";
    "double"; "ptr"; "array";
  ]

let foreign_keywords = [ "foreign" ]

let field_prefix = "_tmp_field_"

module Make(Clang : Coc_clang.S) = struct

  module Cparse = Coc_parse.Make(Clang)
  open Cparse

  open Ast_mapper
  open Ast_helper
  open Asttypes
  open Parsetree
  open Longident
  open Ast_convenience

  let initial_mangler = List.fold_left (fun map str -> M.add str 0 map) M.empty ocaml_keywords

  let error ?loc str = 
    let loc = match loc with Some(loc) -> loc | None -> !default_loc in
    Printf.kprintf (fun str -> raise (Location.Error(Location.error ~loc str))) str

  let cerror ?loc errors = 
    error ?loc
      "%s" (String.concat "\n" (List.map snd errors))

  module Attrs = struct

    type t = 
      {
        mutable clangargs : string list;
        mutable ctypesmodule : string;
        mutable foreignmodule : string;
      }

    let get_str loc = function
      | {pexp_desc=Pexp_constant(Pconst_string(s,_))} -> s
      | _ -> error ~loc "expecting string"

    let rec get_str_list loc = function
      | [%expr []] -> []
      | [%expr [%e? {pexp_desc=Pexp_constant(Pconst_string(h,_))}] :: [%e? t]] -> 
        h :: get_str_list loc t
      | _ -> error ~loc "expecting list of strings"

    let get a = 
      let attrs = 
        {
          clangargs = [];
          ctypesmodule = "Ctypes";
          foreignmodule = "Foreign";
        }
      in
      let rec get_attr = function

        | ({txt="clangargs";loc}, PStr [ [%stri [%e? args]] ]) -> 
          attrs.clangargs <- attrs.clangargs @ get_str_list loc args

        | ({txt="ctypesmodule";loc}, PStr [ [%stri [%e? args]] ]) -> 
          attrs.ctypesmodule <- get_str loc args

        | ({txt="foreignmodule";loc}, PStr [ [%stri [%e? args]] ]) -> 
          attrs.foreignmodule <- get_str loc args

        | ({txt="logfile";loc}, PStr [ [%stri [%e? args]] ]) -> begin
          let logfile = open_out (get_str loc args) in
          Log.set_log_level Log.WARN;
          Log.color_off ();
          Log.set_output logfile;
          at_exit (fun () -> close_out logfile)
        end

        | _ -> ()
      in
      List.iter get_attr a;
      attrs

  end

  open Attrs

  type t = 
    {
      loc : Location.t;
      attrs : Attrs.t;
      mangle : string -> string;
      typetbl :
        ([ `Comp of string
         | `Enum of string
         | `Typedef of string ], 
         string)
        Hashtbl.t;
    }

  let ctypes_str attrs v = 
    if attrs.ctypesmodule = "" then v
    else (attrs.ctypesmodule ^ "." ^ v)

  let foreign_str attrs v = 
    if attrs.foreignmodule = "" then v
    else (attrs.foreignmodule ^ "." ^ v)

  let ctypes_evar attrs v = evar (ctypes_str attrs v)
  let foreign_evar attrs v = evar (foreign_str attrs v)
  let ctypes_lid loc attrs v = Location.mkloc (Longident.parse (ctypes_str attrs v)) loc
  let foreign_lid loc attrs v = Location.mkloc (Longident.parse (foreign_str attrs v)) loc

  (*let carrow a b = [%expr ([%e a] @-> [%e b])]*)
  let carrow ~attrs a b = [%expr ([%e ctypes_evar attrs "@->"] [%e a] [%e b])]

  let rec ctype ~ctx t =
    let find t = 
      try Hashtbl.find ctx.typetbl t
      with Not_found ->
        let typ,name = 
          match t with 
          | `Comp n -> "comp", n 
          | `Enum n -> "enum", n 
          | `Typedef n -> "typedef", n
        in
        error "failed to look up type %s (%s)" name typ 
    in
    match t with
    | TVoid -> ctypes_evar ctx.attrs "void"
    (*| TInt(IBool) ->*)
    | TInt(ISChar) -> ctypes_evar ctx.attrs "schar"
    | TInt(IUChar) -> ctypes_evar ctx.attrs "uchar"
    | TInt(IShort) -> ctypes_evar ctx.attrs "short"
    | TInt(IUShort) -> ctypes_evar ctx.attrs "ushort"
    | TInt(IInt) -> ctypes_evar ctx.attrs "int32_t"
    | TInt(IUInt) -> ctypes_evar ctx.attrs "uint32_t"
    | TInt(ILong) -> ctypes_evar ctx.attrs "long"
    | TInt(IULong) -> ctypes_evar ctx.attrs "ulong"
    | TInt(ILongLong) -> ctypes_evar ctx.attrs "llong"
    | TInt(IULongLong) -> ctypes_evar ctx.attrs "ullong"
    | TFloat(FFloat) -> ctypes_evar ctx.attrs "float"
    | TFloat(FDouble) -> ctypes_evar ctx.attrs "double"
    | TPtr(t,_) -> [%expr [%e ctypes_evar ctx.attrs "ptr"] [%e ctype ~ctx t]]
    | TNamed{ti_name="__builtin_va_list"} -> ctype ~ctx (TPtr(TVoid,false))
    | TNamed(t) -> evar (find (`Typedef (t.ti_name)))
    | TArray(t,s) -> 
      if s = 0L then ctype ~ctx (TPtr(t,false))
      else [%expr [%e ctypes_evar ctx.attrs "array"] 
              [%e Ast_convenience.int (Int64.to_int s)] 
              [%e ctype ~ctx t]]
    | TComp(ci) -> [%expr [%e evar (find (`Comp ci.ci_name))].ctype]
    | TEnum(ei) -> [%expr [%e evar (find (`Enum ei.ei_name))].ctype]
    | TFuncPtr(fs) -> 
      if fs.fs_variadic then error ~loc:ctx.loc "no support for variadic functions"
      else [%expr funptr [%e func_ctype ~ctx fs.fs_ret fs.fs_args]]
    | TFuncProto _ -> error ~loc:ctx.loc "FIXME: TFuncProto?"
    | _ -> error ~loc:ctx.loc "unsupported type '%s'" (show_typ t)

  and func_ctype ~ctx ret args = 
    let args = if args = [] then [TVoid] else (List.map snd args) in
    let fsig = 
      List.fold_right (fun a r -> carrow ~attrs:ctx.attrs (ctype ~ctx a) r) args 
        [%expr 
          [%e ctypes_evar ctx.attrs "returning"]
            [%e ctype ~ctx ret]]
    in
    fsig

  let run ~ctx ~code f = 
    match Cparse.run ~unsaved:["coc_ppx.c",code] ("coc_ppx.c"::ctx.attrs.clangargs) with
    | Ok (g) -> f g
    | Error (errs) -> cerror ~loc:ctx.loc errs

  let gen_cfn ~ctx vi_name fs_args fs_ret = 
    [%expr [%e foreign_evar ctx.attrs "foreign"] 
        [%e Exp.constant (Pconst_string(vi_name,None))] 
        [%e func_ctype ~ctx fs_ret fs_args] ]

  let pvar loc s = Pat.var (Location.mkloc s loc)

  (* XXX phantom type no longer really makes sense 
     XXX of_int not necessarily unique
     XXX consider implementation via assoc lists or constants
  *)
  let gen_enum ~ctx items = 
    let loc = ctx.loc in
    let items = 
      let mangle = mk_mangler initial_mangler in
      List.map (fun i -> { i with eit_name = mangle i.eit_name }) (List.rev items) 
    in
    let to_int = 
      Exp.function_ ~loc @@
      List.map (fun i -> 
        Exp.case 
          (Pat.variant ~loc i.eit_name None) 
          (int ~loc (Int64.to_int i.eit_val))) items
    in
    let of_int =
      let default = Exp.case (Pat.any ()) [%expr failwith "enum to_int"] in
      (* XXX integer values ought to be unique - warn/error? *)
      Exp.function_ ~loc @@
        (List.map (fun i -> 
          Exp.case 
            (Pat.constant ~loc (Const.int (Int64.to_int i.eit_val)))
            (Exp.variant ~loc i.eit_name None))
          items) @ [default]
    in
    let int = ctypes_evar ctx.attrs "int" in
    [%expr
      let to_int, of_int = [%e to_int], [%e of_int] in
      { Coc_runtime.ctype = [%e int];
        to_int; of_int }]

  let rec gen_cstruct ~ctx binding_name ci =
    let loc, attrs = ctx.loc, ctx.attrs in
    let ci = { ci with ci_members=List.rev ci.ci_members } in
    let typ = Ast_helper.Typ.variant 
        [Parsetree.Rtag(binding_name, [], true, [])] 
        Asttypes.Closed None
    in
    let pstruct = pvar loc "_ctype" in
    let estruct = evar "_ctype" in
    let ename = str ci.ci_name in
    let method_ loc txt exp = Cf.method_ {txt;loc} Public (Cfk_concrete(Fresh, exp)) in
    
    let gen_field i = 
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
      let field = ctypes_evar attrs "field" in
      function
      | Field(fi) ->
        let name = "field_" ^ string_of_int i in
        name,
        [%expr [%e field] [%e estruct] [%e str fi.fi_name] [%e ctype ~ctx fi.fi_typ]],
        method_ loc fi.fi_name (evar name)

      | CompField(ci,fi) ->
        let subname = ctx.mangle "" in
        let substruct = gen_cstruct ~ctx subname ci in
        let name = "comp_field_" ^ string_of_int i in
        name,
        [%expr let [%p pvar loc subname] = [%e substruct] in
          { Coc_runtime.field = 
              [%e field] [%e estruct] [%e str fi.fi_name] 
                [%e subctype ~loc ~sub:subname fi.fi_typ]; 
            structure = [%e evar subname] }], 
        method_ loc fi.fi_name (evar name)

      | EnumField(ei,fi) ->
        let subenum = gen_enum ~ctx ei.ei_items in
        let name = "enum_field_" ^ string_of_int i in
        name,
        [%expr let [%p pvar loc fi.fi_name] = [%e subenum] in
          { Coc_runtime.field = 
              [%e field] [%e estruct] [%e str fi.fi_name] 
                [%e subctype ~loc ~sub:fi.fi_name fi.fi_typ]; 
            enum = [%e evar fi.fi_name] }], 
        method_ loc fi.fi_name (evar name)

      | _ as x -> error ~loc "unsupported struct/union field %s" 
                                  (Cparse.show_comp_member x)
    in

    let fields = List.mapi gen_field ci.ci_members in
    let obj = Exp.object_ (Cstr.mk (Pat.any()) (List.map (fun (_,_,m) -> m) fields)) in

    let fields_and_obj = 
      let seal = ctypes_evar attrs "seal" in
      List.fold_right 
        (fun (n,f,_) l -> [%expr let [%p pvar loc n] = [%e f] in [%e l]])
        fields
        [%expr let () = [%e seal] [%e estruct] in 
          { Coc_runtime.ctype=[%e estruct]; members=[%e obj]; } ] 
    in
    
    let typ su = 
      Typ.constr (ctypes_lid loc attrs "typ") 
        [ Typ.constr (ctypes_lid loc attrs su) [ typ ] ]
    in

    match ci.ci_kind with
    | Struct -> 
      [%expr let [%p pstruct] : [%t typ "structure"] = [%e ctypes_evar attrs "structure"] [%e ename] in 
        [%e fields_and_obj ] ]
    | Union -> 
      [%expr let [%p pstruct] : [%t typ "union"] = [%e ctypes_evar attrs "union"] [%e ename] in 
        [%e fields_and_obj ] ]

  let gen_ccode ~ctx ~code = 
    let gen = function

      | GFunc {vi_name; 
               vi_typ=TFuncPtr { fs_args; fs_ret; fs_variadic=false };
               vi_val=None; vi_is_const=false} -> 
        let binding_name = ctx.mangle vi_name in
        Some(binding_name, gen_cfn ~ctx vi_name fs_args fs_ret)

      | GComp ci ->
        let binding_name = ctx.mangle ci.ci_name in
        Hashtbl.add ctx.typetbl (`Comp ci.ci_name) binding_name;
        Some(binding_name, gen_cstruct ~ctx binding_name ci)

      | GEnum{ei_name; ei_items; ei_kind} ->
        let binding_name = ctx.mangle ei_name in
        Hashtbl.add ctx.typetbl (`Enum ei_name) binding_name;
        Some(binding_name, gen_enum ~ctx ei_items)

      (* | GCompDecl | GEnumDecl *)

      | GType{ti_name; ti_typ} ->
        let binding_name = ctx.mangle ti_name in
        Hashtbl.add ctx.typetbl (`Typedef ti_name) binding_name;
        Some(binding_name, ctype ~ctx ti_typ)
      
      | _ when true -> None

      | _ as g -> error ~loc:ctx.loc "unsupported c global [%s]" (Cparse.show_global g)

    in
    let rec map f = function 
      | [] -> [] 
      | h :: t -> 
        (match f h with 
        | None -> map f t 
        | Some(h) -> h :: map f t)
    in
    run ~ctx ~code @@ fun ctx -> map gen (List.rev ctx.globals)

  let ccode ~ctx ~code = 
    let code = gen_ccode ~ctx ~code in
    List.map (fun (name,expr) -> [%stri let [%p pvar ctx.loc name] = [%e expr]]) code

  let coc_mapper argv = 
    let init_ctx loc attrs = 
      let mangle = mk_mangler initial_mangler in
      {
        loc; attrs=Attrs.get attrs; 
        mangle = (fun s -> mangle (ocaml_lid s));
        typetbl = Hashtbl.create 113;
      }
    in

    { default_mapper with

      expr = begin fun mapper expr -> 

        match expr with

        | {pexp_desc=Pexp_constant(Pconst_string(code,Some("ccode")));
                       pexp_loc=loc; pexp_attributes=attrs} ->
          let ctx = init_ctx loc attrs in
          snd @@ List.hd @@ List.rev @@ gen_ccode ~ctx ~code 

        | _ -> default_mapper.expr mapper expr 

      end;

      structure = begin fun mapper items ->
        match items with

        | [%stri [%e? {pexp_desc=Pexp_constant(Pconst_string(code,Some("ccode")));
                       pexp_loc=loc; pexp_attributes=attrs}]] :: rest ->
          let ctx = init_ctx loc attrs in
          ccode ~ctx ~code @ mapper.structure mapper rest

        | h::rest -> mapper.structure_item mapper h :: mapper.structure mapper rest
        | [] -> []
      end;

    }

  let register () = register "cfn" coc_mapper

end


