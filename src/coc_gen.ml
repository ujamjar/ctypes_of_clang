module L = Log.Make(struct let section = "gen" end)

module M = Map.Make(String)

(* create ocaml compatible lower case identifier from C-ident. *)
let ocaml_lid s = 
  if s = "" then "anonymous"
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

let foreign_keywords = [ "foreign"; "funptr" ]

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

        | ({txt="loglevel";loc}, PStr [ [%stri [%e? args]] ]) -> 
          Log.set_log_level @@
            (function
            | "fatal" -> Log.FATAL
            | "error" -> Log.ERROR
            | "warn" -> Log.WARN
            | "info" -> Log.INFO
            | "debug" -> Log.DEBUG
            | s -> error ~loc "unknown log level %s" s) (get_str loc args)

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
        ([ `Type 
         | `Comp 
         | `CompDecl 
         | `Enum 
         | `EnumDecl 
         | `Var 
         | `Func ] * string,
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
          | `Type, n -> "typedef", n
          | `Comp, n -> "comp", n
          | `CompDecl, n -> "comp_decl", n
          | `Enum, n -> "enum", n
          | `EnumDecl, n -> "enum_decl", n
          | `Var, n -> "var", n
          | `Func, n -> "func", n
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
    | TNamed(t) -> evar (find (`Type, t.ti_name))
    | TArray(t,s) -> 
      if s = 0L then ctype ~ctx (TPtr(t,false))
      else [%expr [%e ctypes_evar ctx.attrs "array"] 
              [%e Ast_convenience.int (Int64.to_int s)] 
              [%e ctype ~ctx t]]
    | TComp(ci) -> evar (find (`CompDecl, ci.ci_name))
    | TEnum(ei) -> evar (find (`EnumDecl, ei.ei_name))
    | TFuncPtr(fs) -> 
      if fs.fs_variadic then error ~loc:ctx.loc "no support for variadic functions"
      else [%expr [%e foreign_evar ctx.attrs "funptr"] [%e func_ctype ~ctx fs.fs_ret fs.fs_args]]
    | TFuncProto(fs) -> 
      (*error ~loc:ctx.loc "FIXME: TFuncProto?"*)
      if fs.fs_variadic then error ~loc:ctx.loc "no support for variadic functions"
      else [%expr [%e foreign_evar ctx.attrs "funptr"] [%e func_ctype ~ctx fs.fs_ret fs.fs_args]]
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

  let gen_cstruct_decl ~ctx binding_name ci next = 
    let loc, attrs = ctx.loc, ctx.attrs in
    let ename = str ci.ci_name in
    let pstruct = pvar loc "_ctype" in

    let typ = Ast_helper.Typ.variant 
        [Parsetree.Rtag(binding_name, [], true, [])] 
        Asttypes.Closed None
    in
    let typ su = 
      Typ.constr (ctypes_lid loc attrs "typ") 
        [ Typ.constr (ctypes_lid loc attrs su) [ typ ] ]
    in

    match ci.ci_kind with
    | Struct -> 
      [%expr let [%p pstruct] : [%t typ "structure"] = [%e ctypes_evar attrs "structure"] [%e ename] in 
        [%e next ] ]
    | Union -> 
      [%expr let [%p pstruct] : [%t typ "union"] = [%e ctypes_evar attrs "union"] [%e ename] in 
        [%e next ] ]

  let rec gen_cstruct ~ctx binding_name ci =
    let loc, attrs = ctx.loc, ctx.attrs in
    let ci = { ci with ci_members=List.rev ci.ci_members } in
    let pstruct = pvar loc "_ctype" in
    let estruct = evar "_ctype" in
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

      | _ -> error ~loc "unsupported struct/union field" 
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
    match Hashtbl.find ctx.typetbl (`CompDecl,ci.ci_name) with
    | ci_decl -> 
      [%expr let [%p pstruct] = [%e evar ci_decl ] in 
                 [%e fields_and_obj] ]
    | exception Not_found -> (*error ~loc "couldn't find '%s'" ci.ci_name*)
      gen_cstruct_decl ~ctx binding_name ci fields_and_obj 

  let rec fmap f = function
    | [] -> []
    | h :: t -> 
      (match  f h with
      | Some(h) -> h :: fmap f t
      | None -> fmap f t)

  let unique_globals g = 
    let tbl = Hashtbl.create 113 in
    let rec uniqify g = 
        let find ((t,n) as x) =
          if n="" then false
          else
            match Hashtbl.find tbl x with 
            | _ -> true
            | exception Not_found -> Hashtbl.add tbl x 0; false
        in
        let unique ((t,n) as x) h tl = if find x then uniqify tl else (x,n,h) :: uniqify tl in
        match g with
        | (GType {ti_name} as h) :: tl -> unique (`Type,ti_name) h tl
        | (GComp {ci_name} as h) :: tl -> unique (`Comp,ci_name) h tl
        | (GCompDecl {ci_name} as h) :: tl -> unique (`CompDecl,ci_name) h tl
        | (GEnum {ei_name} as h) :: tl -> unique (`Enum,ei_name) h tl
        | (GEnumDecl {ei_name} as h) :: tl -> unique (`EnumDecl,ei_name) h tl
        | (GVar {vi_name} as h) :: tl -> unique (`Var,vi_name) h tl
        | (GFunc {vi_name} as h) :: tl -> unique (`Func,vi_name) h tl
        | GOther :: tl -> uniqify tl
        | [] -> []
    in
    uniqify g

  let unique_decls_and_bindings mangle g = 

    let rec manglifier p = function
      | ((t,n),m,g) :: tl when p t -> ((t,n),mangle m,g) :: manglifier p tl
      | h :: tl -> h :: manglifier p tl
      | [] -> []
    in

    let comp_decls = fmap (function GComp ci -> Some(GCompDecl {ci with ci_members=[]})
                                  | GCompDecl ci -> Some(GCompDecl ci)
                                  | _ -> None) g
    in
    let enum_decls = fmap (function GEnum ei -> Some(GEnumDecl {ei with ei_items=[]})
                                  | GEnumDecl ei -> Some(GEnumDecl ei)
                                  | _ -> None) g
    in
    let g = List.filter (function GCompDecl _  
                                | GEnumDecl _ -> false 
                                | _ -> true) g in
    let g = List.concat [ comp_decls; enum_decls; g ] in

    (* get unique declatations *)
    let g = unique_globals g in

    (* mangle names in priority order - functions, types, forward decls *)
    let g = manglifier (function `Func | `Var -> true | _ -> false) g in
    let g = manglifier (function `Type -> true | _ -> false) g in
    let g = manglifier (function `Comp | `Enum -> true | _ -> false) g in
    let g = manglifier (function `CompDecl | `EnumDecl -> true | _ -> false) g in

    g

  let rec split_globals (fs,vs,gs) g = 
    match g with
    | [] -> List.(rev fs, rev vs, rev gs)
    | GOther :: tl -> split_globals (fs,vs,gs) tl
    | GFunc _ as h :: tl -> split_globals (h::fs,vs,gs) tl
    | GVar { vi_val=Some _; vi_is_const=true } as h :: tl -> split_globals (fs,vs,h::gs) tl
    | GVar _ as h :: tl -> split_globals (fs,h::vs,gs) tl
    | _ as h :: tl -> split_globals (fs,vs,h::gs) tl

  let redundant_comp_globals g = 
    let typedefs = fmap (function GType(ti) -> Some(ti.ti_typ) | _ -> None) g in 
    let eq g t =
      match g,t with
      | GComp(c1), TComp(c2) -> c1.ci_name = "" && c1 = c2
      | GEnum(e1), TEnum(e2) -> e1.ei_name = "" && e1 = e2
      | _ -> false
    in
    let eq g = List.fold_left (fun e t -> e || eq g t) false typedefs in
    fmap (fun g -> if eq g then None else Some(g)) g

  let unnamed_comp g = fmap
    (function
      | GType(t) as g -> begin
          match t.ti_typ with
          | TComp(c) when c.ci_name = ""        -> c.ci_name <- t.ti_name; None
          | TComp(c) when c.ci_name = t.ti_name -> None
          | TEnum(e) when e.ei_name = ""        -> e.ei_name <- t.ti_name; None
          | TEnum(e) when e.ei_name = t.ti_name -> None
          | _ -> Some(g)
      end
      | _ as g -> Some(g))
    g

  let mangle_and_typetbl ctx g = 
    fmap 
      (fun g ->
        let add t n = 
          let m = ctx.mangle n in
          Hashtbl.add ctx.typetbl (t,n) m;
          Some((t, n), m, g) 
        in
        match g with
        | GType ti -> add `Type ti.ti_name 
        | GComp ci  -> add `Comp ci.ci_name
        | GCompDecl ci -> add `CompDecl ci.ci_name
        | GEnum ei  -> add `Enum ei.ei_name
        | GEnumDecl ei -> add `EnumDecl ei.ei_name
        | GVar vi -> add `Var vi.vi_name
        | GFunc vi -> add `Func vi.vi_name
        | GOther -> None) g

  let split_unique_and_unnamed ctx g = 
    let fs,vs,gs = split_globals ([],[],[]) g in
    let gs = redundant_comp_globals gs in
    let gs = unnamed_comp gs in
    let g = gs @ vs @ fs in
    mangle_and_typetbl ctx g

  let gen_ccode ~ctx ~code = 
    let gen = function

      | _, name, GFunc { vi_name; 
                         vi_typ=TFuncPtr { fs_args; fs_ret; fs_variadic=false };
                         vi_val=None; vi_is_const=false } -> 
        Some(name, gen_cfn ~ctx vi_name fs_args fs_ret)

      | _, name, GComp ci ->
        Some(name, gen_cstruct ~ctx name ci)

      | _, name, GEnum { ei_name; ei_items; ei_kind } ->
        Some(name, gen_enum ~ctx ei_items)

      | _, name, GCompDecl ci ->
        Some(name, gen_cstruct_decl ~ctx name ci (evar "_ctype"))

      | _, name, GEnumDecl ei ->
        Some(name, ctypes_evar ctx.attrs "int")

      | _, name, GType { ti_name; ti_typ } ->
        Some(name, ctype ~ctx ti_typ)
      
      | _ when true -> None

      | (_,n),m,_ -> error ~loc:ctx.loc "unsupported c global [%s - >%s]" n m

    in

    run ~ctx ~code @@ fun g -> 
      let g = List.rev g.globals in
      (*let g = unique_decls_and_bindings ctx.mangle g in
      let () = List.iter (fun (x,m,g) -> Hashtbl.add ctx.typetbl x m) g in*)
      let g = split_unique_and_unnamed ctx g in
      fmap gen g


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


