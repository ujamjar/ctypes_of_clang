module L = Log.Make(struct let section = "gen" end)

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

module Make(Clang : Coc_clang.S) = struct

  module Cparse = Coc_parse.Make(Clang)
  open Cparse

  open Ast_mapper
  open Ast_helper
  open Asttypes
  open Parsetree
  open Longident
  open Ast_convenience

  let initial_mangler = Coc_ident.(init ocaml_keywords)

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
        mutable foreignfnmodule : string;
        mutable typesmodule : string;
        mutable gentypes : bool;
        mutable gendecls : bool;
        mutable staticstructs : bool;
        mutable deferbindingexn : bool;
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
          foreignfnmodule = "Ctypes";
          typesmodule = "";
          gentypes = true;
          gendecls = true;
          staticstructs = false;
          deferbindingexn = false;
        }
      in
      let rec get_attr = function

        | ({txt="clangargs";loc}, PStr [ [%stri [%e? args]] ]) -> 
          attrs.clangargs <- attrs.clangargs @ get_str_list loc args

        | ({txt="ctypesmodule";loc}, PStr [ [%stri [%e? args]] ]) -> 
          attrs.ctypesmodule <- get_str loc args

        | ({txt="foreignmodule";loc}, PStr [ [%stri [%e? args]] ]) -> 
          attrs.foreignmodule <- get_str loc args

        | ({txt="foreignfnmodule";loc}, PStr [ [%stri [%e? args]] ]) -> 
          attrs.foreignfnmodule <- get_str loc args

        | ({txt="typesmodule";loc}, PStr [ [%stri [%e? args]] ]) -> 
          attrs.typesmodule <- get_str loc args

        | ({txt="onlytypes";loc}, _) -> 
          attrs.gendecls <- false;

        | ({txt="onlydecls";loc}, _) -> 
          attrs.gentypes <- false;

        | ({txt="deferbindingexn";loc}, _) -> 
          attrs.deferbindingexn <- true;

        | ({txt="staticstructs";loc}, _) -> 
          attrs.staticstructs <- true;

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

  module G = Map.Make(struct
      type t = global
      let compare = compare
  end)

  type t = 
    {
      loc : Location.t;
      attrs : Attrs.t;
      mangle : string -> string;
      global_to_binding : string G.t;
      builtins : global list;
      comp_members_map : member list TypeMap.t;
      enum_items_map : ((string * int64) list * typ) TypeMap.t;
    }

  let _str attr v = if attr = "" then v else attr ^ "." ^ v

  let ctypes_str attrs = _str attrs.ctypesmodule
  let foreign_str attrs = _str attrs.foreignmodule 
  let foreignfn_str attrs = _str attrs.foreignfnmodule 
  let types_str attrs = _str attrs.typesmodule

  let ctypes_evar attrs v = evar (ctypes_str attrs v)
  let foreign_evar attrs v = evar (foreign_str attrs v)
  let foreignfn_evar attrs v = evar (foreignfn_str attrs v)
  let types_evar attrs v = evar (types_str attrs v)

  let lid loc v = Location.mkloc (Longident.parse v) loc
  let ctypes_lid loc attrs v =  lid loc (ctypes_str attrs v)

  (*let carrow a b = [%expr ([%e a] @-> [%e b])]*)
  let carrow ~attrs a b = [%expr ([%e foreignfn_evar attrs "@->"] [%e a] [%e b])]

  let rec ctype ~ctx t =
    let find g = 
      try G.find g ctx.global_to_binding
      with _ -> error ~loc:ctx.loc "ctype: cannot find %s" (name_of_global g)
    in
    match t with
    | TVoid -> ctypes_evar ctx.attrs "void"
    | TBase(t) -> ctypes_evar ctx.attrs t
    | TNamed(t) -> evar t
    | TPtr(t) -> [%expr [%e ctypes_evar ctx.attrs "ptr"] [%e ctype ~ctx t]]
    | TArray(t,s) -> 
      if s = 0 then ctype ~ctx (TPtr(t))
      else [%expr [%e ctypes_evar ctx.attrs "array"] 
              [%e Ast_convenience.int s] 
              [%e ctype ~ctx t]]
    | TComp{global} -> types_evar ctx.attrs (find global)
    | TEnum{global} -> [%expr [%e types_evar ctx.attrs (find global)].ctype]
    | TFuncPtr{ret;args;variadic} -> 
      if variadic then error ~loc:ctx.loc "no support for variadic functions"
      else [%expr [%e ctypes_evar ctx.attrs "static_funptr"] [%e func_ctype ~ctx ret args] ]
    | TGlobal(global) -> types_evar ctx.attrs (find global)
    (*| _ -> error ~loc:ctx.loc "unsupported type '%s'" (string_of_typ t)*)

  and func_ctype ~ctx ret args = 
    let args = if args = [] then [TVoid] else args in
    let fsig = 
      List.fold_right (fun a r -> carrow ~attrs:ctx.attrs (ctype ~ctx a) r) args 
        [%expr 
          [%e foreignfn_evar ctx.attrs "returning"]
            [%e ctype ~ctx ret]]
    in
    fsig

  let run ~ctx ~code f = 
    match Cparse.run ~unsaved:["coc_ppx.c",code]
            ~builtins:ctx.builtins
            ("coc_ppx.c"::ctx.attrs.clangargs) with
    | Ok (g) -> f g
    | Error (errs) -> cerror ~loc:ctx.loc errs

  let gen_cfn ~ctx fs_ret vi_name fs_args = 
    let defer e = 
      if ctx.attrs.deferbindingexn then [%expr try [%e e] with e -> (fun _ -> raise e)]
      else e
    in
    defer [%expr [%e foreign_evar ctx.attrs "foreign"] 
                   [%e Exp.constant (Pconst_string(vi_name,None))] 
                   [%e func_ctype ~ctx fs_ret fs_args] ]

  let gen_cvar ~ctx name typ = 
    [%expr [%e foreign_evar ctx.attrs "foreign_value"]
             [%e Exp.constant (Pconst_string(name,None))] 
             [%e ctype ~ctx typ]]

  let pvar loc s = Pat.var (Location.mkloc s loc)

  let gen_enum ~ctx items kind = 
    let loc = ctx.loc in
    let items = 
      let mangle = Coc_ident.make initial_mangler in
      List.map (fun (n,v) -> mangle n, v) items 
    in
    let to_int = 
      let int64 ?loc ?attrs x = 
        Exp.constant ?loc ?attrs 
          (Pconst_integer (Int64.to_string x, Some 'L')) in
      Exp.function_ ~loc @@
      List.map (fun (n,v) -> 
        Exp.case 
          (Pat.variant ~loc n None) 
          (int64 ~loc v)) items
    in
    let of_int =
      let default = Exp.case (Pat.any ()) [%expr failwith "enum to_int"] in
      (* XXX integer values ought to be unique - warn/error? *)
      Exp.function_ ~loc @@
        (List.map (fun (n,v) -> 
          Exp.case 
            (Pat.constant ~loc (Const.int64 v))
            (Exp.variant ~loc n None))
          items) @ [default]
    in
    [%expr
      let to_int, of_int = [%e to_int], [%e of_int] in
      { Coc_runtime.ctype = [%e ctype ~ctx kind];
        to_int; of_int }]

  let gen_cstruct_decl ~ctx binding_name name kind next = 
    let loc, attrs = ctx.loc, ctx.attrs in
    let ename = str name in
    let pstruct = pvar loc "_ctype" in

    let typ = Ast_helper.Typ.variant 
        [Parsetree.Rtag(binding_name, [], true, [])] 
        Asttypes.Closed None
    in
    let typ su = 
      Typ.constr (ctypes_lid loc attrs "typ") 
        [ Typ.constr (lid loc ("Ctypes." ^ su)) [ typ ] ]
    in

    match kind with
    | Struct -> 
      [%expr let [%p pstruct] : [%t typ "structure"] = [%e ctypes_evar attrs "structure"] [%e ename] in 
        [%e next ] ]
    | Union -> 
      [%expr let [%p pstruct] : [%t typ "union"] = [%e ctypes_evar attrs "union"] [%e ename] in 
        [%e next ] ]

  let gen_cstruct ~ctx binding_name name members =
    let loc, attrs = ctx.loc, ctx.attrs in
    let estruct = evar binding_name in
    let method_ loc txt exp = Cf.method_ {txt;loc} Public (Cfk_concrete(Fresh, exp)) in
    
    let mangle = Coc_ident.make initial_mangler in
    let gen_field i = 
      let field = ctypes_evar attrs "field" in
      function 
      | Field{name=n;typ=t} ->
        let name = "field_" ^ string_of_int i in
        name,
        [%expr [%e field] [%e estruct] [%e str n] [%e ctype ~ctx t]],
        method_ loc (mangle n) (evar name)
      | Bitfield _ -> error "unexpected bitfield"
    in

    let fields = List.mapi gen_field members in
    let obj = Exp.object_ (Cstr.mk (Pat.any()) (List.map (fun (_,_,m) -> m) fields)) in

    let fields_and_obj = 
      let seal = ctypes_evar attrs "seal" in
      let e = [%expr { Coc_runtime.ctype=[%e estruct]; members=[%e obj]; } ] in
      let e = if members = [] then e else [%expr let () = [%e seal] [%e estruct] in [%e e]] in
      List.fold_right 
        (fun (n,f,_) l -> [%expr let [%p pvar loc n] = [%e f] in [%e l]])
        fields e
    in
    fields_and_obj 

  let gen_cstruct_static ~ctx binding_name name members clayout =
    let loc, attrs = ctx.loc, ctx.attrs in
    let estruct = evar binding_name in
    let method_ loc txt exp = Cf.method_ {txt;loc} Public (Cfk_concrete(Fresh, exp)) in
    
    let mangle = Coc_ident.make initial_mangler in
    let gen_field i = 
      function 
      | Field{name=n;typ=t;offset=o} ->
        let name = "field_" ^ string_of_int i in
        name,
        [%expr Coc_runtime.field ~offset:[%e int (o/8)] 
            [%e estruct] [%e str n] [%e ctype ~ctx t]],
        method_ loc (mangle n) (evar name)
      | Bitfield _ -> error "unexpected bitfield"
    in

    let fields = List.mapi gen_field members in
    let obj = Exp.object_ (Cstr.mk (Pat.any()) (List.map (fun (_,_,m) -> m) fields)) in

    let seal e = 
      [%expr let () = Coc_runtime.seal ~size:[%e int clayout.size] ~align:[%e int clayout.align] [%e estruct] in [%e e] ] 
    in

    let fields_and_obj = 
      let e = [%expr { Coc_runtime.ctype=[%e estruct]; members=[%e obj]; } ] in
      let e = 
        if members = [] then e
        else seal e
      in
      List.fold_right 
        (fun (n,f,_) l -> [%expr let [%p pvar loc n] = [%e f] in [%e l]])
        fields e
    in
    fields_and_obj 

  let rec fmap f = function
    | [] -> []
    | h :: t -> 
      (match  f h with
      | Some(h) -> h :: fmap f t
      | None -> fmap f t)

  let mangle_declarations ~ctx g = 
    let g = List.map (fun g -> let name = name_of_global g in name, name, g) g in
    
    (* mangle names for functions, vars, typedefs, structs then enums *)
    let mangle p (n,m,g) = 
      if p g then n, ctx.mangle m, g
      else n, m, g
    in
    let g = List.map (mangle (function GFunc _ -> true | _ -> false)) g in
    let g = List.map (mangle (function GVar _ -> true | _ -> false)) g in
    let g = List.map (mangle (function GTypedef _ -> true | _ -> false)) g in
    let g = List.map (mangle (function GComp _ -> true | _ -> false)) g in
    let g = List.map (mangle (function GEnum _ -> true | _ -> false)) g in

    (* now create the name of the forward declaration *)
    let g = List.map (function (n,m,g) -> ctx.mangle n, m, g) g in

    g

  let global_bindings_map ~loc builtins globals =
    let map = List.fold_left 
      (fun map -> function (GBuiltin{name;typ=TNamed(oname)} as g) -> G.add g oname map
                         | _ -> error ~loc "") G.empty builtins
    in
    List.fold_left 
      (fun map -> function (n, m, (GComp _ as g)) -> G.add g n map
                         | (n, m, (_ as g)) -> G.add g m map) map globals

  let get_members ~ctx global = 
    let aligned_array size align = 
      if size <> 0 && align <> 0 && size >= align && size mod align = 0 then
        match align with (* we can probably do a bit better than this. *)
        | 1 -> [ Field{name="auto_array"; offset=0; typ=TArray(Cparse.BT.char,  size  )} ]
        | 2 -> [ Field{name="auto_array"; offset=0; typ=TArray(Cparse.BT.short, size/2)} ]
        | 4 -> [ Field{name="auto_array"; offset=0; typ=TArray(Cparse.BT.int,   size/4)} ]
        | 8 -> [ Field{name="auto_array"; offset=0; typ=TArray(Cparse.BT.llong, size/8)} ]
        | _ -> []
      else []
    in
    match global with
    | GComp { clayout={size;align} } -> begin
      match TypeMap.find global ctx.comp_members_map with
      | [] -> 
        L.warn "no members: replacing structure with array [size=%i align=%i]" size align;
        aligned_array size align
      | _ as members ->
        let rec has_bitfields = 
          function [] -> false 
                 | Field _ :: t-> has_bitfields t
                 | Bitfield _ :: t -> true
        in
        if has_bitfields members then 
          aligned_array size align
        else 
          members
      | exception Not_found -> 
        L.warn "Not_found: replacing structure with array [size=%i align=%i]" size align;
        aligned_array size align
    end
    | _ -> error "get_members expecting composite"
   
  let get_enum_items ~ctx global = 
    try 
      TypeMap.find global ctx.enum_items_map 
    with Not_found -> 
      [], default_enum_type

  let gen_ccode ~ctx ~code = 

    let fwd_decl ~ctx (b0, b1, global) = 
      if not ctx.attrs.gentypes then None
      else
        match global with
        | GComp {name=(name,_); kind} -> 
          Some(b0, gen_cstruct_decl ~ctx b1 name kind (evar "_ctype"))

        | GEnum { name=(name,_) } ->
          let items, kind = get_enum_items ~ctx global in
          Some(b1, gen_enum ~ctx items kind)

        | GTypedef { name=(name,_); typ } ->
          Some(b1, ctype ~ctx typ) (* XXX ctypedef? *)
       
        | GVar _ | GFunc _ -> None

        | GBuiltin _ -> error ~loc:ctx.loc "unexpected builtin"

    in

    let gen_decl ~ctx (b0, b1, global) = 
      match global with

      | GComp { name=(name,_); clayout } when ctx.attrs.gentypes ->
        let members = get_members ~ctx global in
        if ctx.attrs.staticstructs then
          Some(b1, gen_cstruct_static ~ctx b0 name members clayout)
        else
          Some(b1, gen_cstruct ~ctx b0 name members)

      | GVar { name=(name,_); typ } when ctx.attrs.gendecls -> 
        Some(b1, gen_cvar ~ctx name typ)

      | GFunc { loc; name=(name,_); typ=TFuncPtr{ret;args;variadic} } when ctx.attrs.gendecls -> 
        Some(b1, gen_cfn ~ctx ret name args)

      | GEnum _ | GTypedef _ | GFunc _ -> None

      | GBuiltin _ -> error ~loc:ctx.loc "unexpected builtin"

      | _ -> None
    in

    run ~ctx ~code @@ fun c_ctx -> 
      let globals = mangle_declarations ~ctx c_ctx.globals in
      let ctx = { ctx with 
        global_to_binding = global_bindings_map ~loc:ctx.loc ctx.builtins globals;
        comp_members_map = c_ctx.comp_members_map;
        enum_items_map = c_ctx.enum_items_map;
      } in
      fmap (fwd_decl ~ctx) globals @ fmap (gen_decl ~ctx) globals

  let ccode ~ctx ~code = 
    let code = gen_ccode ~ctx ~code in
    List.map (fun (name,expr) -> [%stri let [%p pvar ctx.loc name] = [%e expr]]) code

  let builtins = 
    [ 
      GBuiltin{name="__builtin_va_list"; typ=TNamed("Coc_runtime.__builtin_va_list")} 
    ]

  let coc_mapper argv = 
    let init_ctx loc attrs = 
      let mangle = Coc_ident.make initial_mangler in
      {
        loc; attrs=Attrs.get attrs; 
        mangle = (fun s -> mangle (ocaml_lid s));
        global_to_binding = G.empty;
        builtins;
        comp_members_map = TypeMap.empty;
        enum_items_map = TypeMap.empty;
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

