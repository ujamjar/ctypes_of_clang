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
    let find g = 
      try G.find g ctx.global_to_binding
      with _ -> error ~loc:ctx.loc "ctype: cannot find %s" (name_of_global g)
    in
    match t with
    | TVoid -> ctypes_evar ctx.attrs "void"
    | TBase(t) -> ctypes_evar ctx.attrs t
    | TPtr(t) -> [%expr [%e ctypes_evar ctx.attrs "ptr"] [%e ctype ~ctx t]]
    | TArray(t,s) -> 
      if s = 0L then ctype ~ctx (TPtr(t))
      else [%expr [%e ctypes_evar ctx.attrs "array"] 
              [%e Ast_convenience.int (Int64.to_int s)] 
              [%e ctype ~ctx t]]
    | TComp{global} -> evar (find global)
    | TEnum{global} -> [%expr [%e evar (find global)].ctype]
    | TFuncPtr{ret;args;variadic} -> 
      if variadic then error ~loc:ctx.loc "no support for variadic functions"
      else [%expr [%e foreign_evar ctx.attrs "funptr"] [%e func_ctype ~ctx ret args]]
    | TGlobal(global) -> evar (find global)
    | _ -> error ~loc:ctx.loc "unsupported type '%s'" (string_of_typ t)

  and func_ctype ~ctx ret args = 
    let args = if args = [] then [TVoid] else args in
    let fsig = 
      List.fold_right (fun a r -> carrow ~attrs:ctx.attrs (ctype ~ctx a) r) args 
        [%expr 
          [%e ctypes_evar ctx.attrs "returning"]
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
    [%expr [%e foreign_evar ctx.attrs "foreign"] 
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
        [ Typ.constr (ctypes_lid loc attrs su) [ typ ] ]
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
    
    let gen_field i = 
      let field = ctypes_evar attrs "field" in
      fun (n,t) ->
        let name = "field_" ^ string_of_int i in
        name,
        [%expr [%e field] [%e estruct] [%e str n] [%e ctype ~ctx t]],
        method_ loc n (evar name)
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

  let rec fmap f = function
    | [] -> []
    | h :: t -> 
      (match  f h with
      | Some(h) -> h :: fmap f t
      | None -> fmap f t)

  let mangle_declarations ~ctx g = 
    let g = List.map (fun (_, g) -> let name = name_of_global g in name, name, g) g in
    
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

  let global_bindings_map ~loc builtins decls =
    let map = List.fold_left 
      (fun map -> function (GBuiltin{name} as g) -> G.add g name map
                         | _ -> error ~loc "") G.empty builtins
    in
    List.fold_left 
      (fun map -> function (n, m, (GComp _ as g)) -> G.add g n map
                         | (n, m, (_ as g)) -> G.add g m map) map decls

  let gen_ccode ~ctx ~code = 

    let fwd_decl ~ctx ~c_ctx (b0, b1, global) = 
      match global with
      | GComp {name=(name,_); kind} -> 
        Some(b0, gen_cstruct_decl ~ctx b1 name kind (evar "_ctype"))

      | GEnum { name=(name,_) } ->
        let items, kind = 
          try TypeMap.find global c_ctx.enum_items_map with Not_found -> [], default_enum_type
        in
        Some(b1, gen_enum ~ctx items kind)

      | GTypedef { name=(name,_); typ } ->
        Some(b1, ctype ~ctx typ) (* XXX ctypedef? *)
     
      | _ -> None
    in

    let gen_decl ~ctx ~c_ctx (b0, b1, global) = 
      match global with

      | GFunc { loc; name=(name,_); typ=TFuncPtr{ret;args;variadic} } -> 
        Some(b1, gen_cfn ~ctx ret name args)

      | GComp { name=(name,_) } ->
        let members = 
          try TypeMap.find global c_ctx.comp_members_map with Not_found -> [] 
        in
        Some(b1, gen_cstruct ~ctx b0 name members)

      (*| GEnum { name=(name,_) } ->
        let items, kind = 
          try TypeMap.find global c_ctx.enum_items_map with Not_found -> [], default_enum_type
        in
        Some(b1, gen_enum ~ctx items kind)

      | GTypedef { name=(name,_); typ } ->
        Some(b1, ctype ~ctx typ) (* XXX ctypedef? *) *)
     
      | GEnum _ | GTypedef _ -> None

      | GVar { name=(name,_); typ } -> 
        Some(b1, gen_cvar ~ctx name typ)

      (*| _ when true -> None*)

      | _ -> error ~loc:ctx.loc "unsupported c global"

    in

    run ~ctx ~code @@ fun c_ctx -> 
      let decls = mangle_declarations ~ctx c_ctx.decls in
      let ctx = { ctx with global_to_binding = 
                             global_bindings_map ~loc:ctx.loc ctx.builtins decls } in
      fmap (fwd_decl ~ctx ~c_ctx) decls @ fmap (gen_decl ~ctx ~c_ctx) decls

  let ccode ~ctx ~code = 
    let code = gen_ccode ~ctx ~code in
    List.map (fun (name,expr) -> [%stri let [%p pvar ctx.loc name] = [%e expr]]) code

  let coc_mapper argv = 
    let init_ctx loc attrs = 
      let mangle = Coc_ident.make initial_mangler in
      {
        loc; attrs=Attrs.get attrs; 
        mangle = (fun s -> mangle (ocaml_lid s));
        global_to_binding = G.empty;
        builtins=[ GBuiltin{name="__builtin_va_list"; typ=TPtr(TVoid)} ];
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

