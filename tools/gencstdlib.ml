let headers = [
  "bits/types.h"; (* XXX not std-c, but depended on by a number of other headers *)
  "float.h";
  "limits.h";
  "stdarg.h";
  "assert.h";
  "errno.h";
  "stddef.h";
  "locale.h";
  "time.h";
  "ctype.h";
  "signal.h";
  "setjmp.h";
  "math.h";
  "stdio.h";
  "stdlib.h";
]

let na1_headers = [
  "iso646.h";
  "wchar.h";
  "wctype.h";
]

let c99_headers = [
  "stdbool.h";
  "stdint.h";	
  "inttypes.h";
  "fenv.h";
  "complex.h";
  "tgmath.h";	
]

let c11_headers = [
  "stdalign.h";	
  "stdnoreturn.h";
  "uchar.h";
  (*"stdatomic.h";*) (* dont work *)
  (*"threads.h";*)
]

let headers = (headers @ na1_headers @ c99_headers @ c11_headers)

module Clang = Coc_clang.Make(struct let from = None end)
module Cgen = Coc_gen.Make(Clang)
open Printf

let attrs = 
  let attrs = Cgen.Attrs.def_attrs () in
  { attrs with Cgen.Attrs.clangargs = [ "-I/usr/lib/clang/3.8/include" ];
               excludedecls = [ ".*:__.*" ];
               doccomments=true;
  }

let run ~headers ?(attrs=attrs) ~builtins fn = 
  let code = String.concat "\n" (List.map (sprintf "#include <%s>") headers) in
  let ctx = { (Cgen.init_ctx Location.none attrs) with Cgen.builtins } in
  Cgen.run ~ctx ~code (fun c_ctx -> fn c_ctx)

module S = Set.Make(String)

(* run through all headers and dump definitions *)
let dump_all_defns () =
  run ~headers ~builtins:[] (fun c_ctx ->
    List.iter (fun g -> 
      printf "%s %s\n" 
        (Cgen.Cparse.loc_of_global g).Clang.Loc.file
        (Cgen.Cparse.name_of_global g)) c_ctx.Cgen.Cparse.globals)

(* show how headers refer to each other *)
let get_header_deps headers = 
  run ~headers ~builtins:[] (fun c_ctx ->
      List.fold_left (fun set g -> S.add (Cgen.Cparse.loc_of_global g).Clang.Loc.file set) 
        S.empty c_ctx.Cgen.Cparse.globals)

let dump_headers () () = 
  List.iter (fun header -> 
    let s = get_header_deps [header] in
    printf "%s: [\n%s]\n" header 
      (String.concat "" (List.map (sprintf "  %s\n") @@ S.elements s)))
    headers

let () = 
  Log.set_log_level Log.ERROR;
  Log.color_off ();
  Log.set_output stdout

open Ast_helper

let generate_cstdlib headers = 
  let generate_header header builtins = 
    let open Cgen in
    let loc = Location.none in
    let code = sprintf "#include <%s>" header in
    let mod_name = String.capitalize_ascii (Filename.basename (Filename.chop_extension header)) in
    let lid loc v = Location.mkloc (Longident.parse v) loc in

    let module_ n s = Str.module_ (Mb.mk (Location.mkloc n loc) s) in
    let functor_ n a t i = 
      module_ n
       (Mod.functor_ 
          (Location.mkloc a loc) 
          (Some(Mty.ident (lid loc t))) 
          (Mod.structure i))
    in

    let types = 
      let ctx = Cgen.init_ctx loc attrs in
      ccode 
        ~ctx:{ ctx with 
               attrs = { ctx.attrs with
                         Attrs.gentypes = true;
                         gendecls = false; } } ~code 
    in
    let decls =
      let ctx = Cgen.init_ctx loc attrs in
      ccode ~ctx:{ ctx with
                   attrs = { ctx.attrs with
                             Attrs.gendecls = true;
                             gentypes = false;
                             foreignmodule = "F";
                             foreignfnmodule = "F"; } } ~code
    in
    let bindings = functor_ "Bindings" "F" "Cstubs.FOREIGN" decls in

    module_ mod_name (Mod.structure (types @ [bindings]))
  in
  let rec gen = function 
    | [] -> [] 
    | header :: tl -> 
      let modl = generate_header header [] in
      modl :: gen tl
  in
  Pprintast.structure Format.std_formatter (gen headers)
      
let () = generate_cstdlib headers
    

