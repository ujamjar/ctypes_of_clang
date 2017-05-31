open Ctypes
open Dateapi.Date

module Fns = Bindings(Datestubs.Stubs)
open Fns

let cstr = coerce (ptr char) string

let () = begin
  let timep = allocate_n ~count:1 time_t in
  let time = time timep in
  assert (time = !@timep);
  let tm' = localtime timep in
  Printf.printf "tm.tm_mon  = %d\n" (Signed.SInt.to_int (getf !@tm' tm#members#tm_mon));
  Printf.printf "tm.tm_year = %d\n" (Signed.SInt.to_int (getf !@tm' tm#members#tm_year));
  print_endline @@ cstr (asctime tm');
  print_endline @@ cstr (CArray.get (!@ __tzname) 0);
  print_endline @@ cstr (CArray.get (!@ __tzname) 1);
end


