open Ctypes
open Coc_runtime
open Ncurses

module Fns = Bindings(Ncurses_stubs)
open Fns

let ul = Unsigned.ULong.of_int

let () =
  let main_window = initscr () in
  ignore @@ cbreak ();
  let small_window = newwin 10 10 5 5 in
  ignore @@ mvwaddstr main_window 1 2 "Hello";
  ignore @@ mvwaddstr small_window 2 2 "World";
  ignore @@ box small_window (ul 0) (ul 0);
  ignore @@ refresh ();
  Unix.sleep 1;
  ignore @@ wrefresh small_window;
  Unix.sleep 5;
  ignore @@ endwin()
