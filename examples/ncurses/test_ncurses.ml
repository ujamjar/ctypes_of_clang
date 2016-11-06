open Ctypes
open Coc_runtime
open Ncurses

module Fns = Bindings(Ncurses_stubs)
open Fns

let ul = Unsigned.ULong.of_int

let () =
  let main_window = initscr () in
  cbreak ();
  let small_window = newwin 10 10 5 5 in
  mvwaddstr main_window 1 2 (coerce string (ptr char) "Hello");
  mvwaddstr small_window 2 2 (coerce string (ptr char) "World");
  box small_window (ul 0) (ul 0);
  refresh ();
  Unix.sleep 1;
  wrefresh small_window;
  Unix.sleep 5;
  ignore @@ endwin()
