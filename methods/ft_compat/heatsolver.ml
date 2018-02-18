open Lacaml.D ;;
open Plplot ;;
open Heatparams ;;
open Solvers ;;
open Printf ;;

let gridpoints : int = xmax /. dx |> int_of_float |> succ ;;

let bigFmat : mat =
  let fill (i : int) (j : int) : float =
    (k /. dx ** 2.) *.
      (if j = i then -2.
      else if j = i + 1 || j = i - 1 then
        if i = 1 || i = gridpoints then 2. else 1.
      else 0.) in
  Mat.init_rows gridpoints gridpoints fill ;;

let d : vec = Mat.copy_diag bigFmat ;;

let dl : vec = Mat.copy_diag ~ar:2 bigFmat ;;

let du : vec = Mat.copy_diag ~ac:2 bigFmat ;;

let (d', dl', du') : vec * vec * vec = copy d, copy dl, copy du ;;

let tridiag (_x : vec) : vec * vec * vec =
  copy ~y:dl' dl, copy ~y:d d', copy ~y:du' du ;;

let bigF (x : vec) (_t : float) (x' : vec) : unit =
  gemv ~y:x' bigFmat ~beta:0. x |> ignore ;;

let bigF' (_x : vec) (_t : float) (dFx : mat) : unit =
  if dFx <> bigFmat then Bigarray.Array2.blit bigFmat dFx ;;

let dom : vec = Vec.linspace 0. xmax gridpoints ;;

let phi (n : int) : float = (dom.{n} -. xmax /. 2.) ** 2. |> (~-.) |> exp ;;

let x0 : vec = Vec.init gridpoints phi ;;

let t : float = Unix.gettimeofday () ;;

let (ts, sol) : float array * vec array =
  List.assoc Sys.argv.(1) methods bigF bigF' (Some tridiag) x0 dt tt ;;

printf "elapsed time: %f s \n" (Unix.gettimeofday () -. t) ;;

plsdev "qtwidget";
plinit ();
plenv 0. xmax 0. 1. 0 0;
plline (Vec.to_array dom) (Vec.to_array sol.(Array.length sol - 1));
plend () ;;

Quick_plot.lines ~device:(`window `qt)
  [(Vec.to_array dom, Vec.to_array sol.(Array.length sol - 1))] ;;
