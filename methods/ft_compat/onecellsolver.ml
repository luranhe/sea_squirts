open Lacaml.D ;;
open Plplot ;;
open Onecellparams ;;
open Solvers ;;
open Printf ;;

let bigF (x : vec) (t : float) (x' : vec) : unit =
  let (h, v) : float * float = x.{1}, x.{2} in
  x'.{1} <-
    (if v < vcrit then (1. -. h) /. topen
    else -.h /. tclose);
  x'.{2} <- (h *. (1. -. v) *. v ** 2.
    /. tin -. v /. tout +. (if mod_float t 500. < 1. then 0.4 else 0.)) ;;

let bigF' (x : vec) (_t : float) (dFx : mat) : unit =
  let (h, v) : float * float = x.{1}, x.{2} in
  dFx.{1, 1} <- -1. /. (if v < vcrit then topen else tclose);
  dFx.{1, 2} <- 0.;
  dFx.{2, 1} <- (1. -. v) *. v ** 2. /. tin;
  dFx.{2, 2} <- h *. (2. -. 3. *. v) *. v /. tin -. 1. /. tout ;;

let x0 : vec = Vec.of_list [h0; v0] ;;

let t : float = Unix.gettimeofday () ;;

let (ts, sol) : float array * vec array =
  List.assoc Sys.argv.(1) methods bigF bigF' None x0 dt tt ;;

printf "elapsed time: %f s \n" (Unix.gettimeofday () -. t) ;;

let output : unit =
  let open Printf in
  let ochan : out_channel = open_out file in
  let rec writer (n : int) : unit =
    match ts.(n), sol.(n) with
    | time, xvec -> fprintf ochan "%f, %f\r\n" time xvec.{1}; writer (n + 1)
    | exception (Invalid_argument _) -> close_out ochan in
  writer 0 ;;

let plot_formatted : (float array * float array) list =
  Array.to_list @@ Array.map (fun (a : float array) -> ts, a) @@ Mat.to_array
    @@ Mat.of_col_vecs sol ;;

Quick_plot.lines ~device:(`window `qt) plot_formatted ;;
