open Lacaml.D ;;
open Plplot ;;
open Spiralparams ;;

type odemethod =
  (vec -> float -> vec -> unit) ->
  (vec -> float -> mat -> unit) ->
  vec -> float -> float -> float array * vec array ;;

let methods : (string * odemethod) list =
  [("euler", Solvers.euler); ("trapezoid", Solvers.trap)] ;;

let bigF (x : vec) (_t : float) (x' : vec) : unit =
  let (h, v) : float * float = x.{1}, x.{2} in
  x'.{1} <- e *. h -. v;
  x'.{2} <- h +. e *. v ;;

let bigF' (x : vec) (_t : float) (dFx : mat) : unit =
  let (_h, _v) : float * float = x.{1}, x.{2} in
  dFx.{1, 1} <- e;
  dFx.{1, 2} <- -1.;
  dFx.{2, 1} <- 1.;
  dFx.{2, 2} <- e ;;

let x0 : vec = Vec.of_list [h0; v0] ;;

let t : float = Unix.gettimeofday () ;;

let (ts, sol) : float array * vec array =
  List.assoc Sys.argv.(1) methods bigF bigF' x0 dt tt ;;

let elapsed : float = Unix.gettimeofday () -. t ;;

let plot_formatted : (float array * float array) list =
  Array.to_list @@ Array.map (fun (a : float array) -> ts, a) @@ Mat.to_array
    @@ Mat.of_col_vecs sol ;;

Quick_plot.lines ~device:(`window `qt) plot_formatted ;;
