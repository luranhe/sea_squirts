open Onecellparams ;;

type odemethod =
  (float array -> float -> float array -> unit) ->
  (float array -> float -> float array array -> unit) ->
  float array -> float -> float -> float array ;;

let methods : (string * odemethod) list =
  [("euler", Solvers.euler); ("trapezoid", Solvers.trap)] ;;

let bigF (x : float array) (t : float) (x' : float array) : unit =
  let (h, v) : float * float = x.(0), x.(1) in
  x'.(0) <-
    (if v < vcrit then (1. -. h) /. topen
    else -.h /. tclose);
  x'.(1) <- (h *. (1. -. v) *. v ** 2.
    /. tin -. v /. tout +. (if mod_float t 500. < 1. then 0.5 else 0.)) ;;

let bigF' (x : float array) (t : float) (dFx : float array array) : unit =
  let (h, v) : float * float = x.(0), x.(1) in
  dFx.(0).(0) <- (1. -. v) *. v ** 2. /. tin;
  dFx.(0).(1) <- -1. /. (if v < vcrit then topen else tclose);
  dFx.(1).(0) <- h *. (2. -. 3. *. v) *. v /. tin -. 1. /. tout;
  dFx.(1).(1) <- 0. ;;

let sol : float array =
  List.assoc Sys.argv.(1) methods bigF bigF' [|h0; v0|] dt tt ;;

sol.(0) |> string_of_float |> print_endline ;;
