let tin : float = 0.1 ;;
let tout : float = 2.4 ;;
let topen : float = 130.0 ;;
let tclose : float = 150.0 ;;
let vcrit : float = 0.13 ;;

let dt : float = 0.001 ;;
let tt : float = 138000. ;;
let nmax : int = tt /. dt |> int_of_float ;;

let h0 : float = 1.0 ;;
let v0 : float = 0.5 ;;

let curr : (float * float) ref = ref (h0, v0) ;;

open Array ;;

let t : float = Unix.gettimeofday () ;;

let plot : float array array = make_matrix nmax 2 0. ;;

Unix.gettimeofday () -. t |> string_of_float |> print_endline ;;

let action () : unit =
  let rec hv (n : int) : unit =
    let (h, v) : float * float = !curr in
    let t : float = float_of_int n *. dt in
    curr := h +. dt *.
      (if v < vcrit then (1. -. h) /. topen
      else -.h /. tclose),
    v +. dt *. (h *. (1. -. v) *. v ** 2.
      /. tin -. v /. tout +. (if mod_float t 500. < 1. then 0.5 else 0.));
    if n >= nmax - 1 then () else hv (n + 1) in
  hv 0 ;;

let t : float = Unix.gettimeofday () ;;

action () ;;

Unix.gettimeofday () -. t |> string_of_float |> print_endline ;;

print_endline (string_of_float (fst !curr)) ;;
