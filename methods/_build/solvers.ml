open Batteries ;;
open BatArray ;;
open Parmap ;;

let numpoints : int = 500 ;;

let euler
(bigF : float array -> float -> float array -> unit)
(_bigF' : float array -> float -> float array array -> unit)
(init : float array)
(dt : float)
(tmax : float)
  : float array * float array array =
  let tvals : float array =
    (0., dt) --. tmax |> of_enum in
  let nmax : int = length tvals in
  if nmax < numpoints then raise (Failure "dt too large for plot") else
    let xvals : float array array =
      (make @@ nmax) @@ create_float @@ 0 in
    let xcounter : int ref = ref 1 in
    let currdiv : float array = copy init in
    let shared_buf : buf = init_shared_buffer init in
    let rec loop (n : int) : unit =
      let t : float = float_of_int n *. dt in
      if n < nmax then
        (bigF init t currdiv;
        array_float_parmapi ~result:init ~sharedbuffer:shared_buf
          (fun (i : int) (x : float) -> init.(i) +. dt *. x) currdiv |> ignore;
        if t >= tvals.(!xcounter)
          then (xvals.(!xcounter) <- copy init; xcounter := succ !xcounter);
        loop (succ n)) in
    xvals.(0) <- copy init; loop 1; xvals.(pred numpoints) <- init;
    tvals, xvals ;;
