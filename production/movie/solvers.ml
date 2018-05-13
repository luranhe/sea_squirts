(* Remember: Matrices are indexed row, column! *)

open Lacaml.D ;;
open Batteries ;;

(* Number of video frames, I should have a better way of controlling this *)
let numpoints : int =
  let open Multicellparams in
  int_of_float tt / 20 ;;

let rand_store : float option ref = ref None ;;

(* Utility for generating random variables with a normal distribution *)
let rand_normal (r : float option ref) : float =
  let normal () : float * float =
    let open Core.Float in
    let rec gen_pair () : float * float * float =
      let (u, v) : float * float =
        let open Core.Random in
        1. - (float 2.), 1. - (float 2.) in
      let s : float = u ** 2. + v ** 2. in
      if s >=. 1. || s =. 0. then gen_pair () else u, v, s in
    let (u, v, s) : float * float * float = gen_pair () in
    let s_aux : float =
      let open Multicellparams in
      sqrt @@ -2. / s * log s |> ( * ) std in
    u * s_aux, v * s_aux in
  match !r with
  | Some z -> r := None; z
  | None ->
    let (z0, z1) : float * float = normal () in r := Some z0; z1 ;;

(* Where the next left/right kicks will be *)
let (lkick, rkick) : float ref * float ref = ref 0., ref 10000. ;;

let euler
(bigF : vec -> float -> vec -> unit)
(init : vec)
(dt : float)
(tmax : float)
  : vec array =
  let nmax : int = tmax /. dt |> int_of_float in
  if nmax < numpoints then raise (Failure "dt too large for plot") else
    let tvals : float array =
      Vec.linspace 0. tmax @@ succ numpoints |> Vec.to_array in
    let xvals : vec array =
      Vec.create 0 |> BatArray.make @@ succ numpoints in
    let xcounter : int ref = ref 1 in
    (* This will be the derivative vector *)
    let iF : vec = Vec.create @@ Vec.dim init in
    let gridpoints : int = Vec.dim init / 2 in
    let kickpoints : int = gridpoints / 60 in
    let gridoff : int = gridpoints - kickpoints in
    let open Core.Float in
    xvals.(0) <- copy init;
    (* Main loop *)
    for n = 1 to nmax do
      let t : float =
        float_of_int n * dt in
      bigF init t iF;
      axpy ~alpha:dt iF init;
      let open Multicellparams in
      (* Update the future times at which left and right kicks happen *)
      if t >=. !lkick then
        begin
          Vec.fill ~n:kickpoints init kickvalue;
          lkick := !lkick + bcl + rand_normal rand_store
        end;
      if t >=. !rkick then
        begin
          Vec.fill ~n:kickpoints ~ofsx:gridoff init kickvalue;
          rkick := !rkick + bcl + rand_normal rand_store
        end;
      if t >=. tvals.(!xcounter) then
        begin
          xvals.(!xcounter) <- copy init; xcounter := succ !xcounter
        end
    done;
    xvals.(numpoints) <- init; xvals ;;
