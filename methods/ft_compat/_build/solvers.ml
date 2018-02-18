(* Remember: Matrices are indexed row, column! *)

let numpoints : int = 50 ;;

open Lacaml.D ;;
open Batteries ;;

type tridiag = vec -> vec * vec * vec ;;

type odemethod =
  (vec -> float -> vec -> unit) ->
  (vec -> float -> mat -> unit) ->
  (tridiag option) ->
  vec -> float -> float -> float array * vec array ;;

let kick : int ref = ref 1 ;;

let euler
(bigF : vec -> float -> vec -> unit)
(_bigF' : vec -> float -> mat -> unit)
(_tridiag : tridiag option)
(init : vec)
(dt : float)
(tmax : float)
  : float array * vec array =
  let nmax : int = tmax /. dt |> int_of_float in
  if nmax < numpoints then raise (Failure "dt too large for plot") else
    let tvals : float array =
      Vec.linspace 0. tmax @@ succ numpoints |> Vec.to_array in
    let xvals : vec array =
      Vec.create 0 |> Array.make @@ succ numpoints in
    let xcounter : int ref = ref 1 in
    let iF : vec = Vec.create @@ Vec.dim init in
    let gridpoints : int = Vec.dim init / 2 in
    xvals.(0) <- copy init;
    for n = 1 to nmax do
      let t : float = float_of_int n *. dt in
      bigF init t iF;
      axpy ~alpha:dt iF init;
      if t >= tvals.(!xcounter) then
        begin
          xvals.(!xcounter) <- copy init; xcounter := !xcounter + 1
        end;
      if t >= float_of_int !kick *. 350. then
        begin
          Vec.fill ~n:(gridpoints / 60) init 0.8;
          kick := succ !kick
        end
    done;
    xvals.(numpoints) <- init; tvals, xvals ;;

let trap
(bigF : vec -> float -> vec -> unit)
(bigF' : vec -> float -> mat -> unit)
(tridiag : tridiag option)
(init : vec)
(dt : float)
(tmax : float)
  : float array * vec array =
  let nmax : int = tmax /. dt |> int_of_float in
  if nmax < numpoints then raise (Failure "dt too large for plot") else
    let tvals : float array =
      Vec.linspace 0. tmax (numpoints + 1) |> Vec.to_array in
    let xvals : vec array =
      Array.make (succ numpoints) (Vec.create @@ Vec.dim init) in
    let xcounter : int ref = ref 1 in
    let d : int = Vec.dim init in
    let iF : vec = Vec.create d in
    let guess : vec = copy init in
    let guessF : vec = Vec.create d in
    let guessg : vec = Vec.create d in
    let guessgmat : mat = Mat.from_col_vec guessg in
    let iden : mat = Mat.identity d in
    let onevec : vec = Vec.make d 1. in
    let guessG' : mat = Mat.create d d in
    let rec loop (n : int) : unit =
      let t : float = float_of_int n *. dt in
      let bigG (xvec : vec) : vec =
        bigF xvec t guessF;
        axpy iF guessF;
        axpy ~alpha:(-.dt /. 2.) guessF (Vec.sub ~z:guessg xvec init);
        guessg in
      let bigG' (xvec : vec) : mat =
        bigF' xvec t guessG'; Mat.scal (-.dt /. 2.) guessG';
        Mat.axpy iden guessG'; guessG' in
      let triG' (dl, d, du : vec * vec * vec) : vec * vec * vec =
        BatList.iter (scal (-.dt /. 2.) ~ofsx:1 ~incx:1) [dl; d; du];
        axpy onevec d; dl, d, du in
      if n <= nmax then
        ((match tridiag with
        | None -> Newton.findzero bigG bigG'
        | Some f -> Newton.findzero_tridiag bigG (triG' % f)) guess guessgmat;
        bigF (copy ~y:init guess) t iF;
        if t >= tvals.(!xcounter)
          then (xvals.(!xcounter) <- copy init; xcounter := !xcounter + 1);
        loop (n + 1)) in
    xvals.(0) <- copy init; bigF init 0. iF; loop 1;
    xvals.(numpoints) <- init; tvals, xvals ;;

let methods : (string * odemethod) list =
  [("euler", euler); ("trapezoid", trap)] ;;
