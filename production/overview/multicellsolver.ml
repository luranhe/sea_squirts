open Lacaml.D ;;
open Batteries ;;
open Array ;;
open Paramsets ;;

module type FUNCTIONS =
  sig
    val dom : float array
    val x0 : vec
    val bigF : vec -> float -> vec -> unit
  end ;;

module MultiCell(P : PARAMS) : FUNCTIONS =
  struct
    include P

    let tininv : float = 1. /. tin
    let negtoutinv : float = -1. /. tout
    let negtclose : float = -.tclose
    let rt : float = k /. dx ** 2.

    let dom : float array = (0., dx) --. xmax |> of_enum
    let gridpoints : int = length dom

    let x0 : vec =
      let initf (n : int) : float = if n <= gridpoints then 0. else 1. in
      Vec.init (2 * gridpoints) initf

    let band_storage : mat =
      let fill (i : int) (j : int) : float =
        if i = 2 then -2. else if j = 1 then 0. else 1. in
      let m_aux : mat = Mat.init_rows 2 gridpoints fill in
      Mat.scal rt m_aux; m_aux

    let bigF (x : vec) (_t : float) (x' : vec) : unit =
      (* Main computation, see bottom *)
      Vec.zmxy ~n:gridpoints x' x (Vec.sqr ~n:gridpoints ~y:x' x);
      axpy ~alpha:negtoutinv ~n:gridpoints x (sbmv ~y:(Vec.mul ~n:gridpoints
        ~z:x' x' ~ofsy:(succ gridpoints) x) band_storage ~beta:tininv x);
      let open Bigarray.Array1 in
      let open Core in
      (* Gated activation *)
      for n = succ gridpoints to 2 * gridpoints do
        let (h, v) : float * float =
          unsafe_get x n, unsafe_get x @@ n - gridpoints in
        let open Float in
        unsafe_set x' n @@
          if v <=. vcrit then (1. - h) / topen else h / negtclose
      done;
      (* Fix boundaries *)
      let (x2, xm2) : float * float =
        unsafe_get x 2, unsafe_get x @@ pred gridpoints in
      let (x1', xm1') : float * float =
        unsafe_get x' 1, unsafe_get x' gridpoints in
      let open Float in
      unsafe_set x' 1 @@ x1' + rt * x2;
      unsafe_set x' gridpoints @@ xm1' + rt * xm2
  end ;;

(*
Vec.sqr: v^2
Vec.zmxy: v^2 - v^3
Vec.mul: h * (v^2 - v^3)
sbmv: diffusion term + h * (v^2 - v^3) / tin (except boundaries)
axpy: diffusion term + h * (v^2 - v^3) / tin - v / tout
*)
