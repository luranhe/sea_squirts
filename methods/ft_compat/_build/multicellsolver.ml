open Batteries ;;
open BatArray ;;
open Lacaml.D ;;
open Plplot ;;
open BatPrintf ;;
open Parmap ;;
open BatSys ;;
open BatUnix ;;

open Multicellparams ;;

let dom : float array = (0., dx) --. xmax |> of_enum ;;
let gridpoints : int = length dom ;;

let x0 : vec =
  let initf (n : int) : float =
    match dom.(pred n) with
    | xval -> if xval <= 0.05 then 0.8 else 0.
    | exception (Invalid_argument _) -> 1. in
  Vec.init (2 * gridpoints) initf ;;

let band_storage : mat =
  let fill (i : int) (j : int) : float =
  if i = 2 then -2.
  else if (i = 1 && j = 1) || (i = 3 && j = gridpoints) then 0.
  else if (i = 1 && j = 2) || (i = 3 && j = pred gridpoints) then 2.
  else 1. in
  let m_aux : mat = Mat.init_rows 3 gridpoints fill in
  Mat.scal (k /. dx ** 2.) m_aux; m_aux ;;

let tininv : float = 1. /. tin ;;
let toutinv : float = -1. /. tout ;;
let negtclose : float = -.tclose ;;

let bigF (x : vec) (_t : float) (x' : vec) : unit =
  let open BatBigarray.Array1 in
  for n = succ gridpoints to 2 * gridpoints do
    let (h, v) : float * float =
      unsafe_get x n, unsafe_get x @@ n - gridpoints in
    let open Core.Float in
    unsafe_set x' n (if v <=. vcrit then (1. - h) / topen else h / negtclose)
  done;
  Vec.zmxy ~n:gridpoints x' x (Vec.sqr ~n:gridpoints ~y:x' x);
  axpy ~alpha:toutinv ~n:gridpoints x (gbmv ~y:(Vec.mul ~n:gridpoints ~z:x' x'
    ~ofsy:(succ gridpoints) x) band_storage ~beta:tininv 1 1 x) ;;

(*
Vec.sqr: v^2
Vec.zmxy: v^2 - v^3
Vec.mul: h * (v^2 - v^3)
gbmv: diffusion term + h * (v^2 - v^3) / tin
axpy: diffusion term + h * (v^2 - v^3) / tin - v / tout
*)

let bigF' (_x : vec) (_t : float) (_dFx : mat) : unit =
  failwith "not implemented" ;;

command @@ "rm -r " ^ folder ^ "*.png ; rm -r " ^ file ^ ".gif" ;;

let t : float = gettimeofday () ;;

let (ts, sol) : float array * vec array =
  let t : float = gettimeofday () in
  let p : float array * vec array =
    BatList.assoc argv.(1) Solvers.methods bigF bigF' None x0 dt tt in
  printf "elapsed time: %f s" (gettimeofday () -. t); print_newline (); p ;;

let export_png (n : int) (v : vec) : unit =
  plsdev "pngqt";
  plsfnam @@ file ^ sprintf "%04d" n ^ ".png";
  plinit ();
  plenv 0. xmax 0. 1. 0 0;
  plline dom @@ flip left gridpoints @@ Vec.to_array v;
  plend () ;;

pariteri ~ncores:4 ~chunksize:1 export_png (A sol) ;;

command @@
  "convert -delay 5 -loop 0 " ^ folder ^ "/*.png " ^ file ^ ".gif" ;;
