open Plplot ;;
open Batteries ;;
open BatArray ;;
open Unix ;;
open Parmap ;;
open Multicellparams ;;

let dom : float array = (0., dx) --. xmax |> of_enum ;;

let gridpoints : int = length dom ;;

let x0 : float array =
  let initf (n : int) : float =
    match dom.(n) with
    | xval -> if xval <= 0.05 then 0.8 else 0.
    | exception (Invalid_argument _) -> 1. in
  init (2 * gridpoints) initf ;;

let shared_buf : buf = init_shared_buffer x0 ;;

let bigF (x : float array) (_t : float) (x' : float array) : unit =
  let update (n : int) (xval : float) : float =
    if n >= gridpoints then
      if x.(n - gridpoints) <= vcrit
        then (1. -. xval) /. topen
      else -.xval /. tclose
    else
      let difterm : float =
        if n = pred gridpoints then 2. *. x.(pred n) else
          try x.(pred n) +. x.(succ n)
          with Invalid_argument _ -> 2. *. x.(succ n) in
      (-2. *. xval +. difterm) *. k /. dx ** 2.
        +. x.(n + gridpoints) *. (xval ** 2. -. xval ** 3.) /. tin
        -. xval /. tout in
  array_float_parmapi ~result:x' ~sharedbuffer:shared_buf
    update x |> ignore ;;

let bigF' (_x : float array) (_t : float) (_dFx : float array array) : unit =
  failwith "not implemented" ;;

set_default_ncores 1 ;;

Printf.printf "Number of cores: %d" @@ get_default_ncores ();
print_newline () ;;

let t : float = gettimeofday () ;;

let (ts, sol) : float array * float array array =
  Solvers.euler bigF bigF' x0 dt tt ;;

Printf.printf "elapsed time: %f s" (gettimeofday () -. t);
print_newline () ;;

plsdev "qtwidget";
plinit ();
plenv 0. xmax 0. 1. 0 0;
plline dom @@ left sol.(length sol |> pred) gridpoints;
plend () ;;
