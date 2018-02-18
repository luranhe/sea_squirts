open Multicellparams ;;
open Multicellsolver ;;

Core.Random.self_init () ;;

let (ts, sol) : float array * Lacaml.D.vec array =
  let open BatUnix in
  let open BatPrintf in
  let t : float = gettimeofday () in
  let p : float array * Lacaml.D.vec array =
    let open Solvers in
    euler bigF x0 dt tt in
  printf "elapsed time: %f s" (gettimeofday () -. t); print_newline (); p ;;

Movie.make dom sol ;;
