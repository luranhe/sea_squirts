open Multicellparams ;;
open Multicellsolver ;;

Core.Random.self_init () ;;

let sol : Lacaml.D.vec array =
  let open BatUnix in
  let open BatPrintf in
  let t : float = gettimeofday () in
  let res : Lacaml.D.vec array =
    let open Solvers in
    euler bigF x0 dt tt in
  printf "elapsed time: %f s" (gettimeofday () -. t); print_newline (); res ;;

Movie.make dom sol ;;
