(* curr_mat is assumed to share data with curr *)

open Lacaml.D ;;

let findzero
(bigG : vec -> vec)
(bigG' : vec -> mat)
?(tol : float = 1e-14)
(guess : vec)
(curr_mat : mat)
  : unit =
  let rec loop () : unit =
    let curr : vec = bigG guess in
    if (asum curr |> abs_float) /. (asum guess) > tol then
      (gesv (bigG' guess) curr_mat; axpy ~alpha:(-1.) curr guess; loop ()) in
  loop () ;;

let findzero_tridiag
(bigG : vec -> vec)
(tridiag : vec -> (vec * vec * vec))
?(tol : float = 1e-14)
(guess : vec)
(curr_mat : mat)
  : unit =
  let rec loop () : unit =
    let curr : vec = bigG guess in
    let (dl, d, du) : vec * vec * vec = tridiag guess in
    if (asum curr |> abs_float) /. (asum guess) > tol then
      (gtsv dl d du curr_mat; axpy ~alpha:(-1.) curr guess; loop ())
  in loop () ;;
