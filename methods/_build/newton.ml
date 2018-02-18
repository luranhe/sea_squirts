open Lacaml.D ;;

let findzero
(bigG : vec -> vec)
(bigG' : vec -> mat)
?(tol : float = 1e-14)
(guess : vec)
  : unit =
  let rec loop () =
    let curr : vec = bigG guess in
    let curr_mat : mat = Mat.from_col_vec curr in
    if nrm2 curr |> abs_float > tol then
      (gesv (bigG' guess) curr_mat; axpy ~alpha:(-1.) curr guess; loop ()) in
  loop () ;;
