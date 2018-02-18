open Lacaml.D ;;
open Newton ;;

let curr : vec = Vec.make0 2 ;;
let deriv : mat = Mat.make0 2 2 ;;

let bigG (xvec : vec) : vec =
  let (x, y) : float * float = xvec.{1}, xvec.{2} in
  curr.{1} <- exp (x -. 2.) -. y;
  curr.{2} <- y ** 2. -. x;
  curr ;;

let bigG' (xvec : vec) : mat =
  let (x, y) : float * float = xvec.{1}, xvec.{2} in
  deriv.{1, 1} <- exp (x -. 2.);
  deriv.{1, 2} <- -1.;
  deriv.{2, 1} <- -1.;
  deriv.{2, 2} <- 2. *. y;
  deriv ;;

let guess : vec = [-4.; -10.] |> Vec.of_list ;;

findzero bigG bigG' ~tol:0. guess ;;

Printf.printf "x: %f\ny: %f\n" guess.{1} guess.{2} ;;
