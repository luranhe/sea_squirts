open Parmap ;;

let factors (n : int) : int list =
  let rec aux (d : int) (n : int) (acc : int list) : int list =
    if n = 1 then acc
    else if n mod d = 0 then aux d (n / d) (d::acc) else aux (d+1) n acc
  in aux 2 n [] |> List.rev ;;

let factors_big (n : int) : int list =
  let m : float = float_of_int n in
  (10. ** m -. 1.) /. 9. |> int_of_float |> factors ;;

let numlist (n : int) : int list =
  let rec aux (n : int) (acc : int list) =
    if n <= 0 then acc else aux (n - 1) (n::acc) in
  aux n [] |> List.rev ;;

let faclist : int list list =
  parmap ~ncores:4 factors_big (L (numlist 20)) ;;

print_endline "Factored!" ;;
