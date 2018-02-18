open Lacaml.D ;;

let exmat : mat = Mat.make0 2 2 ;;

exmat.{1, 2} <- 1. ;;

let export : vec array = Mat.to_col_vecs exmat ;;

print_endline (string_of_float export.(0).{2}) ;;
