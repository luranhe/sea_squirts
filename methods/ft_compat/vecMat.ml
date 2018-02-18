open Lacaml.D ;;

let arr2vec (a : float array) (v : vec) : unit =
  Array.iteri (fun (i : int) (x : float) -> v.{i + 1} <- x) a ;;

let vec2arr (v : vec) (a : float array) : unit =
  Vec.iteri (fun (i : int) (x : float) -> a.(i - 1) <- x) v ;;

let arr2mat (a : float array array) (m : mat) : unit =
  if Array.length a > 0 then
    Array.iteri (fun (i : int) (col : float array) ->
      Array.iteri (fun (j : int) (x : float) -> m.{i + 1, j + 1} <- x) col) a ;;

let mat2arr (m : mat) (a : float array array) : unit =
  if Array.length a > 0 then
    Array.iteri (fun (i : int) (col : float array) ->
      Array.iteri (fun (j : int) (_ : float) -> a.(i).(j) <- m.{i + 1, j + 1})
    col) a ;;
