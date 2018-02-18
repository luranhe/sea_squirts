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
  let rows : int = Mat.dim1 m in
  let cols : int = Mat.dim2 m in
  for i = 0 to pred rows do
    for j = 0 to pred cols do
      a.(i).(j) <- m.{i + 1, j + 1}
    done
  done ;;

let b : float array = [|1.; 2.|] ;;
let bm : float array array = [|b; b|] ;;

let bvec : vec = Vec.of_array b ;;
let bmat : mat = Mat.of_col_vecs_list [bvec; bvec] ;;

arr2vec b bvec ;;
vec2arr bvec b ;;
arr2mat bm bmat ;;
mat2arr bmat bm ;;
