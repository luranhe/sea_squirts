open Core ;;
open Stack ;;
open Paramsets ;;

let coords (st : (float * float) t) : string =
  let string_of_pair (sl : string list) (a, b : float * float) : string list =
    Printf.sprintf "{%s,%s}" (Float.to_string a) (Float.to_string b)
      |> Fn.flip List.cons sl in
  fold st ~init:[] ~f:string_of_pair |> String.concat ~sep:",\r\n" ;;

let make
(p : (module PARAMS))
(front, back : (float * float) t * (float * float) t)
: string =
  let module P = (val p : PARAMS) in
  let open P in
  let open Out_channel in
  write_all (folder ^ "wavefronts.txt") ~data:("{" ^ coords front ^ "}");
  write_all (folder ^ "wavebacks.txt") ~data:("{" ^ coords back ^ "}");
  write_lines (folder ^ "parameters.txt") @@ infodump p;
  file ^ filetype ;;
