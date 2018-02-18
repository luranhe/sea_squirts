open Lacaml.D ;;
open Multicellparams ;;

let make (dom : float array) (sol : vec array) : int =
  let open Parmap in
  let open Plplot in
  let open BatPrintf in
  let open BatSys in
  let open BatArray in
  let timesteps : int = length sol in
  let log10 (n : int) : int =
    let rec log_aux (n : int) (acc : int) =
      if n = 0 then acc else log_aux (n / 10) (acc + 1) in
    log_aux n 0 in
  let framedigits : int = log10 timesteps in
  let export_png (n : int) (v : vec) : unit =
    let instance : float array = Vec.to_array v in
    plsdev "pngqt"; plsfnam @@ sprintf "%s%0*d.png" file framedigits n;
    plinit (); plenv 0. xmax 0. 1. 0 0;
    left instance @@ length instance / 2 |> plline dom; plend () in
  begin
    match is_directory folder with
    | true ->
      command @@ sprintf "rm -r %s*.png ; rm -r %s%s" folder file movtype
    | false -> remove folder; command @@ sprintf "mkdir %s" folder
    | exception (Sys_error _) -> command @@ sprintf "mkdir %s" folder
  end |> ignore;
  pariteri ~ncores:4 ~chunksize:1 export_png (A sol);
  command @@ sprintf "ffmpeg -r 24 -pattern_type glob -i '%s*.png' %s%s"
    file file movtype ;;
