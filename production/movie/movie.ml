open Core ;;
open Unix ;;
open Lacaml.D ;;
open Multicellparams ;;

(* New directory *)
let folder_int : int =
  let to_int (s : string) : int option =
    match Int.of_string s with
    | i -> Some i
    | exception (Failure _) -> None in
  let ifs () : int array =
    Array.filter_map (Sys.readdir topfolder) ~f:to_int in
  match Array.max_elt (ifs ()) ~compare:Int.compare with
  | None -> 0
  | Some i -> succ i
  | exception (Sys_error _) -> 0

let folder : string = topfolder ^ Int.to_string folder_int ^ "/" ;;

let file : string = folder ^ filename ;;

(* Automatically detect # of CPUs *)
let cpu_count : int =
  (* Utility for running in the command line and retrieving the result *)
  let syscall (cmd : string) : string =
    let (ic, oc) : In_channel.t * Out_channel.t = open_process cmd in
    let open Buffer in
    let buf : t = create 16 in
    (try while true do add_channel buf ic 1 done with End_of_file -> ());
    close_process (ic, oc) |> ignore;
    String.strip (contents buf) in
  try
    Int.of_string @@
      match Sys.os_type with
      | "Win32" -> Sys.getenv_exn "NUMBER_OF_PROCESSORS"
      | _ ->
        syscall @@
          if syscall "uname -s" = "FreeBSD" then "sysctl -n hw.ncpu"
          else "getconf _NPROCESSORS_ONLN"
  with _ -> 1 ;;

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
  (* Utility for making images *)
  let export_png (n : int) (v : vec) : unit =
    let instance : float array = Vec.to_array v in
    plsdev "pngqt"; plsfnam @@ sprintf "%s%0*d.png" file framedigits n;
    plinit (); plenv 0. xmax 0. 1. 0 0;
    left instance @@ length instance / 2 |> plline dom; plend () in
  (* Make directory *)
  begin
    match is_directory folder with
    | true ->
      command @@ sprintf "rm -r %s*.png ; rm -r %s%s" folder file movtype
    | false -> remove folder; command @@ sprintf "mkdir %s" folder
    | exception (Sys_error _) -> command @@ sprintf "mkdir %s" folder
  end |> ignore;
  (* Make images, by far the most time-consuming step *)
  pariteri ~ncores:cpu_count export_png (A sol);
  (* Make movie *)
  command @@ sprintf "ffmpeg -r 24 -pattern_type glob -i '%s*.png' %s%s"
    file file movtype ;;
