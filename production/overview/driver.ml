open Core ;;
open Unix ;;
open Printf ;;
open ForkWork ;;
open Multicellsolver ;;
open Paramsets ;;

let prompt (m : string) (f : string -> 'a) : 'a =
  let rec prompter () : 'a =
    printf "%s " m; Out_channel.(flush stdout);
    try f In_channel.(input_line_exn stdin)
    with e -> printf "%s\n" @@ Exn.to_string e; prompter () in
  prompter () ;;

let process (p : (module PARAMS)) : string =
  let module P = (val p : PARAMS) in
  let module M = MultiCell(P) in
  let open M in
  Random.self_init (); mkdir_p P.folder;
  Solvers.euler p bigF x0 dom |> Picture.make p ;;

let cpu_count : int =
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

let numtasks : int = prompt "Number of tasks:" Int.of_string in
let t : float = gettimeofday () in
set_ncores ~detect:true cpu_count;
printf "Processing on %i cores.\n" @@ ncores ();
create_process ~prog:"./plotter.wls"
  ~args:(map_list process @@ make_params numtasks) |> ignore;
printf "Time elapsed: %f (s)\n" @@ gettimeofday () -. t ;;
