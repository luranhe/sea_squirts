open Core ;;

module type PARAMS =
  sig
    val tin : float
    val tout : float
    val topen : float
    val tclose : float
    val vcrit : float
    val k : float
    val dt : float
    val tt : float
    val dx : float
    val xmax : float
    val minbcl : float
    val maxbcl : float
    val period : float
    val std : float
    val kickvalue : float
    val folder : string
    val file : string
    val filetype : string
  end ;;

module type FRESHDIR =
  sig
    val topfolder : string
    val folder_int : int
  end ;;

module FromDir(F : FRESHDIR) : PARAMS =
  struct
    include Multicellparams

    let folder : string = F.topfolder ^ Int.to_string F.folder_int ^ "/"
    let file : string = folder ^ filename
  end ;;

let inc (f : (module FRESHDIR)) : (module FRESHDIR) =
  let module F = (val f : FRESHDIR) in
  (module
    struct
      let topfolder : string = F.topfolder
      let folder_int : int = F.folder_int |> succ
    end) ;;

let new_dir : (module FRESHDIR) =
  (module
    struct
      include Multicellparams

      let folder_int : int =
        let to_int (s : string) : int option =
          match Int.of_string s with
          | i -> Some i
          | exception (Failure _) -> None in
        let ifs () : int array =
          Array.filter_map (Sys.readdir topfolder) ~f:to_int in
        match Array.max_elt (ifs ()) ~cmp:Int.compare with
        | None -> 0
        | Some i -> succ i
        | exception (Sys_error _) -> 0
    end) ;;

let dirlist (n : int) : (module FRESHDIR) list =
  let rec dirlist_aux
  (n : int) (f : (module FRESHDIR)) (l : (module FRESHDIR) list)
  : (module FRESHDIR) list =
    if n <= 0 then l else if n = 1 then f::l
    else dirlist_aux (pred n) (inc f) (f::l) in
  dirlist_aux n new_dir [] ;;

let from_dir (f : (module FRESHDIR)) : (module PARAMS) =
  let module F = (val f : FRESHDIR) in
  (module FromDir(F) : PARAMS) ;;

let infodump (p : (module PARAMS)) : string list =
  let module P = (val p : PARAMS) in
  let open P in
  let info : (string * float) list =
    ["tin", tin; "tout", tout; "topen", topen; "tclose", tclose;
      "vcrit", vcrit; "k", k; "dt", dt; "tt", tt; "dx", dx; "xmax", xmax;
      "minbcl", minbcl; "maxbcl", maxbcl; "std", std; "period", period;
      "kickvalue", kickvalue] in
  List.map info
    ~f:(fun (s, r : string * float) -> s ^ ": " ^ Float.to_string r ^ "\r") ;;

let make_params : int -> (module PARAMS) list =
  Fn.compose (List.rev_map ~f:from_dir) dirlist ;;
