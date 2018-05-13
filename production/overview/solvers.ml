open Lacaml.D ;;
open Core ;;
open Paramsets ;;

let euler
(p : (module PARAMS))
(bigF : vec -> float -> vec -> unit)
(xvec : vec)
(dom : float array)
: (float * float) Stack.t * (float * float) Stack.t =
  let module P = (val p : PARAMS) in
  let open P in
  (* Utility for generating random variables with a normal distribution *)
  let rand_normal : unit -> float =
    let rand_store : float option ref = ref None in
    fun () ->
      let normal () : float * float =
        let open Float in
        let rec gen_pair () : float * float * float =
          let (u, v) : float * float =
            let open Random in
            1. - (float 2.), 1. - (float 2.) in
          let s : float = u ** 2. + v ** 2. in
          if s >=. 1. || s =. 0. then gen_pair () else u, v, s in
        let (u, v, s) : float * float * float = gen_pair () in
        let s_aux : float =
          sqrt @@ -2. / s * log s |> ( * ) maxbcl in
        u * s_aux, v * s_aux in
      match !rand_store with
      | Some z -> rand_store := None; z
      | None ->
        let (z0, z1) : float * float = normal () in
        rand_store := Some z0; z1 in
  let hmin : float = 4. *. tin /. tout in
  let (lkick, rkick) : float ref * float ref = ref 0., ref 0. in
  let nmax : int = tt /. dt |> int_of_float in
  let iF : vec = Vec.create @@ Vec.dim xvec in
  let gridpoints : int = Vec.dim xvec / 2 in
  let kickpoints : int = gridpoints / 60 in
  let gridoff : int = gridpoints - kickpoints in
  (* Front and back tracking utilities *)
  let track : bool array = Array.create ~len:(2 * gridpoints |> succ) true in
  let open Stack in
  let (front, back) : (float * float) t * (float * float) t =
    create (), create () in
  let detect_front (t : float) (i : int) (r : float) : unit =
    let cross : bool = Float.(r >=. vcrit) in
    let open Array in
    if cross <> unsafe_get track i then
      begin
        unsafe_set track i cross;
        if cross then push front (unsafe_get dom @@ pred i, t)
      end in
  let detect_back (t : float) (i : int) (r : float) : unit =
    let cross : bool = Float.(r <=. hmin) in
    let open Array in
    if cross <> unsafe_get track i then
      begin
        unsafe_set track i cross;
        if cross then push back (unsafe_get dom @@ pred i - gridpoints, t)
      end in
  (* Main loop *)
  for n = 1 to nmax do
    let t : float =
      let open Float in
      of_int n * dt in
    begin
      let open Float in
      let open Posix_math in
      bigF xvec t iF;
      axpy ~alpha:dt iF xvec;
      (* Update the future times at which left and right kicks happen *)
      if t >=. !lkick then
        begin
          Vec.fill ~n:kickpoints xvec kickvalue;
          lkick := !lkick + maxbcl + (maxbcl - minbcl)
            * cbrt (cos (2. * pi * t / period)) + rand_normal ()
        end;
      if t >=. !rkick then
        begin
          Vec.fill ~n:kickpoints ~ofsx:gridoff xvec kickvalue;
          rkick := !rkick + maxbcl - (maxbcl - minbcl)
            * cbrt (cos (2. * pi * t / period)) + rand_normal ()
        end
    end;
    (* Front and back tracking *)
    if Random.bool () then
      begin
        Vec.iteri (detect_front (t /. 1000.)) ~n:(gridpoints / 2) ~incx:2 xvec;
        Vec.iteri (detect_back (t /. 1000.)) ~ofsx:(succ gridpoints)
          ~incx:2 xvec
      end
  done; front, back ;;
