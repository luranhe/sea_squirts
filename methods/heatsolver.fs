let numpoints = 500 ;;

let bigF x t xd =
  (async {xd.[0] <- 2. * (x.[1] - x.[0])};
  async {xd.[x.Length - 1] <- 2. * (x.[x.Length - 2] - x.[x.Length - 1])};
  for n = 1 to x.Length - 2 do
    async{xd.[n] <- x.[n - 1] + x.[n + 1] - 2. * x.[n]})
  |> Async.Parallel |> Async.Run ;;

let euler bigF init dt tmax =
  let nmax = int(tmax / dt) in
  let tvals = [0. .. dt .. tmax] in

