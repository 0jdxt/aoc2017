open Batteries

let firewall : (int * int) array =
  File.lines_of "data/day13.txt"
  |> Enum.map (fun ln ->
      Tuple2.mapn int_of_string (String.split ln ~by:": ")
      |> Tuple2.map2 (fun r -> 2 * (r - 1)))
  |> Array.of_enum

let part1 () =
  let caught =
    Array.fold_left
      (fun acc (idx, period) ->
        if idx mod period = 0 then (idx * (1 + (period / 2))) + acc else acc)
      0 firewall
  in
  Results.Int' caught

let part2 () =
  let rec find d =
    if Array.exists (fun (idx, period) -> (d + idx) mod period = 0) firewall
    then find (d + 1)
    else d
  in
  Results.Int' (find 0)
