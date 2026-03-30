open Batteries

let path =
  File.lines_of "data/day11.txt" |> Enum.get_exn |> String.split_on_char ','

let distance x y z = max (max (abs !x) (abs !y)) (abs !z)

let move x y z = function
  | "n" ->
      incr y;
      decr z
  | "ne" ->
      incr x;
      decr z
  | "nw" ->
      decr x;
      incr y
  | "s" ->
      decr y;
      incr z
  | "se" ->
      incr x;
      decr y
  | "sw" ->
      decr x;
      incr z
  | _ -> failwith "unreachable: unknown direction"

let part1 () =
  let x, y, z = (ref 0, ref 0, ref 0) in
  List.iter (move x y z) path;
  Results.Int' (distance x y z)

let part2 () =
  let x, y, z = (ref 0, ref 0, ref 0) in
  let furthest =
    List.fold_left
      (fun m dir ->
        move x y z dir;
        max m (distance x y z))
      0 path
  in
  Results.Int' furthest
