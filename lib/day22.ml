open Batteries

type state = Clean | Weakened | Infected | Flagged

let get_nodes () =
  let lines = File.lines_of "data/day22.txt" |> Array.of_enum in
  let size = Array.length lines in
  let offset = size / 2 in
  let infected = Hashtbl.create 100_000 in
  Array.iteri
    (fun y line ->
      String.iteri
        (fun x c ->
          if c = '#' then
            let px = x - offset in
            let py = offset - y in
            Hashtbl.add infected (px, py) Infected)
        line)
    lines;
  infected

let left (dx, dy) = (-dy, dx)
let right (dx, dy) = (dy, -dx)
let rev (dx, dy) = (-dx, -dy)
let add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

let part1 () =
  let nodes = get_nodes () in
  let pos_x = ref 0 in
  let pos_y = ref 0 in
  let dir = ref (0, 1) in
  let infections = ref 0 in
  for _ = 1 to 10_000 do
    if Hashtbl.mem nodes (!pos_x, !pos_y) then (
      dir := right !dir;
      Hashtbl.remove nodes (!pos_x, !pos_y))
    else (
      dir := left !dir;
      Hashtbl.add nodes (!pos_x, !pos_y) Infected;
      incr infections);

    pos_x := !pos_x + fst !dir;
    pos_y := !pos_y + snd !dir
  done;
  Results.Int' !infections

let part2 () =
  let nodes = get_nodes () in
  let pos = ref (0, 0) in
  let dir = ref (0, 1) in
  let infections = ref 0 in
  for _ = 1 to 10_000_000 do
    let state = Hashtbl.find_default nodes !pos Clean in
    (match state with
    | Clean ->
        dir := left !dir;
        Hashtbl.replace nodes !pos Weakened
    | Weakened ->
        Hashtbl.replace nodes !pos Infected;
        incr infections
    | Infected ->
        dir := right !dir;
        Hashtbl.replace nodes !pos Flagged
    | Flagged ->
        dir := rev !dir;
        Hashtbl.replace nodes !pos Clean);
    pos := add !pos !dir
  done;
  Results.Int' !infections
