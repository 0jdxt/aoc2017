type state = Clean | Weakened | Infected | Flagged

let min_x, max_x = (-182, 180)
let min_y, max_y = (-173, 216)
let width = max_x - min_x + 1
let height = max_y - min_y + 1
let offset_x = -min_x
let offset_y = -min_y
let dx = [| 0; 1; 0; -1 |]
let dy = [| 1; 0; -1; 0 |]

let get_grid () =
  let grid = Array.make_matrix width height Clean in
  let lines = BatFile.lines_of "data/day22.txt" |> BatArray.of_enum in
  let size = Array.length lines in
  let offset = size / 2 in
  Array.iteri
    (fun y line ->
      String.iteri
        (fun x c ->
          if c = '#' then
            let px = x - offset + offset_x in
            let py = offset - y + offset_y in
            grid.(px).(py) <- Infected)
        line)
    lines;
  grid

let part1 () =
  let pos_x = ref offset_x
  and pos_y = ref offset_y
  and dir = ref 0 (* 0=up, 1=right, 2=down, 3=left *)
  and infections = ref 0 in

  let grid = get_grid () in
  for _ = 1 to 10_000 do
    if grid.(!pos_x).(!pos_y) = Infected then (
      dir := (!dir + 1) land 3;
      grid.(!pos_x).(!pos_y) <- Clean)
    else (
      dir := (!dir + 3) land 3;
      grid.(!pos_x).(!pos_y) <- Infected;
      incr infections);

    pos_x := !pos_x + dx.(!dir);
    pos_y := !pos_y + dy.(!dir)
  done;

  Results.Int' !infections

let part2 () =
  let pos_x = ref offset_x in
  let pos_y = ref offset_y in
  let dir = ref 0 in
  let infections = ref 0 in

  let grid = get_grid () in
  for _ = 1 to 10_000_000 do
    let state = grid.(!pos_x).(!pos_y) in
    (match state with
    | Clean ->
        dir := (!dir + 3) land 3;
        grid.(!pos_x).(!pos_y) <- Weakened
    | Weakened ->
        grid.(!pos_x).(!pos_y) <- Infected;
        incr infections
    | Infected ->
        dir := (!dir + 1) land 3;
        grid.(!pos_x).(!pos_y) <- Flagged
    | Flagged ->
        dir := (!dir + 2) land 3;
        grid.(!pos_x).(!pos_y) <- Clean);
    pos_x := !pos_x + dx.(!dir);
    pos_y := !pos_y + dy.(!dir)
  done;

  Results.Int' !infections
