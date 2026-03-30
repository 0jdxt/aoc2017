let target = 361527

let part1 () =
  let k =
    let r = int_of_float (ceil (sqrt (float_of_int target))) in
    if r mod 2 = 0 then r + 1 else r
  in
  let side = k - 1 in
  let ring = side / 2 in
  let offset = ((k * k) - target) mod side in
  let dist = abs (offset - ring) in
  Results.Int' (ring + dist)

let part2 () =
  let cells = Hashtbl.create 100 in
  Hashtbl.add cells (0, 0) 1;
  let x = ref 0 and y = ref 0 in
  (* current side length *)
  let len = ref 1 in
  (* current direction *)
  let dir_index = ref 0 in
  (* steps on current side *)
  let steps = ref 0 in
  (* count done sides *)
  let step_count = ref 0 in

  (* R, U, D, L *)
  let directions = [ (1, 0); (0, 1); (-1, 0); (0, -1) ] in

  let rec loop () =
    (* move to next cell *)
    let dx, dy = List.nth directions !dir_index in
    x := !x + dx;
    y := !y + dy;

    (* sum neighbors *)
    let sum = ref 0 in
    for dx = -1 to 1 do
      for dy = -1 to 1 do
        if dx <> 0 || dy <> 0 then (* skip current cell *)
          match Hashtbl.find_opt cells (!x + dx, !y + dy) with
          | Some v -> sum := !sum + v
          | None -> ()
      done
    done;

    Hashtbl.add cells (!x, !y) !sum;
    if !sum > target then !sum
    else (
      incr steps;
      (* if side finished, change direction *)
      if !steps = !len then (
        steps := 0;
        dir_index := (!dir_index + 1) mod 4;
        incr step_count;
        if !step_count mod 2 = 0 then incr len);
      loop ())
  in
  Results.Int' (loop ())
