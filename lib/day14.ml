open Batteries

let key : string = "wenycdww-"
let grid_size : int = 128
let grid : int array = Array.init (grid_size * grid_size) (Fun.const 0)
let coord (x : int) (y : int) : int = (x * grid_size) + y

let part1 () =
  for row = 0 to grid_size - 1 do
    String.cat key (string_of_int row)
    |> Day10.knot_hash
    |> String.iteri (fun i c ->
        let n = int_of_string ("0x" ^ String.make 1 c) in
        for j = 0 to 3 do
          grid.(coord row ((i * 4) + j)) <- (n lsr (3 - j)) land 1
        done)
  done;
  Results.Int' (Array.sum grid)

let part2 () =
  let rec dfs i j =
    if not (i < 0 || i >= grid_size || j < 0 || j >= grid_size) then
      let idx = coord i j in
      if grid.(idx) = 1 then (
        grid.(idx) <- 0;
        dfs (i + 1) j;
        dfs (i - 1) j;
        dfs i (j + 1);
        dfs i (j - 1))
  in
  let count = ref 0 in
  for i = 0 to grid_size - 1 do
    for j = 0 to grid_size - 1 do
      if grid.(coord i j) = 1 then (
        incr count;
        dfs i j)
    done
  done;
  Results.Int' !count
