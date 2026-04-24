open Batteries

type cell = Path of char option | Junc | Empty

let cell_of_char : char -> cell = function
  | ' ' -> Empty
  | '+' -> Junc
  | '|' | '-' -> Path None
  | c -> Path (Some c)

let grid : cell array =
  File.lines_of "data/day19.txt"
  |> List.of_enum |> Seq.of_list
  |> Seq.flat_map (fun s -> String.to_seq s |> Seq.map cell_of_char)
  |> Array.of_seq

let steps = ref 0

let part1 () =
  let size = 201 in
  let i = ref (Array.findi (( <> ) Empty) grid) in
  let di = ref size in

  let buf = Buffer.create 10 in
  try
    while true do
      (match grid.(!i) with
      | Empty -> raise Exit
      | Path None -> ()
      | Path (Some c) -> Buffer.add_char buf c
      | Junc -> (
          di :=
            if Int.abs !di = size then
              match grid.(!i + 1) with Empty -> -1 | _ -> 1
            else match grid.(!i + size) with Empty -> -size | _ -> size));
      i := !i + !di;
      incr steps
    done
  with Exit -> Results.Str' (Buffer.contents buf)

let part2 () = Results.Int' !steps
