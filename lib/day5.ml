open Batteries

let ins () : int array =
  File.lines_of "data/day5.txt" |> Enum.map int_of_string |> Array.of_enum

let jump new_offset =
  let ins = ins () in
  let len = Array.length ins in
  let jumps = ref 0 in
  let i = ref 0 in
  while !i >= 0 && !i < len do
    let offset = ins.(!i) in
    ins.(!i) <- new_offset offset;
    i := !i + offset;
    incr jumps
  done;
  Results.Int' !jumps

let part1 () = jump Int.succ

let part2 () =
  jump (fun offset -> if offset > 2 then offset - 1 else offset + 1)
