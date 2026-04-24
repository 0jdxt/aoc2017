open Batteries
open Results

let digits : int array =
  File.lines_of "data/day1.txt"
  |> Enum.get_exn |> String.to_seq
  |> Seq.map (fun c -> int_of_char c - int_of_char '0')
  |> Array.of_seq

let len = Array.length digits

let part1 () =
  Int'
    (Array.sum
       (Array.mapi
          (fun i x ->
            let j = if i = len - 1 then 0 else i + 1 in
            if x = digits.(j) then x else 0)
          digits))

let part2 () =
  Int'
    (Array.sum
       (Array.mapi
          (fun i x -> if x = digits.((i + (len / 2)) mod len) then x else 0)
          digits))
