open Batteries
open Results

let digits =
  File.lines_of "data/day1.txt"
  |> Enum.get_exn |> String.to_seq
  |> Seq.map (fun c -> int_of_char c - int_of_char '0')
  |> Array.of_seq

let part1 () =
  let len = Array.length digits in
  Int'
    (digits
    |> Array.mapi (fun i x ->
        let j = if i = len - 1 then 0 else i + 1 in
        if x = digits.(j) then x else 0)
    |> Array.sum)

let part2 () =
  let len = Array.length digits in
  Int'
    (digits
    |> Array.mapi (fun i x ->
        if x = digits.((i + (len / 2)) mod len) then x else 0)
    |> Array.sum)
