open Batteries

let rows =
  File.lines_of "data/day2.txt"
  |> Enum.map (fun ln ->
      ln |> String.trim
      |> String.split_on_string ~by:"\t"
      |> List.map int_of_string |> Array.of_list)
  |> List.of_enum

let part1 () =
  List.fold_left (fun acc row -> acc + Array.max row - Array.min row) 0 rows

let part2 () =
  List.fold_left
    (fun acc row ->
      let len = Array.length row in
      let rec find i j =
        if i >= len then 0
        else if j >= len then find (i + 1) (i + 2)
        else
          let a, b = (row.(i), row.(j)) in
          if a mod b = 0 then a / b
          else if b mod a = 0 then b / a
          else find i (j + 1)
      in
      acc + find 0 1)
    0 rows
