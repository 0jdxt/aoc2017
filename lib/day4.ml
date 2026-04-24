open Batteries

let passphrases : string array list =
  File.lines_of "data/day4.txt"
  |> Enum.map (fun ln ->
      String.trim ln |> String.split_on_char ' ' |> Array.of_list)
  |> List.of_enum

let is_valid (f : 'a -> 'b) =
  let sum = ref 0 in
  List.iter
    (fun phrase ->
      let dup = ref false in
      let i = ref 0 in
      while !i < Array.length phrase - 1 && not !dup do
        let x = f phrase.(!i) in
        for j = !i + 1 to Array.length phrase - 1 do
          if x = f phrase.(j) then dup := true
        done;
        incr i
      done;
      if not !dup then incr sum)
    passphrases;
  Results.Int' !sum

let part1 () = is_valid Fun.id

let part2 () =
  is_valid (fun word ->
      let counts = Array.make 26 0 in
      String.iter
        (fun c ->
          let i = Char.code c - Char.code 'a' in
          counts.(i) <- counts.(i) + 1)
        word;
      counts)
