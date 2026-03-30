open Batteries

let pipes =
  File.lines_of "data/day12.txt"
  |> Enum.map (fun ln ->
      String.split ln ~by:" <-> "
      |> Tuple2.map int_of_string (fun rest ->
          rest |> String.split_on_string ~by:", " |> List.map int_of_string))
  |> Hashtbl.of_enum

let rec visit_id seen id =
  if seen.(id) = 0 then (
    seen.(id) <- 1;
    List.iter (visit_id seen) (Hashtbl.find pipes id))

let part1 () =
  let seen = Array.init (Hashtbl.length pipes) (Fun.const 0) in
  visit_id seen 0;
  Results.Int' (Array.sum seen)

let part2 () =
  let seen = Array.init (Hashtbl.length pipes) (Fun.const 0) in
  Results.Int'
    (Array.fold_lefti
       (fun count id is_seen ->
         if is_seen = 0 then (
           visit_id seen id;
           count + 1)
         else count)
       0 seen)
