open Batteries

let get_banks () : int array =
  File.lines_of "data/day6.txt"
  |> Enum.get_exn |> String.split_on_char '\t' |> List.map int_of_string
  |> Array.of_list

let move_blocks (banks : int array) =
  (* get bank w most blocks *)
  let max, max_i =
    Array.fold_lefti
      (fun (curr_max, max_i) i bank ->
        if bank > curr_max then (bank, i) else (curr_max, max_i))
      (0, 0) banks
  in
  (* remove and redistribute *)
  banks.(max_i) <- 0;
  for i = 1 to max do
    let idx = (max_i + i) mod Array.length banks in
    banks.(idx) <- banks.(idx) + 1
  done

let hash_array (arr : int array) : int =
  Array.fold_left (fun hash x -> (hash * 31) + x) 0 arr

let part1 () =
  let rec loop steps seen banks =
    let h = hash_array banks in
    match Hashtbl.find_opt seen h with
    | Some () -> steps
    | None ->
        Hashtbl.add seen h ();
        move_blocks banks;
        loop (steps + 1) seen banks
  in
  Results.Int' (loop 0 (Hashtbl.create 5000) (get_banks ()))

let part2 () =
  let rec loop steps seen banks =
    let h = hash_array banks in
    match Hashtbl.find_opt seen h with
    | Some n -> steps - n
    | None ->
        Hashtbl.add seen h steps;
        move_blocks banks;
        loop (steps + 1) seen banks
  in
  Results.Int' (loop 0 (Hashtbl.create 5000) (get_banks ()))
