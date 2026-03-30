open Batteries

let programs =
  File.lines_of "data/day7.txt"
  |> Enum.map (fun ln ->
      let info, ontop =
        try
          String.split ln ~by:" -> "
          |> Tuple2.map2 (String.split_on_string ~by:", ")
        with Not_found -> (ln, [])
      in

      let name, weight = Tuple2.map2 String.chop (String.split ~by:" " info) in
      (name, (int_of_string weight, ontop)))
  |> Hashtbl.of_enum

let part1 () =
  let referenced =
    programs |> Hashtbl.to_seq_values
    |> Seq.flat_map (fun (_, ontop) -> Seq.of_list ontop)
    |> Set.of_seq
  in
  let program_set = Set.of_seq (Hashtbl.to_seq_keys programs) in
  let d = Set.diff program_set referenced in
  Results.Str' (Set.choose d)

let weight_cache = Hashtbl.create 1200

let rec weight name =
  match Hashtbl.find_opt weight_cache name with
  | Some w -> w
  | None ->
      let w0, ontop = Hashtbl.find programs name in
      let w = w0 + List.sum (List.map weight ontop) in
      Hashtbl.add weight_cache name w;
      w

let part2 () =
  let current =
    ref (match part1 () with Str' s -> s | Int' _ -> failwith "unreachable")
  in
  let found = ref false in
  let last_min, last_max = (ref 0, ref 0) in
  while not !found do
    let _, ontop = Hashtbl.find programs !current in

    let weights = List.map weight ontop in
    let curr_min = List.min weights in
    let max_i, curr_max =
      List.fold_lefti
        (fun (mi, mx) idx x -> if x > mx then (idx, x) else (mi, mx))
        (0, 0) weights
    in

    if curr_min = curr_max then found := true
    else (
      current := List.at ontop max_i;
      last_min := curr_min;
      last_max := curr_max)
  done;

  let w, _ = Hashtbl.find programs !current in
  Results.Int' (w + !last_min - !last_max)
