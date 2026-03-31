open Aoc2017
open Results

let time f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  (res, stop -. start)

let run_part label f =
  let res, t = time f in
  let r = match res with Int' i -> string_of_int i | Str' s -> s in
  Printf.printf "%s %-20s (%.6fs)\n" label r t;
  t

let run_day day_num (d : Registry.day) =
  Printf.printf "%02d:" day_num;
  let t1 = run_part "╶┰" d.part1 in
  let t2 = run_part "    ┖" d.part2 in
  t1 +. t2

let find_day day_num =
  match Registry.get day_num with
  | None ->
      Printf.printf "Day %d not found\n" day_num;
      0.0
  | Some d -> run_day day_num d

let run_all () =
  let total =
    Array.mapi (fun i part_fn -> run_day (i + 1) part_fn) (Registry.all_days ())
    |> BatArray.fsum
  in
  Printf.printf "\nTotal time: %.6fs\n" total

let run_day_part day part =
  match Registry.get day with
  | None -> Printf.printf "Day %d not found\n" day
  | Some d ->
      let f = if part = 1 then d.part1 else d.part2 in
      ignore (run_part "Result" f)

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> run_all ()
  | [ _; day ] -> ignore (find_day (int_of_string day))
  | [ _; day; part ] -> run_day_part (int_of_string day) (int_of_string part)
  | _ ->
      Printf.printf "Usage:\n";
      Printf.printf "  ./aoc\n";
      Printf.printf "  ./aoc <day>\n";
      Printf.printf "  ./aoc <day> <part>\n"
