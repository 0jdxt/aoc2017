open Batteries

let one_round pos skip arr lengths =
  let len = Array.length arr in
  List.iter
    (fun length ->
      if length <= len then (
        for i = 0 to (length / 2) - 1 do
          let a = (!pos + i) mod len in
          let b = (!pos + length - 1 - i) mod len in
          let tmp = arr.(a) in
          arr.(a) <- arr.(b);
          arr.(b) <- tmp
        done;
        pos := (!pos + length + !skip) mod len;
        incr skip))
    lengths

let part1 () =
  let lengths =
    File.lines_of "data/day10.txt"
    |> Enum.get_exn |> String.split_on_char ',' |> List.map int_of_string
  in
  let nums = Array.init 256 Fun.id in
  one_round (ref 0) (ref 0) nums lengths;
  Results.Int' (nums.(0) * nums.(1))

let knot_hash s =
  let bytes =
    String.to_seq s |> Seq.map int_of_char |> List.of_seq |> fun l ->
    l @ [ 17; 31; 73; 47; 23 ]
  in
  let nums = Array.init 256 Fun.id in

  let pos = ref 0 in
  let skip = ref 0 in
  for _ = 1 to 64 do
    one_round pos skip nums bytes
  done;

  List.init 16 (fun i ->
      let x = ref 0 in
      let s = i * 16 in
      for j = s to s + 15 do
        x := !x lxor nums.(j)
      done;
      Printf.sprintf "%02x" !x)
  |> String.join ""

let part2 () =
  Results.Str' (File.lines_of "data/day10.txt" |> Enum.get_exn |> knot_hash)
