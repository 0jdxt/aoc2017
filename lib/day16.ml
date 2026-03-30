open Batteries

let programs = ref (Bytes.of_string "abcdefghijklmnop")
let num_prog = Bytes.length !programs
let offset = ref 0

type move = Spin of int | Swap of (char * char) | Swapi of (int * int)

let moves =
  File.lines_of "data/day16.txt" |> Enum.get_exn |> String.split_on_char ','

let swap a b =
  let tmp = Bytes.get !programs a in
  Bytes.set !programs a (Bytes.get !programs b);
  Bytes.set !programs b tmp

let dance move =
  match String.get move 0 with
  | 's' ->
      let size = int_of_string (String.tail move 1) in
      offset := (!offset - size + num_prog) mod num_prog
  | 'x' ->
      let a, b =
        String.tail move 1 |> String.split ~by:"/" |> Tuple2.mapn int_of_string
      in
      let ai = (!offset + a) mod num_prog in
      let bi = (!offset + b) mod num_prog in
      swap ai bi
  | 'p' ->
      let a, b =
        String.tail move 1 |> String.split ~by:"/"
        |> Tuple2.mapn (fun s -> String.get s 0)
      in
      let ai = Bytes.index !programs a in
      let bi = Bytes.index !programs b in
      swap ai bi
  | _ -> failwith "unknown move"

let rotated_string () =
  let out = Bytes.create num_prog in
  for i = 0 to num_prog - 1 do
    Bytes.set out i (Bytes.get !programs ((!offset + i) mod num_prog))
  done;
  Bytes.to_string out

let part1 () =
  List.iter dance moves;
  Results.Str' (rotated_string ())

let part2 () =
  let seen = Hashtbl.create 100 in
  let reverse = Hashtbl.create 100 in
  let s = ref (rotated_string ()) in
  let i = ref 0 in
  while not (Hashtbl.mem seen !s) do
    Hashtbl.add seen !s !i;
    Hashtbl.add reverse !i !s;
    List.iter dance moves;
    s := rotated_string ();
    incr i
  done;

  let cycle_len = !i - Hashtbl.find seen !s in
  Results.Str' (Hashtbl.find reverse (999_999_999 mod cycle_len))
