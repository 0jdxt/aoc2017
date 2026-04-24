open Batteries

let image = ref [| [| 0; 1; 0 |]; [| 0; 0; 1 |]; [| 1; 1; 1 |] |]
let size = ref 3

let parse_rule rule =
  String.to_seq rule
  |> Seq.filter_map (function '.' -> Some 0 | '#' -> Some 1 | _ -> None)
  |> Array.of_seq

let flip n x =
  let res = ref 0 in
  for y = 0 to n - 1 do
    for x0 = 0 to n - 1 do
      let old_bit = (x lsr ((n * n) - 1 - ((y * n) + x0))) land 1 in
      let new_pos = (y * n) + (n - 1 - x0) in
      res := !res lor (old_bit lsl ((n * n) - 1 - new_pos))
    done
  done;
  !res

let rotate n x =
  let res = ref 0 in
  for y = 0 to n - 1 do
    for x0 = 0 to n - 1 do
      let old_bit = (x lsr ((n * n) - 1 - ((y * n) + x0))) land 1 in
      let new_pos = (x0 * n) + (n - 1 - y) in
      res := !res lor (old_bit lsl ((n * n) - 1 - new_pos))
    done
  done;
  !res

let variants n x =
  let flip_x = flip n x in
  [
    x;
    rotate n x;
    rotate n (rotate n x);
    rotate n (rotate n (rotate n x));
    flip_x;
    rotate n flip_x;
    rotate n (rotate n flip_x);
    rotate n (rotate n (rotate n flip_x));
  ]

let encode_block n arr =
  Array.fold_lefti (fun acc i v -> acc lor (v lsl ((n * n) - 1 - i))) 0 arr

let decode_block n x =
  Array.init (n * n) (fun i -> (x lsr ((n * n) - 1 - i)) land 1)

let rules =
  let t = Hashtbl.create 1000 in
  File.lines_of "data/day21.txt"
  |> Enum.iter (fun ln ->
      let left, right = String.split ~by:" => " ln |> Tuple2.mapn parse_rule in
      let left_int =
        match Array.length left with
        | 4 -> variants 2 (encode_block 2 left)
        | 9 -> variants 3 (encode_block 3 left)
        | _ -> failwith "invalid size"
      in
      let right_int =
        match Array.length right with
        | 4 -> encode_block 2 right
        | 9 -> encode_block 3 right
        | 16 -> encode_block 4 right
        | _ -> failwith "invalid output size"
      in
      List.iter (fun x -> Hashtbl.add t x right_int) left_int);
  t

let get2x2 image x y =
  [|
    image.(y).(x); image.(y).(x + 1); image.(y + 1).(x); image.(y + 1).(x + 1);
  |]

let get3x3 image x y =
  [|
    image.(y).(x);
    image.(y).(x + 1);
    image.(y).(x + 2);
    image.(y + 1).(x);
    image.(y + 1).(x + 1);
    image.(y + 1).(x + 2);
    image.(y + 2).(x);
    image.(y + 2).(x + 1);
    image.(y + 2).(x + 2);
  |]

let rules_old =
  File.lines_of "data/day21.txt"
  |> Enum.map (fun ln -> String.split ~by:" => " ln |> Tuple2.mapn parse_rule)
  |> Hashtbl.of_enum

let flip_arr = function
  | [| a; b; c; d |] -> [| b; a; d; c |]
  | [| a; b; c; d; e; f; g; h; i |] -> [| c; b; a; f; e; d; i; h; g |]
  | _ -> failwith "bad size"

let rotate_arr = function
  | [| a; b; c; d |] -> [| c; a; d; b |]
  | [| a; b; c; d; e; f; g; h; i |] -> [| g; d; a; h; e; b; i; f; c |]
  | _ -> failwith "bad size"

let expand_old arr =
  let rec find count g =
    if count < 4 then
      match Hashtbl.find_opt rules_old g with
      | Some r -> r
      | None -> find (count + 1) (rotate_arr g)
    else find 0 (flip_arr g)
  in
  find 0 arr

let expand x =
  match Hashtbl.find_opt rules x with
  | Some r -> r
  | None ->
      Printf.eprintf "Not found! block = %d\n" x;
      failwith "lookup failed"

let enhance n =
  for _ = 1 to n do
    let psize, getter, nsize =
      match !size mod 2 with 0 -> (2, get2x2, 3) | _ -> (3, get3x3, 4)
    in

    let new_size = !size + (!size / psize) in
    let new_image = Array.make_matrix new_size new_size 0 in

    for i = 0 to (!size / psize) - 1 do
      for j = 0 to (!size / psize) - 1 do
        let piece = getter !image (j * psize) (i * psize) in
        let block = decode_block nsize (expand (encode_block psize piece)) in
        let block_old = expand_old piece in
        if Array.sum block <> Array.sum block_old then begin
          Printf.eprintf "MISMATCH: piece=[%s] encoded=%d new_decoded=[%s] old=[%s]\n"
            (String.concat "," (Array.to_list (Array.map string_of_int piece)))
            (encode_block psize piece)
            (String.concat "," (Array.to_list (Array.map string_of_int block)))
            (String.concat "," (Array.to_list (Array.map string_of_int block_old)))
        end;

        for y = 0 to psize do
          for x = 0 to psize do
            new_image.((i * nsize) + y).((j * nsize) + x) <-
              block.((y * nsize) + x)
          done
        done
      done
    done;

    image := new_image;
    size := new_size;
    Printf.eprintf "after iter: size=%d pixels=%d\n" !size (Array.sum (Array.map Array.sum !image))
  done;
  Array.sum (Array.map Array.sum !image)

let part1 () = Results.Int' (enhance 5)
let part2 () = Results.Int' (enhance 13)
