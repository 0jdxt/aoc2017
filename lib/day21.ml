open Batteries

let image = ref [| [| 0; 1; 0 |]; [| 0; 0; 1 |]; [| 1; 1; 1 |] |]
let size = ref 3

let parse_rule rule =
  String.to_seq rule
  |> Seq.filter_map (function '.' -> Some 0 | '#' -> Some 1 | _ -> None)
  |> Array.of_seq

let rules =
  File.lines_of "data/day21.txt"
  |> Enum.map (fun ln -> String.split ~by:" => " ln |> Tuple2.mapn parse_rule)
  |> Hashtbl.of_enum

let flip = function
  | [| a; b; c; d |] -> [| b; a; d; c |]
  | [| a; b; c; d; e; f; g; h; i |] -> [| c; b; a; f; e; d; i; h; g |]
  | _ -> failwith "bad size"

let rotate = function
  | [| a; b; c; d |] -> [| c; a; d; b |]
  | [| a; b; c; d; e; f; g; h; i |] -> [| g; d; a; h; e; b; i; f; c |]
  | _ -> failwith "bad size"

let expand arr =
  let rec find count g =
    if count < 4 then
      match Hashtbl.find_opt rules g with
      | Some r -> r
      | None -> find (count + 1) (rotate g)
    else find 0 (flip g)
  in
  find 0 arr

let get2x2 image x y =
  [| (x, y); (x + 1, y); (x, y + 1); (x + 1, y + 1) |]
  |> Array.map (fun (x', y') -> image.(y').(x'))

let get3x3 image x y =
  [|
    (x, y);
    (x + 1, y);
    (x + 2, y);
    (x, y + 1);
    (x + 1, y + 1);
    (x + 2, y + 1);
    (x, y + 2);
    (x + 1, y + 2);
    (x + 2, y + 2);
  |]
  |> Array.map (fun (x', y') -> image.(y').(x'))

let enhance n =
  for _ = 1 to n do
    let psize, getter, nsize =
      match !size mod 2 with 0 -> (2, get2x2, 3) | _ -> (3, get3x3, 4)
    in

    let new_size = !size + (!size / psize) in
    let new_image = Array.make_matrix new_size new_size 0 in

    for i = 0 to (!size / psize) - 1 do
      for j = 0 to (!size / psize) - 1 do
        let block = expand (getter !image (j * psize) (i * psize)) in

        for y = 0 to psize do
          for x = 0 to psize do
            new_image.((i * nsize) + y).((j * nsize) + x) <-
              block.((y * nsize) + x)
          done
        done
      done
    done;

    image := new_image;
    size := new_size
  done;
  Array.sum (Array.map Array.sum !image)

let part1 () = Results.Int' (enhance 5)
let part2 () = Results.Int' (enhance 13)
