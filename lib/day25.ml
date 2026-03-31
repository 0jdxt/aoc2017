open Batteries

let tape = Array.make 7113 0

type s = A | B | C | D | E | F

let part1 () =
  let state = ref A in
  let pos = ref 7061 in
  for _ = 1 to 12683008 do
    match (!state, tape.(!pos)) with
    | A, 0 ->
        tape.(!pos) <- 1;
        incr pos;
        state := B
    | A, 1 ->
        tape.(!pos) <- 0;
        decr pos;
        state := B
    | B, 0 ->
        tape.(!pos) <- 1;
        decr pos;
        state := C
    | B, 1 ->
        tape.(!pos) <- 0;
        incr pos;
        state := E
    | C, 0 ->
        tape.(!pos) <- 1;
        incr pos;
        state := E
    | C, 1 ->
        tape.(!pos) <- 0;
        decr pos;
        state := D
    | D, x ->
        tape.(!pos) <- 1;
        decr pos;
        state := A
    | E, x ->
        tape.(!pos) <- 0;
        incr pos;
        state := if x = 0 then A else F
    | F, x ->
        tape.(!pos) <- 1;
        incr pos;
        state := if x = 0 then E else A
    | _ -> failwith "unreachable"
  done;
  Results.Int' (Array.sum tape)

let part2 () = Results.Str' "Merry Xmas 2017!"
