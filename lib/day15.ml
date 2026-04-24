let a_start = 883
let a_factor = 16807
let b_start = 879
let b_factor = 48271
let low16 x = x land 0xFFFF

(* fast mersenne prime modulo *)
let mode (x : int) : int =
  (* 0x7FFFFFFF *)
  let r = 2147483647 in
  let a = (x land r) + (x lsr 31) in
  if a >= r then a - r else a

let part1 () =
  let count = ref 0 in
  let a = ref a_start in
  let b = ref b_start in
  for _ = 0 to 40000000 do
    a := mode (!a * a_factor);
    b := mode (!b * b_factor);
    if low16 !a = low16 !b then incr count
  done;
  Results.Int' !count

let part2 () =
  let a_gen prev =
    let x = ref (mode (!prev * a_factor)) in
    while !x land 3 <> 0 do
      x := mode (!x * a_factor)
    done;
    prev := !x;
    !x
  in

  let b_gen prev =
    let x = ref (mode (!prev * b_factor)) in
    while !x land 7 <> 0 do
      x := mode (!x * b_factor)
    done;
    prev := !x;
    !x
  in

  let count = ref 0 in
  let a = ref a_start in
  let b = ref b_start in
  for _ = 0 to 5_000_000 do
    if low16 (a_gen a) = low16 (b_gen b) then incr count
  done;
  Results.Int' !count
