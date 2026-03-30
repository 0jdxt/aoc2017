open Batteries

let steps = 366

let part1 () =
  let arr = DynArray.singleton 0 in
  let i = ref 0 in
  for n = 1 to 2017 do
    i := (!i + steps) mod DynArray.length arr;
    DynArray.insert arr (!i + 1) n;
    incr i
  done;
  Results.Int' (DynArray.get arr (!i + 1))

let part2 () =
  let rec loop i n at_1 =
    if n = 50_000_000 then at_1
    else
      let pos = i + steps in
      let i = if pos >= n then pos - n else pos in
      let at_1 = if i = 0 then n else at_1 in
      loop (i + 1) (n + 1) at_1
  in
  Results.Int' (loop 0 1 0)
