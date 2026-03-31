open Batteries

type comp = { lo : int; hi : int }

let comps =
  File.lines_of "data/day24.txt"
  |> Enum.map (fun ln ->
      let x, y = String.split ~by:"/" ln |> Tuple2.mapn int_of_string in
      if x > y then { lo = y; hi = x } else { lo = x; hi = y })
  |> Array.of_enum

let port_map =
  let a = Array.make 51 [] in
  Array.iteri
    (fun i c ->
      a.(c.lo) <- i :: a.(c.lo);
      a.(c.hi) <- i :: a.(c.hi))
    comps;
  a

let rec dfs mask port =
  let s = ref 0 in
  let l = ref (0, 0) in
  List.iter
    (fun i ->
      let bit = 1 lsl i in
      if mask land bit = 0 then (
        let comp = comps.(i) in
        let next_port = if comp.lo = port then comp.hi else comp.lo in
        let next_s, (next_l, next_ls) = dfs (mask lor bit) next_port in
        s := max !s (comp.hi + comp.lo + next_s);
        l := max !l (next_l + 1, comp.lo + comp.hi + next_ls)))
    port_map.(port);
  (!s, !l)

type answer = { mutable fst : int option; mutable snd : int option }

let ans = { fst = None; snd = None }

let part1 () =
  let a, (_, b) =
    port_map.(0)
    |> List.fold
         (fun (acc_s, acc_l) i ->
           let bit = 1 lsl i in
           let comp = comps.(i) in
           let s, (l, ls) = dfs bit comp.hi in
           (max acc_s (comp.hi + s), max acc_l (l + 1, comp.hi + ls)))
         (0, (0, 0))
  in
  ans.fst <- Some a;
  ans.snd <- Some b;
  Results.Int' a

let rec part2 () =
  match ans.snd with
  | Some x -> Results.Int' x
  | None ->
      let _ = part1 () in
      part2 ()
