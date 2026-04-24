open Batteries

type particle = {
  mutable pos : int * int * int;
  mutable vel : int * int * int;
  acc : int * int * int;
}

let man (p : particle) =
  let manhatten (x, y, z) = Int.abs x + Int.abs y + Int.abs z in
  (manhatten p.acc, manhatten p.vel, manhatten p.pos)

let add3 ((x0, y0, z0) : int * int * int) ((x1, y1, z1) : int * int * int) :
    int * int * int =
  (x0 + x1, y0 + y1, z0 + z1)

let tick (p : particle) =
  p.vel <- add3 p.vel p.acc;
  p.pos <- add3 p.pos p.vel

let particles : particle array =
  File.lines_of "data/day20.txt"
  |> Enum.map (fun ln ->
      Scanf.sscanf ln "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>"
        (fun x y z vx vy vz ax ay az ->
          { pos = (x, y, z); vel = (vx, vy, vz); acc = (ax, ay, az) }))
  |> Array.of_enum

let num_parts : int = Array.length particles

let part1 () =
  let i, _ =
    Array.fold_lefti
      (fun (mini, minman) i p ->
        let manp = man p in
        if manp < minman then (i, manp) else (mini, minman))
      (0, (Int.max_num, Int.max_num, Int.max_num))
      particles
  in
  Results.Int' i

let part2 () =
  let len = ref num_parts in
  let j = ref 0 in
  while !len = num_parts || !j <> !len do
    let counts = Hashtbl.create num_parts in
    if !j > 0 then len := !j;
    Array.iter
      (fun p ->
        tick p;
        Hashtbl.modify_def 0 p.pos (( + ) 1) counts)
      particles;

    (* swap remove with final j as length *)
    j := 0;
    for i = 0 to !len - 1 do
      if Hashtbl.find counts particles.(i).pos = 1 then (
        particles.(!j) <- particles.(i);
        incr j)
    done
  done;

  Results.Int' !len
