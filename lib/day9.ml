open Batteries

let stream = File.lines_of "data/day9.txt" |> Enum.get_exn

type state = { depth : int; in_garbage : bool; skip : bool; sum : int }

let part1 () =
  Results.Int'
    (String.fold_left
       (fun st ch ->
         if st.skip then { st with skip = false }
         else if st.in_garbage then
           match ch with
           | '!' -> { st with skip = true }
           | '>' -> { st with in_garbage = false }
           | _ -> st
         else
           match ch with
           | '{' -> { st with depth = st.depth + 1 }
           | '}' -> { st with sum = st.sum + st.depth; depth = st.depth - 1 }
           | '<' -> { st with in_garbage = true }
           | _ -> st)
       { depth = 0; in_garbage = false; skip = false; sum = 0 }
       stream)
      .sum

let part2 () =
  Results.Int'
    (String.fold_left
       (fun st ch ->
         if st.skip then { st with skip = false }
         else if st.in_garbage then
           match ch with
           | '!' -> { st with skip = true }
           | '>' -> { st with in_garbage = false }
           | _ -> { st with sum = st.sum + 1 }
         else if ch = '<' then { st with in_garbage = true }
         else st)
       { depth = 0; in_garbage = false; skip = false; sum = 0 }
       stream)
      .sum
