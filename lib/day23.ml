open Batteries

let register : (char, int) Hashtbl.t =
  String.to_seq "abcdefgh" |> Seq.map (fun c -> (c, 0)) |> Hashtbl.of_seq

type target = Val of int | Reg of char

type instruction =
  | Set of char * target
  | Mul of char * target
  | Sub of char * target
  | Jnz of target * int

let get_target : target -> int = function
  | Val x -> x
  | Reg c -> Hashtbl.find register c

let to_target (s : string) : target =
  match int_of_string_opt s with None -> Reg s.[0] | Some x -> Val x

let ins : instruction array =
  File.lines_of "data/day23.txt"
  |> Enum.map (fun ln ->
      match String.split_on_char ' ' ln with
      | [ "set"; x; y ] -> Set (x.[0], to_target y)
      | [ "sub"; x; y ] -> Sub (x.[0], to_target y)
      | [ "mul"; x; y ] -> Mul (x.[0], to_target y)
      | [ "jnz"; x; y ] -> Jnz (to_target x, int_of_string y)
      | _ -> failwith "unreachable")
  |> Array.of_enum

let part1 () =
  let i = ref 0 in
  let count = ref 0 in
  while !i >= 0 && !i < Array.length ins do
    match ins.(!i) with
    | Set (x, y) ->
        Hashtbl.replace register x (get_target y);
        incr i
    | Sub (x, y) ->
        let xv = Hashtbl.find register x in
        Hashtbl.replace register x (xv - get_target y);
        incr i
    | Mul (x, y) ->
        let xv = Hashtbl.find register x in
        Hashtbl.replace register x (xv * get_target y);
        incr count;
        incr i
    | Jnz (x, y) -> if get_target x <> 0 then i := !i + y else incr i
  done;
  Results.Int' !count

let part2 () =
  let count = ref 0 in
  let n = ref 105700 in
  while !n <= 122700 do
    let flag = ref false in
    for i = 2 to int_of_float (sqrt (float_of_int !n)) do
      if !n mod i = 0 then flag := true
    done;
    if !flag then incr count;
    n := !n + 17
  done;
  Results.Int' !count
