open Batteries

type instruction = {
  op : int -> int;
  reg : string;
  comp : int -> bool;
  c_reg : string;
}

let parse_line line =
  match String.split_on_char ' ' line with
  | [ a; b; c; "if"; e; f; g ] ->
      let c = int_of_string c in
      let op =
        match b with
        | "inc" -> fun x -> x + c
        | "dec" -> fun x -> x - c
        | _ -> failwith "unknown operation"
      in
      let g = int_of_string g in
      let comp =
        match f with
        | ">" -> fun x -> x > g
        | "<" -> fun x -> x < g
        | ">=" -> fun x -> x >= g
        | "<=" -> fun x -> x <= g
        | "==" -> fun x -> x = g
        | "!=" -> fun x -> x <> g
        | _ -> failwith "unknown comp"
      in
      { op; reg = a; comp; c_reg = e }
  | _ -> failwith "invalid instruction"

let program =
  File.lines_of "data/day8.txt" |> Enum.map parse_line |> List.of_enum

let part1 () =
  let register = Hashtbl.create 1000 in
  let max =
    program
    |> List.iter (fun i ->
        let cval = Hashtbl.find_default register i.c_reg 0 in
        if i.comp cval then Hashtbl.modify_def 0 i.reg i.op register);
    Hashtbl.to_seq_values register |> Seq.max
  in
  Results.Int' max

let part2 () =
  let register = Hashtbl.create 1000 in
  let max = ref 0 in
  program
  |> List.iter (fun i ->
      let cval = Hashtbl.find_default register i.c_reg 0 in
      if i.comp cval then (
        let rval = Hashtbl.find_default register i.reg 0 in
        let new_val = i.op rval in
        Hashtbl.replace register i.reg new_val;
        max := Int.max !max new_val));
  Results.Int' !max
