open Batteries

type instruction = {
  op : int -> int;
  reg : string;
  comp : int -> bool;
  c_reg : string;
}

let parse_line (line : string) : instruction =
  match String.split_on_char ' ' line with
  | [ reg; ud; n; "if"; c_reg; c; t ] ->
      let n = int_of_string n in
      let op =
        match ud with
        | "inc" -> fun x -> x + n
        | "dec" -> fun x -> x - n
        | _ -> failwith "unknown operation"
      in
      let t = int_of_string t in
      let comp =
        match c with
        | ">" -> fun x -> x > t
        | "<" -> fun x -> x < t
        | ">=" -> fun x -> x >= t
        | "<=" -> fun x -> x <= t
        | "==" -> fun x -> x = t
        | "!=" -> fun x -> x <> t
        | _ -> failwith "unknown comp"
      in
      { op; reg; comp; c_reg }
  | _ -> failwith "invalid instruction"

let program : instruction list =
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
