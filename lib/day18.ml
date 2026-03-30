open Batteries

let get_registers () =
  String.to_seq "abfip" |> Seq.map (fun c -> (c, 0)) |> Hashtbl.of_seq

type target = Reg of char | Val of int

type instruction =
  | Snd of char
  | Set of char * target
  | Add of char * target
  | Mul of char * target
  | Mod of char * target
  | Rcv of char
  | Jgz of target * target

let to_target s =
  match int_of_string_opt s with
  | Some x -> Val x
  | None -> Reg (String.get s 0)

let get_target registers = function
  | Val x -> x
  | Reg c -> Hashtbl.find registers c

let ins =
  File.lines_of "data/day18.txt"
  |> Enum.map (fun ln ->
      match String.split_on_char ' ' ln with
      | [ "snd"; reg ] -> Snd (String.get reg 0)
      | [ "rcv"; reg ] -> Rcv (String.get reg 0)
      | [ "set"; reg; x ] -> Set (String.get reg 0, to_target x)
      | [ "add"; reg; x ] -> Add (String.get reg 0, to_target x)
      | [ "mul"; reg; x ] -> Mul (String.get reg 0, to_target x)
      | [ "mod"; reg; x ] -> Mod (String.get reg 0, to_target x)
      | [ "jgz"; reg; x ] -> Jgz (to_target reg, to_target x)
      | _ -> failwith "unknown instruction")
  |> Array.of_enum

let part1 () =
  let freq = ref 0 in
  let i = ref 0 in
  let registers = get_registers () in
  while !i >= 0 do
    match Array.get ins !i with
    | Snd r ->
        freq := Hashtbl.find registers r;
        incr i
    | Set (r, t) ->
        Hashtbl.replace registers r (get_target registers t);
        incr i
    | Add (r, t) ->
        let x = Hashtbl.find registers r in
        Hashtbl.replace registers r (x + get_target registers t);
        incr i
    | Mul (r, t) ->
        let y = Hashtbl.find registers r in
        Hashtbl.replace registers r (y * get_target registers t);
        incr i
    | Mod (r, t) ->
        let x = Hashtbl.find registers r in
        Hashtbl.replace registers r (x mod get_target registers t);
        incr i
    | Rcv r ->
        let x = Hashtbl.find registers r in
        if x <> 0 then i := -1 else incr i
    | Jgz (x, y) ->
        if get_target registers x > 0 then i := !i + get_target registers y
        else incr i
  done;
  Results.Int' !freq

type program = {
  registers : (char, int) Hashtbl.t;
  i : int ref;
  iqueue : int Queue.t;
  oqueue : int Queue.t;
}

let make_program p iqueue oqueue =
  let reg = get_registers () in
  Hashtbl.replace reg 'p' p;
  { registers = reg; i = ref 0; iqueue; oqueue }

type status = Running | Blocked

let part2 () =
  let step program =
    match Array.get ins !(program.i) with
    | Snd r ->
        Queue.push (Hashtbl.find program.registers r) program.oqueue;
        incr program.i;
        Running
    | Set (r, t) ->
        Hashtbl.replace program.registers r (get_target program.registers t);
        incr program.i;
        Running
    | Add (r, t) ->
        let x = Hashtbl.find program.registers r in
        Hashtbl.replace program.registers r (x + get_target program.registers t);
        incr program.i;
        Running
    | Mul (r, t) ->
        let y = Hashtbl.find program.registers r in
        Hashtbl.replace program.registers r (y * get_target program.registers t);
        incr program.i;
        Running
    | Mod (r, t) ->
        let x = Hashtbl.find program.registers r in
        Hashtbl.replace program.registers r
          (x mod get_target program.registers t);
        incr program.i;
        Running
    | Rcv r ->
        if Queue.is_empty program.iqueue then Blocked
        else (
          Hashtbl.replace program.registers r (Queue.pop program.iqueue);
          incr program.i;
          Running)
    | Jgz (x, y) ->
        if get_target program.registers x > 0 then
          program.i := !(program.i) + get_target program.registers y
        else incr program.i;
        Running
  in

  let a_queue = Queue.create () in
  let b_queue = Queue.create () in

  let a_prog = make_program 0 b_queue a_queue in
  let b_prog = make_program 1 a_queue b_queue in

  let rec run_until_block program =
    match step program with
    | Running -> run_until_block program
    | status -> status
  in

  let rec scheduler a_prog b_prog count =
    match
      (run_until_block a_prog, run_until_block b_prog, Queue.length b_queue)
    with
    | Blocked, Blocked, 0 when Queue.is_empty a_queue -> count
    | _, _, x -> scheduler a_prog b_prog (count + x)
  in

  Results.Int' (scheduler a_prog b_prog 0)
