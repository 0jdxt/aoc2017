open Batteries

let get_registers () : (char, int) Hashtbl.t =
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

let to_target (s : string) : target =
  match int_of_string_opt s with
  | Some x -> Val x
  | None -> Reg (String.get s 0)

let get_target (registers : (char, int) Hashtbl.t) : target -> int = function
  | Val x -> x
  | Reg c -> Hashtbl.find registers c

let ins : instruction array =
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

let make_program (p : int) (iqueue : int Queue.t) (oqueue : int Queue.t) :
    program =
  let reg = get_registers () in
  Hashtbl.replace reg 'p' p;
  { registers = reg; i = ref 0; iqueue; oqueue }

type status = Running | Blocked

let part2 () =
  let step (p : program) =
    match Array.get ins !(p.i) with
    | Snd r ->
        Queue.push (Hashtbl.find p.registers r) p.oqueue;
        incr p.i;
        Running
    | Set (r, t) ->
        Hashtbl.replace p.registers r (get_target p.registers t);
        incr p.i;
        Running
    | Add (r, t) ->
        let x = Hashtbl.find p.registers r in
        Hashtbl.replace p.registers r (x + get_target p.registers t);
        incr p.i;
        Running
    | Mul (r, t) ->
        let y = Hashtbl.find p.registers r in
        Hashtbl.replace p.registers r (y * get_target p.registers t);
        incr p.i;
        Running
    | Mod (r, t) ->
        let x = Hashtbl.find p.registers r in
        Hashtbl.replace p.registers r (x mod get_target p.registers t);
        incr p.i;
        Running
    | Rcv r ->
        if Queue.is_empty p.iqueue then Blocked
        else (
          Hashtbl.replace p.registers r (Queue.pop p.iqueue);
          incr p.i;
          Running)
    | Jgz (x, y) ->
        if get_target p.registers x > 0 then
          p.i := !(p.i) + get_target p.registers y
        else incr p.i;
        Running
  in

  let aq = Queue.create () in
  let bq = Queue.create () in

  let ap = make_program 0 bq aq in
  let bp = make_program 1 aq bq in

  let rec run_until_block (p : program) =
    match step p with Running -> run_until_block p | status -> status
  in

  let rec scheduler (a : program) (b : program) (count : int) =
    match (run_until_block a, run_until_block b, Queue.length bq) with
    | Blocked, Blocked, 0 when Queue.is_empty aq -> count
    | _, _, x -> scheduler a b (count + x)
  in

  Results.Int' (scheduler ap bp 0)
