(** * Defining the Oracl type and operations  *)
(* the array in which we store for each position of the string if each lookaround can be matched or not *)

open Array
open Regs

type regtype =
  | Capture
  | Lookaround
  | Quantifier
type cell = {
  mutable visited    : bool;
  mutable next_state : (int * int);
  mutable prev_states: (int * int) list;
  mutable holds      : bool;
  mutable cp         : int;
  mutable reg        : int;
  mutable regtype    : regtype;
}

type oracle  = cell array array
type oracles = oracle array

let fresh_cell () = { visited = false; next_state = (-1, -1); prev_states = []; holds = false; cp = -1; reg= -1; regtype = Capture }

let update_cell (o:oracle) (pos: int) (s: int) (next_state: int * int) (holds: bool) (cp: int) (reg: int) (regtype: regtype): unit=
  let cell = o.(pos).(s) in  
  
  if next_state <> (-1, -1) && next_state <> (pos,s) then begin
    cell.next_state <- next_state;
    let (next_pos, next_s) = next_state in
    let next_cell = o.(next_pos).(next_s) in
    next_cell.prev_states <- (pos, s) :: next_cell.prev_states;

    cell.holds <- holds;
    cell.cp <- cp;
    cell.reg <- reg;
    cell.regtype <- regtype;
  end
  (* do we also need to have a type for register:(lookaround/ capture/ quantifier) *)
(*Todo: the lookarounds ids start at 1 but here not sure if the state_counts list is reversed or not:) *)
let create_oracles (str_len : int) (state_counts : int array) : oracles =
  state_counts
  |> Array.map (fun n_states ->
       Array.init (str_len + 1) (fun _ ->
         Array.init (n_states*2) (fun _ -> fresh_cell ())))

(* we only allow setting to true, there's no reason to set an entry of the table back to false *)
(* let set_oracle (o:oracles) (cp:int) (lookid:int): unit =
  assert (cp < Array.length o);
  assert (lookid < Array.length o.(0));
  assert (cp >= 0);
  o.(lookid).(cp).(lookid) <- true
*)
(* rename to look_holds in the future? *)
let get_oracle (o:oracles) (cp:int) (lookid: int): bool =
  assert (lookid < Array.length o);
  let oracle = o.(lookid) in
  assert(cp < Array.length oracle);
  assert(cp >= 0);
  oracle.(cp).(0).holds
(*let get_oracle (o:oracles) (cp:int) (lookid:int): bool =
  assert (cp < Array.length o);
  assert (lookid < Array.length o.(0));
  assert (cp >= 0);
  o.(cp).(lookid) *)

(** * Pretty-printing  *)

let print_bool (b:bool) : string =
  if b then "\027[32m✔\027[0m" else "\027[31m✘\027[0m"

(* let print_oracle (o:oracles) : string =
  let s = ref "\027[31mOracle:\027[0m\n" in
  for j = 0 to ((Array.length o.(0)) -1) do
    s := !s ^ "\027[36m" ^ string_of_int j ^ ":\027[0m ";
    (* for i = 0 to ((Array.length o) -1) do
      s := !s ^ print_bool (get_oracle o i j); *)
    done;
      s := !s ^ "\n";
  (* done; *)
  !s *)

type match_result = {
  accept     : bool;
  capture    : Array_Regs.regs;
  lookaround : Array_Regs.regs;
  quantifier : Array_Regs.regs;
}
type oracle_res = match_result array


let create_match_result (capture_cnt:int) (look_cnt:int) (quant_cnt:int) : match_result =
  { accept = true;
    capture = Array_Regs.init_regs capture_cnt;
    lookaround = Array_Regs.init_regs look_cnt;
    quantifier = Array_Regs.init_regs quant_cnt }
let create_oracle_res (str_len : int) : oracle_res =
  Array.make (str_len + 1) { accept = false;
                              capture = Array_Regs.init_regs 0;
                              lookaround = Array_Regs.init_regs 0;
                              quantifier = Array_Regs.init_regs 0 }
let copy_match_result (res:match_result) : match_result =
  { accept = res.accept;
    capture = Array_Regs.copy res.capture;
    lookaround = Array_Regs.copy res.lookaround;
    quantifier = Array_Regs.copy res.quantifier }
let rec init_oracle_res (res:oracle_res) (cp_list: int option list) (capture_cnt:int) (look_cnt: int) (quant_cnt:int) = 
  match cp_list with
  | [] -> ()
  | None :: tl -> init_oracle_res res tl capture_cnt look_cnt quant_cnt
  | Some cp :: tl ->
    res.(cp) <- create_match_result capture_cnt look_cnt quant_cnt;
    init_oracle_res res tl capture_cnt look_cnt quant_cnt

let print_oracles (o:oracles) : string =
  let format_oracle (idx:int) (oracle:oracle) : string =
    let format_rows (format_cell:int -> cell -> string * int) =
      let state_count =
        if Array.length oracle = 0 then 0 else Array.length oracle.(0)
      in
      let column_widths = Array.make state_count 0 in
      let rows =
        Array.mapi
          (fun pos states -> pos, Array.mapi (fun s cell -> format_cell s cell) states)
          oracle
      in
      Array.iter
        (fun (_, cells) ->
          Array.iteri
            (fun s (_, width) ->
              column_widths.(s) <- max column_widths.(s) width)
            cells)
        rows;
      let position_width =
        max 1 (String.length (string_of_int (max 0 (Array.length oracle - 1))))
      in
      Array.map
        (fun (pos, cells) ->
          let values =
            Array.mapi
              (fun s (value, width) ->
                if s = Array.length cells - 1 then value
                else value ^ String.make (column_widths.(s) - width) ' ')
              cells
          in
          Printf.sprintf "\027[36m%*d:\027[0m [%s]"
            position_width pos (String.concat " " (Array.to_list values)))
        rows
    in
    let holds_rows =
      format_rows (fun s cell ->
        Printf.sprintf "%d:%s" s (print_bool cell.holds),
        String.length (string_of_int s) + 2)
    in
    let next_state_rows =
      format_rows (fun s cell ->
        let next_pos, next_state = cell.next_state in
        let color = if cell.holds then "\027[32m" else "\027[31m" in
        let value = Printf.sprintf "%d:(%d,%d)" s next_pos next_state in
        color ^ value ^ "\027[0m", String.length value)
    in
    Printf.sprintf
      "\027[31mOracle %d:\027[0m\nHolds:\n%s\nNext states:\n%s\n"
      idx
      (String.concat "\n" (Array.to_list holds_rows))
      (String.concat "\n" (Array.to_list next_state_rows))
  in
  String.concat "\n" (Array.to_list (Array.mapi format_oracle o))