(** * Defining the Oracl type and operations  *)
(* the array in which we store for each position of the string if each lookaround can be matched or not *)

open Array

type oracle = bool Array.t Array.t

let create_oracle (str_size:int) (look_nb:int): oracle =
  assert (str_size >= 0);
  Array.make_matrix (str_size+1) look_nb false

(* we only allow setting to true, there's no reason to set an entry of the table back to false *)
let set_oracle (o:oracle) (cp:int) (lookid:int): unit =
  assert (cp < Array.length o);
  assert (lookid < Array.length o.(0));
  assert (cp >= 0);
  o.(cp).(lookid) <- true

let get_oracle (o:oracle) (cp:int) (lookid:int): bool =
  assert (cp < Array.length o);
  assert (lookid < Array.length o.(0));
  assert (cp >= 0);
  o.(cp).(lookid)

(** * Pretty-printing  *)

let print_bool (b:bool) : string =
  if b then "\027[32m✔\027[0m" else "\027[31m✘\027[0m"

let print_oracle (o:oracle) : string =
  let s = ref "\027[31mOracle:\027[0m\n" in
  for j = 0 to ((Array.length o.(0)) -1) do
    s := !s ^ "\027[36m" ^ string_of_int j ^ ":\027[0m ";
    for i = 0 to ((Array.length o) -1) do
      s := !s ^ print_bool (get_oracle o i j);
    done;
      s := !s ^ "\n";
  done;
  !s
