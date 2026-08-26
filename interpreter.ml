(** * Bytecode Interpreter  *)
(* A VM-based interpreter of the bytecode that handles thread priority *)

open Bytecode
open Regex
open Map
open Array
open Format
open Oracle
open Compiler
open Cdn
open Anchors
open Regs
open Charclasses
open Flags


module type INTERP = sig
  val regs_name : unit -> string
  val get_op : int array -> int -> int option
  val print_cap_regs : int Array.t -> int -> string -> string
  val build_oracle : compiled_regex -> string -> oracle
  val build_ts_table : compiled_regex -> string -> oracle -> int array
  val build_capture : compiled_regex -> string -> oracle -> (int Array.t) list
  val matcher : compiled_regex -> string -> (int Array.t) list
  val full_match : raw_regex -> string -> (int Array.t) list
  val get_linear_result : raw_regex -> string -> string
end
(* I might move the last two functions out of this signature *)

module Interpreter (Regs:REGS) : INTERP = struct

  let regs_name () = Regs.name

(** * Direction  *)
(* In our algorithm, the interpreter can traverse the string in a forward way or in a backward way *)
(* Backward is used when building the oracle for lookaheads *)
(* Or when building capture groups for lookbehinds *)

let print_direction (d:direction) : string =
  match d with
  | Forward -> "Forward"
  | Backward -> "Backward"

let oracle_direction (l:lookaround) : direction =
  match l with
  | Lookahead | NegLookahead -> Backward
  | Lookbehind | NegLookbehind -> Forward

let capture_direction (l:lookaround) : direction =
  match l with
  | Lookahead -> Forward
  | Lookbehind -> Backward
  | _ -> failwith "No capture on negative lookarounds"

(* negative lookarounds don't capture anything *)
let capture_type (l:lookaround) : bool =
  match l with
  | Lookahead | Lookbehind -> true
  | NegLookahead | NegLookbehind -> false

(* increments the current position in the string depending on the direction *)
let incr_cp (cp:int) (dir:direction) : int =
  match dir with
  | Forward -> cp + 1
  | Backward -> cp - 1

let init_cp (dir:direction) (str_size:int) : int =
  match dir with
  | Forward -> 0
  | Backward -> str_size
(* there are (str_size+1) distinct positions: before and after the string count *)

(* when writing to the oracle, if we've been going backward we need an offset *)
let cp_offset (dir:direction) : int =
  match dir with
  | Forward -> 0
  | Backward -> 1


(** * Capture Registers  *)

(* just printing the contents of an Array for debugging purposes *)
let debug_regs (regs:(int Array.t) list) : string =
      let s = ref "" in
      for c = 0 to (Array.length (List.hd regs))-1 do
        s := !s ^ string_of_int c ^ ": " ^ string_of_int ((List.hd regs).(c)) ^ " | "
      done;
      !s

(* each thread stores capture registers for the capture groups it has matched so far *)
(* module Regs = (List_Regs : REGS) *)


(** * String Manipulation *)

let get_char (str:string) (cp:int) : char option =
  try Some (String.get str cp) with
  | Invalid_argument _ -> None    (* when reaching the end of the string *)


(** * History Tree  *)
(* Todo:  move the thread and history tree defenition to another file *)
(* but the Reg.reg is undefined outside of this module *)
type node = {
  pc : int;                      (* only meaningful when not a leaf *)
  mutable start_pos : int;
  mutable parent : node option;
  mutable left : node option;
  mutable right : node option;
}

type slot = Left | Right

let holds (s : node option) (n : node) : bool =
  match s with Some c -> c == n | None -> false

let get_slot (p : node) (side : slot) : node option =
  match side with Left -> p.left | Right -> p.right

let set_slot (p : node) (side : slot) (v : node option) : unit =
  match side with Left -> p.left <- v | Right -> p.right <- v

let side_of (node : node) : (node * slot) option =
  match node.parent with
  | None -> None
  | Some parent ->
      if holds parent.left node then Some (parent, Left)
      else if holds parent.right node then Some (parent, Right)
      else failwith "Corrupt tree: node is not a child of its parent"

let detach (node : node) : unit =
  match side_of node with
  | None -> ()
  | Some (parent, side) ->
      set_slot parent side None;
      node.parent <- None

let replace ~(old_node : node) ~(new_node : node) : unit =
  match side_of old_node with
  | None -> failwith "replace: node is a root"
  | Some (parent, side) ->
      detach new_node;
      set_slot parent side (Some new_node);
      old_node.parent <- None;
      new_node.parent <- Some parent

let attach ~(child : node) ~(parent : node) (side : slot) : unit =
  detach child;
  (match get_slot parent side with
   | Some old_child -> detach old_child
   | None -> ());
  set_slot parent side (Some child);
  child.parent <- Some parent

let compare_history (new_node : node) (old_node : node) : bool =
  new_node.start_pos > old_node.start_pos

let g_tee  = "\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80 "   (* |--  *)
let g_end  = "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80 "   (* `--  *)
let g_bar  = "\xe2\x94\x82   "                          (* |    *)
let g_gap  = "    "

let print_forest ?(oc = stdout)
    (roots : node list) : unit =
  let label n = Printf.sprintf "pc=%d,start=%d" n.pc n.start_pos in
  let children n =
    match n.left, n.right with
    | Some l, Some r -> [ ("", l); ("", r) ]
    | Some l, None   -> [ ("L ", l) ]
    | None,   Some r -> [ ("R ", r) ]
    | None,   None   -> []
  in
  let rec render ~prefix ~branch ~cont n =
    Printf.fprintf oc "%s%s%s\n" prefix branch (label n);
    let kids = children n in
    let last = List.length kids - 1 in
    let prefix = prefix ^ cont in
    List.iteri
      (fun i (t, c) ->
        let is_last = i = last in
        render ~prefix
          ~branch:(if is_last then g_end else g_tee)
          ~cont:(if is_last then g_gap else g_bar)
          c)
      kids
  in
  match roots with
  | [] -> Printf.fprintf oc "<empty forest>\n"
  | roots ->
      let n = List.length roots in
      let last = n - 1 in
      List.iteri
        (fun i r ->
          let is_last = i = last in
          render ~prefix:"" ~branch:(if is_last then g_end else g_tee)
            ~cont:(if is_last then g_gap else g_bar) r)
        roots

(** * Threads  *)

(* each thread has a program counter in the bytecode and a map of capture registers *)
type thread =
  {
    mutable pc: int;
    mutable history_node: node;
    mutable capture_regs: Regs.regs; (* cp and clock for each capture group *)
    mutable look_regs: Regs.regs;    (* cp and clock for each lookaround *)
    mutable quant_regs: Regs.regs;   (* cp (if nulled) and clock for each quantifier *)
    mutable exit_allowed : bool;    (* are we allowed to exit the current loop *)
  }

let init_thread (initcap:Regs.regs) (initlook:Regs.regs) (initquant:Regs.regs) (start_pos:int): thread =
  let history_node = { start_pos = start_pos; pc = -1; parent = None; left = None; right = None } in
  { pc = 0; history_node = history_node; capture_regs = initcap; look_regs = initlook; quant_regs = initquant; exit_allowed = false }


let get_nodes (threads: thread list) : node list =
  List.map (fun t -> t.history_node) threads
let rec set_start_pos (nodes: node list) :unit = 
  match nodes with
  | [] -> ()
  | n :: ns ->
    if n.start_pos = -1 then n.start_pos <- (Option.get n.parent).start_pos;
    let ns = match n.right with Some r -> r :: ns | None -> ns in
    let ns = match n.left  with Some l -> l :: ns | None -> ns in
    set_start_pos ns
(** * PC Sets  *)

(* for each PC of the bytecode, we want to track if, in a step of the interpreter, this PC has already been processed *)
(* this prevents us from handling the same pc twice: only the top priority thread with that pc should be handled *)
(* We use a mutable array of booleans, since we know the size of the bytecode *)
type pcset = node option Array.t

let init_pcset (bytecode_size: int) : pcset =
  assert (bytecode_size > 0);
  Array.make bytecode_size None

(* add a pc to the set after it has been processed *)
let pc_add (pcs:pcset) (pc:label) (node:node): unit =
  pcs.(pc) <- Some node

(* check if a pc has been processed already *)
let pc_mem (pcs:pcset) (pc:label) : bool =
  match pcs.(pc) with
  | Some _ -> true
  | None -> false

let takes_priority (new_thread :thread) (old_thread:thread option) : bool =
  true

(* adds a thread and char at the head of a blocked list only if it's not already in *)
(* modifies the pcset in place *)
let add_thread (t:thread) (x:char_expectation) (current:(thread*char_expectation) list) (inset:pcset) : (thread*char_expectation) list =
  if (pc_mem inset t.pc) then current
  else begin
      pc_add inset t.pc t.history_node;
      (t,x)::current
    end

(** * Boolean PC Sets  *)

(* For each PC of the bytecode, remembers if it has been handled for each possible state of the exit_allowed boolean *)
type bpcset =
  {
   true_set: pcset;
   false_set: pcset;
  }

let init_bpcset (bytecode_size: int) : bpcset =
  { true_set = init_pcset(bytecode_size); false_set = init_pcset(bytecode_size) }

let bpc_add (bpcs:bpcset) (pc:label) (exit_bool:bool) (node:node) : unit =
  match exit_bool with
  | true -> pc_add bpcs.true_set pc node
  | false -> pc_add bpcs.false_set pc node

let bpc_mem (bpcs:bpcset) (pc:label) (exit_bool:bool) : bool =
  match exit_bool with
  | true -> pc_mem bpcs.true_set pc
  | false -> pc_mem bpcs.false_set pc

let get_history_node (bpcs:bpcset) (pc:label) (exit_bool:bool) : node option =
  match exit_bool with
  | true -> bpcs.true_set.(pc)
  | false -> bpcs.false_set.(pc)

(** * Interpreter States  *)

(* The interpreter alternates between two different steps to keep threads synchronized:
   - first it advances all of its states through epsilon transitions, until they reach a Consume instruction
   - then it reads the next input character of the string at the current position and advances or kill each thread
 *)

type interpreter_state =
  {
    mutable cp: int;             (* current position in the input string *)
    mutable active: thread list;    (* ordered list of threads. high to low priority *)
    mutable processed: bpcset;       (* already processed pcs. Similar to isPcProcessed in Experimental  *)
    (* mutable because we reset it each step, but its modifications during a step are done in-place *)
    mutable blocked: (thread*char_expectation) list;   (* threads stuck at a Consume instruction. low to high priority. *)
    mutable isblocked: pcset;       (* already blocked pcs, to avoid duplicates *)
    mutable bestmatch: thread option;   (* best match found so far, but there might be a higher priotity one still *)
    mutable context: char_context;             (* prev and next character at the current position *)
    mutable clock: int;                 (* global clock *)
    mutable cdn: cdn_table;             (* nullability table for cdn + *)
    mutable starting_nodes: node list;
  }

(* initializing the context *)
(* when starting the interpreter at any position, possibly in the middle of the string *)
let cp_context (cp:int) (str:string) (dir:direction) : char_context =
  let nextop = get_char str cp in
  let prevop = get_char str (cp-1) in
  match dir with
  | Forward -> { prevchar = prevop ; nextchar = nextop }
  | Backward -> { prevchar = nextop ; nextchar = prevop }

let init_state (c:code) (initcp:int) (initcap:Regs.regs) (initlook:Regs.regs) (initquant:Regs.regs) (initclk:int) (initctx:char_context) : interpreter_state =
  let initial_thread = init_thread initcap initlook initquant initcp in
  { cp = initcp;
    active = [initial_thread];
    processed = init_bpcset (size c);
    blocked = [];
    isblocked = init_pcset (size c);
    bestmatch = None;
    context = initctx;
    clock = initclk;
    cdn = init_cdn();
    starting_nodes = [initial_thread.history_node];
  }

(** * Debugging Utilities  *)

let print_exit_allowed (b:bool) : string =
  if b then "\027[32m✔\027[0m" else "\027[31m✘\027[0m"

let print_thread (t:thread) : string = string_of_int t.pc ^ print_exit_allowed t.exit_allowed

let print_active (l:thread list) : string =
  "  ACTIVE: " ^ List.fold_left (fun s t -> if (s = "") then (print_thread t) else (print_thread t) ^ ", " ^ s) "" l

let print_blk (b:thread * char_expectation) : string =
  "(" ^ print_thread (fst b) ^ ":" ^ expectation_to_string (snd b) ^ ")"

let print_blocked (l:(thread*char_expectation) list) : string =
  "  BLOCKED: " ^ List.fold_left (fun s b -> if (s = "") then (print_blk b) else (print_blk b) ^ ", " ^ s) "" l ^ "\n"

let print_cp (cp:int) : string =
  "  CP: " ^ string_of_int cp ^ "\n"

let print_match (b:thread option) =
  match b with
  | None -> "None\n"
  | Some t -> print_thread t ^ "\n" ^ Regs.to_string t.capture_regs

let print_bestmatch (b:thread option) =
  "  BEST: " ^ print_match b ^ "\n"


(** * Printing Results  *)
(* once the registers have been transformed to an array *)

let get_op (c:int Array.t) (reg:int) : int option =
  let value = c.(reg) in
  if (value < 0) then None else Some value


(* extracting a capture group slice given its registers *)
let print_slice (str:string) (startreg:int option) (endreg:int option) : string =
  match startreg with
  | None -> "Undefined"
  | Some startv ->
     begin match endreg with
     | None -> failwith "startreg is set but not endreg"
     | Some endv ->
        if (endv >= startv) then
          String.sub str startv (endv - startv)
        else
          String.sub str endv (startv - endv)
                     (* capture groups defined in lookbehind have their capture regs reversed *)
     end

(* printing all capture groups *)
let print_cap_regs (c:int Array.t) (max_groups:int) (str:string) : string =
  let s = ref "" in
  for i = 0 to max_groups do
    s := !s ^ "#" ^ string_of_int i ^ ":";
    let startr = get_op c (start_reg i) in
    let endr = get_op c (end_reg i) in
    s := !s ^ print_slice str startr endr ^ "\n"
  done;
  !s

let print_cap_option (c:(int Array.t) list) (max_groups:int) (str:string) : string =
  match c with
  | [] -> "NoMatch\n"
  | ca :: _ -> print_cap_regs ca max_groups str


let print_result (r:regex) (str:string) (c:(int Array.t) list) : string =
  let s = ref "" in
  if !verbose then
    s := !s ^ ("Result of matching " ^ print_regex r ^ " on string " ^ str ^ " : \n");
  let max = max_group r in
  !s ^ print_cap_option c max str ^ "\n"



(** * Filtering For Capture Reset  *)
(* At the end of the algorithm, we get many capture groups that should be reset *)
(* We use the quantifier registers to filter out those that are too old *)

(* modifies regs in-place *)
let rec filter_capture (r:regex) (cap_regs:int Array.t) (cap_clocks: int Array.t) (look_clocks:int Array.t) (quant_clocks:int Array.t) (maxclock:int) : unit =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> ()
  | Re_alt (r1,r2) -> filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks maxclock;
                      filter_capture r2 cap_regs cap_clocks look_clocks quant_clocks maxclock
  | Re_con (r1,r2) -> filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks maxclock;
                      filter_capture r2 cap_regs cap_clocks look_clocks quant_clocks maxclock
  | Re_quant (nul, qid, quant, r1) ->
     let quant_val = quant_clocks.(qid) in (* the last time we went in *)
     if (quant_val < maxclock) then
       (* the last repetition of the inner quantifier happened before the last repetition of the outer one *)
       filter_all r1 cap_regs
     else
       filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks quant_val
  | Re_capture (cid, r1) ->
     let start = cap_clocks.(start_reg cid) in
     if (start < 0) then
       filter_all r1 cap_regs (* there is already no value for this capture group, we can clear everything inside *)
     else begin
         if (start < maxclock) (* cleaning the value of group cid (and everything inside) if its value is too old *)
         then begin cap_regs.(start_reg cid) <- -1; filter_all r1 cap_regs end
         else filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks maxclock
       end
  | Re_lookaround (lid, l, r1) ->
     let look_val = look_clocks.(lid) in (* the last time we needed the lookaround to hold *)
     if (look_val < 0) then
       filter_all r1 cap_regs (* we didn't need the lookaround: clear everything inside *)
     else begin
         if (look_val < maxclock) (* cleaning everything inside the lookaround since it's too old *)
         then filter_all r1 cap_regs
         else filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks (-1)
                             (* resetting the maxclock to -1: lookaround clocks are reset *)
       end

and filter_all (r:regex) (regs:int Array.t) : unit = (* clearing all capture group inside a regex *)
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> ()
  | Re_alt (r1,r2) -> filter_all r1 regs; filter_all r2 regs
  | Re_con (r1,r2) -> filter_all r1 regs; filter_all r2 regs
  | Re_quant (nul, qid, quant, r1) ->
     filter_all r1 regs
  | Re_capture (cid, r1) ->
     regs.(start_reg cid) <- -1; filter_all r1 regs
  | Re_lookaround (lid, l, r1) -> filter_all r1 regs

(* we transform the registers to an Array with constant-time access and insertion when filtering *)
let filter_reset (r:regex) (capture:Regs.regs) (look:Regs.regs) (quant:Regs.regs) (maxclock:int) : int Array.t list =
  let (cap_regs, cap_clocks) = Regs.to_arrays capture in
  let (_, look_clocks) = Regs.to_arrays look in
  let (_, quant_clocks) = Regs.to_arrays quant in
  filter_capture r cap_regs cap_clocks look_clocks quant_clocks maxclock;
  [cap_regs]


(** * Interpreter  *)

(* modifies the state by advancing all threads along epsilon transitions *)
(* calls itself recursively until there are no more active threads *)
(* the direction is only used to evaluate anchors *)
let rec advance_epsilon (c:code) (s:interpreter_state) (o:oracle) (dir:direction) (findall:bool) : unit =
  if !debug then Printf.printf "%s\n%!" ("Clock "^string_of_int s.clock^"|Epsilon active: " ^ print_active s.active);

  match s.active with
  | [] -> () (* done advancing epsilon transitions *)
  | t::ac -> (* t: highest priority active thread *)
    let i = get_instr c t.pc in
    if (bpc_mem s.processed t.pc t.exit_allowed) then (* killing the lower priority thread if it has already been processed *)
      begin s.active <- ac;
        let old_node = Option.get (get_history_node s.processed t.pc t.exit_allowed) in
        if compare_history t.history_node old_node then
          replace ~old_node:old_node ~new_node:t.history_node;
        advance_epsilon c s o dir findall
      end
    else begin
       s.clock <- s.clock + 1;  (* augmenting the global clock *)
       bpc_add s.processed t.pc t.exit_allowed t.history_node; (* adding the current pc being handled to the set of proccessed pcs *)

       let new_node = { start_pos = -1; pc = t.pc; parent = Some t.history_node; left = None; right = None } in
       if t.history_node.left  = None then
        t.history_node.left <- Some new_node
       else
        t.history_node.right <- Some new_node;
       t.history_node <- new_node;

       match i with
       | Consume ce -> (* adding the thread to the list of blocked thread if it isn't already there *)
          s.blocked <- add_thread t ce s.blocked s.isblocked; (* also updates isblocked *)
          s.active <- ac;
          advance_epsilon c s o dir findall
       | Accept ->             (* updates the best match and don't consider the remain active threads *)
          if findall then begin
            s.active <- ac;
            s.bestmatch <- Some t;
            advance_epsilon c s o dir findall
          end
          else begin
            s.active <- [];
            s.bestmatch <- Some t;
            () (* no recursive call *)
          end
       | Jmp x ->
          t.pc <- x;
          advance_epsilon c s o dir findall
       | Fork (x,y) ->           (* x has higher priority *)
          t.pc <- y;
          s.active <- {pc = x;
                       history_node = t.history_node;
                       capture_regs = Regs.copy t.capture_regs;
                       look_regs = Regs.copy t.look_regs;
                       quant_regs = Regs.copy t.quant_regs;
                       exit_allowed = t.exit_allowed}::s.active;
          advance_epsilon c s o dir findall
       | SetRegisterToCP r ->
          (* modifying the capture regs of the current thread *)
          t.capture_regs <- Regs.set_reg t.capture_regs r (Some s.cp) s.clock;
          t.pc <- t.pc + 1;
          advance_epsilon c s o dir findall
       | SetQuantToClock (q,b) ->
          (* saving the current cp if we are nulling a + *)
          let ocp = if b then (Some s.cp) else None in
          (* adding the last iteration clock *)
          t.quant_regs <- Regs.set_reg t.quant_regs q ocp s.clock;
          t.pc <- t.pc + 1;
          advance_epsilon c s o dir findall
       | CheckOracle l ->
          if (get_oracle o s.cp l)
          then begin
              t.pc <- t.pc + 1; (* keeping the thread alive *)
              (* remembering the cp where we last needed the oracle *)
              t.look_regs <- Regs.set_reg t.look_regs l (Some s.cp) s.clock;
            end
          else s.active <- ac;  (* killing the thread *)
          advance_epsilon c s o dir findall
       | NegCheckOracle l ->
          if (get_oracle o s.cp l)
          then s.active <- ac   (* killing the thread *)
          else t.pc <- t.pc + 1;(* keeping the thread alive *)
          advance_epsilon c s o dir findall
       | WriteOracle l ->
          (* we reached a match but we want to write that into the oracle. we don't discard lower priority threads *)
          s.active <- ac;       (* no need to consider that thread anymore *)
          set_oracle o s.cp l;    (* writing to the oracle *)
          advance_epsilon c s o dir findall  (* we keep searching for more matches *)
       | BeginLoop ->
       (* we need to set exit_allowed to false: now exiting a loop is forbidden according to JS semantics *)
          t.exit_allowed <- false;
          t.pc <- t.pc + 1;
          advance_epsilon c s o dir findall
       | EndLoop ->
          (* this transition is only possible if we didn't begin this loop during this epsilon transition phase *)
          begin match t.exit_allowed with
          | true -> t.pc <- t.pc+1; advance_epsilon c s o dir findall
          | false -> s.active <- ac; advance_epsilon c s o dir findall (* killing the current thread *)
          end
       | CheckNullable qid ->
          if (cdn_get s.cdn qid)
          then t.pc <- t.pc+1   (* keeping the thread alive *)
          else s.active <- ac;  (* killing the thread *)
          advance_epsilon c s o dir findall
       | AnchorAssertion a ->
          if (is_satisfied a s.context dir)
          then t.pc <- t.pc+1   (* keeping the thread alive *)
          else s.active <- ac;  (* killing the thread *)
          advance_epsilon c s o dir findall
       | Fail ->
          s.active <- ac;       (* killing the current thread *)
          advance_epsilon c s o dir findall
     end


(* modifies the state by consuming the next character  *)
(* calls itself recursively until there are no more blocked threads *)
let rec consume (s:interpreter_state): unit =
  match s.blocked with
  | [] -> ()
  | (t,ce)::blocked' ->
     s.blocked <- blocked';
     if (is_accepted s.context.nextchar ce) then
       begin t.exit_allowed <- true; t.pc <- t.pc + 1; s.active <- t::s.active end;
     (* adding t to the list of active threads *)
     (* since t just consumed something, we set its exit_allowed flag to true *)
     consume s


(** * Null interpreter  *)
(* an interpreter that does not read the string, but instead simply follows epsilon transitions *)
(* this is used to reconstruct the capture groups of last nulled plus iteration at the end of a match *)
(* the direction is only used to evaluate anchors *)
(* This function expects that s contains an up-to-date cp, context and cdn table *)
let null_interp (c:code) (s:interpreter_state) (o:oracle) (dir:direction): thread option =
  if !verbose then Printf.printf "%s CP%d\n" ("\n\027[36mNull Interpreter:\027[0m ") (s.cp);
  if !verbose then Printf.printf "%s\n" (print_code c);
  if !debug then
    begin
      Printf.printf "%s" (print_cp s.cp);
      Printf.printf "%s" (print_active s.active);
      Printf.printf "%s%!" (print_bestmatch s.bestmatch);
    end;
  if !debug then
    begin
      Printf.printf "At CP%d, CDN table:%s\n" (s.cp) (print_cdn_table s.cdn);
    end;
  (* follow epsilon transitions *)
  advance_epsilon c s o dir false;
  if !debug then
    begin
      Printf.printf "%s\n%!" (print_blocked s.blocked);
    end;
  s.bestmatch


(** * Finding the top priority match in a bytecode automaton  *)
(* This functions assumes that s.context already contains the correct characters *)
(* this does not yet reconstruct any plus, simply alternates advance epsilon and consume *)
let rec find_match (c:code) (main_ast:regex) (str:string) (s:interpreter_state) (o:oracle) (dir:direction) (cdn:cdns) (findall:bool) (table:int Array.t): thread option =
  if !debug then
    begin
      Printf.printf "%s" (print_cp s.cp);
      Printf.printf "%s" (print_active s.active);
      Printf.printf "%s%!" (print_bestmatch s.bestmatch);
    end;

  (* building the CDN table *)
  s.cdn <- build_cdn cdn s.cp o s.context dir;
  if !debug then
    begin
      Printf.printf "At CP%d, CDN table:%s\n" (s.cp) (print_cdn_table s.cdn);
    end;

  (* follow epsilon transitions *)
  let previous_ends = get_nodes s.active in
  s.bestmatch <- None;
  advance_epsilon c s o dir findall;

  if findall then begin
    set_start_pos previous_ends;
    if !debug then
      print_forest s.starting_nodes;
    table.(s.cp) <- (match s.bestmatch with
    | None -> -1
    | Some t -> t.history_node.start_pos);
  end;

  
  if !debug then
    begin
      Printf.printf "%s\n%!" (print_blocked s.blocked);
    end;
  (* checking if there are still surviving threads *)
  match s.blocked, s.context.nextchar with
  | [], _ -> s.bestmatch        (* no more surviving threads *)
  | _, None -> s.bestmatch      (* we reached the end of the string *)
  | _, _ ->
     (* advancing blocked threads *)
     consume s;
     (* resetting the processed, blocked sets and the CDN table *)
     s.processed <- init_bpcset (size c);
     s.isblocked <- init_pcset (size c);
     s.cdn <- init_cdn();
     (* advancing the current position *)
     s.cp <- incr_cp s.cp dir;
     (* updating the context *)
     let newchar = get_char str (s.cp - cp_offset dir) in
     update_context s.context newchar;
     (* recursive call *)

     if findall then begin
      let maxlook = max_lookaround main_ast in
      let maxcap = max_group main_ast in
      let maxquant = max_quant main_ast in

      let capture = Regs.init_regs (2*maxcap+2) in
      let lookmem = Regs.init_regs (maxlook+1) in
      let quant = Regs.init_regs (maxquant+1) in
      
      let new_thread = init_thread capture lookmem quant s.cp in
      s.starting_nodes <- new_thread.history_node :: s.starting_nodes;
      s.active <- new_thread::s.active;
     end;
     find_match c main_ast str s o dir cdn findall table


(** * Reconstructing Nullable + Values  *)
(* when the winning thread of a match decided to go through the nullable path of a +, we might need to reconstruct any groups set during that nullable path *)
(* for this, we need the bytecode of every nullable plus, and the AST of the regex we previously matched *)
(* so that we can reconstruct exactly the plusses that are defined inside that AST *)

let reconstruct_plus_groups (thread:thread) (ast:regex) (plus_bc:code Array.t) (s:string) (o:oracle) (dir:direction): thread =
  let capture = ref thread.capture_regs in
  let look = ref thread.look_regs in
  let quant = ref thread.quant_regs in
  (* goes through the regex, if it encounters a nulled +, it calls the null interpreter *)
  let rec nulled_plus (reg:regex) : unit =
    match reg with
    | Re_empty | Re_character _ -> ()
    | Re_alt (r1, r2) | Re_con (r1, r2) ->
       nulled_plus r1; nulled_plus r2
    | Re_capture (_,r1) -> nulled_plus r1
    | Re_lookaround (lid,lk,r1) -> ()
    | Re_anchor _ -> ()
    (* from shallowest to deepest plus: *)
    | Re_quant (nul,qid,quanttype,body) ->
       begin match (Regs.get_cp !quant qid) with
       | None -> nulled_plus body (* recursive call: an inner + may have been nulled *)
       | Some start_cp ->         (* the last iteration of the plus was nulling *)
          let start_clock = int_of_opt (Regs.get_clock !quant qid) in
          if !debug then Printf.printf ("QID: %d | start_clock: %d\n") qid start_clock;
          let bytecode = plus_bc.(qid) in
          let ctx = cp_context start_cp s dir in
          let inits = (init_state bytecode start_cp !capture !look !quant start_clock ctx) in
          let subcdn = compile_cdns body in
          let subtable = build_cdn subcdn start_cp o ctx dir in
          inits.cdn <- subtable;
          let result = null_interp bytecode inits o dir in
          begin match result with
          | None -> failwith "expected a nullable plus"
          | Some w ->             (* there's a winning thread when nulling *)
             (* updating all registers *)
             capture := w.capture_regs;
             look := w.look_regs;
             quant := w.quant_regs;
          end;
          nulled_children body subtable start_cp
       end
  (* goes through the subregex when a plus above was nulled *)
  (* for all its children that got nulled while nulling the parent plus, *)
  (* the CDN table can be shared *)
  and nulled_children (reg:regex) (cdnt:cdn_table) (cp:int) : unit =
    match reg with
    | Re_empty | Re_character _ -> ()
    | Re_alt (r1, r2) | Re_con (r1, r2) ->
       nulled_children r1 cdnt cp; nulled_children r2 cdnt cp
    | Re_capture (_,r1) -> nulled_children r1 cdnt cp
    | Re_lookaround (lid,lk,r1) -> ()
    | Re_anchor _ -> ()
    | Re_quant (nul,qid,quanttype,body) ->
       begin match (Regs.get_cp !quant qid) with
       | None -> nulled_children body cdnt cp
       | Some start_cp ->
          if (start_cp = cp) then begin
              (* otherwise we don't have to reconstruct, it was nulled in a previous iteration *)
              let start_clock = int_of_opt (Regs.get_clock !quant qid) in
              let bytecode = plus_bc.(qid) in
              let ctx = cp_context cp s dir in
              let inits = (init_state bytecode cp !capture !look !quant start_clock ctx) in
              inits.cdn <- cdnt;
              let result = null_interp bytecode inits o dir in
              begin match result with
              | None -> failwith "expected a nullable children plus"
              | Some w ->             (* there's a winning thread when nulling *)
                 (* updating all registers *)
                 capture := w.capture_regs;
                 look := w.look_regs;
                 quant := w.quant_regs;
              end;
              nulled_children body cdnt cp
            end
          else ()
       end
  in
  nulled_plus ast;
  {pc = thread.pc; history_node = thread.history_node; capture_regs = !capture; look_regs = !look; quant_regs = !quant; exit_allowed = thread.exit_allowed}


(** * Finds a match in an a bytecode automaton AND reconstructs the corresponding plus groups  *)

(* running the interpreter on some code, with a particular initial interpreter state *)
(* also reconstructs the + groups *)
let find_match_plus (c:code) (ast:regex) (plus_bc:code Array.t) (s:string) (o:oracle) (dir:direction) (start_cp:int) (capture:Regs.regs) (look:Regs.regs) (quant:Regs.regs) (start_clock:int) (cdn:cdns): thread option =
  if !verbose then Printf.printf "%s - %s\n" ("\n\027[36mInterpreter:\027[0m "^s) (print_direction dir);
  if !verbose then Printf.printf "%s\n" (print_code c);
  if !verbose then Printf.printf "%s\n" (print_cdns cdn);
  if !verbose then Printf.printf "%s\n" (print_context (cp_context start_cp s dir));
  let initstate = init_state c start_cp capture look quant start_clock (cp_context start_cp s dir) in
  let result = find_match c ast s initstate o dir cdn false [||] in
  (* reconstruct + groups *)
  let full_result =
    match result with
    | None -> None
    | Some thread -> Some (reconstruct_plus_groups thread ast plus_bc s o dir)
  in
  if !verbose then Printf.printf "%s\n" ("\027[36mResult:\027[0m "^(print_match full_result));
  full_result


(* (\* running the interpreter using the default initial state *\)
 * let interp_default_init (cr:compiled_regex) (s:string) (o:oracle) (dir:direction) (cdn:cdns): thread option =
 *   let maxcap = max_group cr.ast in
 *   let maxlook = max_lookaround cr.ast in
 *   let maxquant = max_quant cr.ast in
 *   let capture = Regs.init_regs (2*maxcap+2) in
 *   let look = Regs.init_regs (maxlook+1) in
 *   let quant = Regs.init_regs (maxquant+1) in
 *   interp cr s o dir (init_cp dir (String.length s)) capture look quant 0 cdn
 *
 * (\* for tests, sometimes we only want to know if there is a match *\)
 * let boolean_interp (cr:compiled_regex) (s:string) (o:oracle) (dir:direction) (cdn:cdns): bool =
 *   match (interp_default_init cr s o dir cdn) with
 *   | None -> false
 *   | _ -> true *)


(* the following functions work on the main regex, in its compiled_regex form *)


(** * Bulding the Oracle  *)
(* we consider lookarounds by reverse order of their identifiers *)
(* we do not need to do this for the main regex *)

let build_oracle (cr:compiled_regex) (str:string): oracle =
  let maxlook = max_lookaround cr.main_ast in
  let maxcap = max_group cr.main_ast in
  let maxquant = max_quant cr.main_ast in
  let o = create_oracle (String.length str) (maxlook + 1) in
  for lid = maxlook downto 1 do
    let bytecode = cr.look_build_bc.(lid) in
    let looktype = cr.look_types.(lid) in
    let direction = oracle_direction looktype in
    let lookcdn = cr.look_cdns.(lid) in
    let initcp = init_cp direction (String.length str) in
    let initctx = cp_context initcp str direction in
    (* TODO: we could reuse capture, lookmem and quants instead of reallocating for each lookaround *)
    let capture = Regs.init_regs (2*maxcap+2) in
    let lookmem = Regs.init_regs (maxlook+1) in
    let quant = Regs.init_regs (maxquant+1) in
    let initstate = init_state bytecode initcp capture lookmem quant 0 initctx in
    if !verbose then Printf.printf "%s\n" (print_code bytecode);
    if !verbose then Printf.printf "%s\n" (print_cdns lookcdn);
    (* no need to call find_match_plus, we don't care about any capture groups *)
    (* inside lookarounds in the oracle building phase *)
      ignore (find_match bytecode cr.main_ast str initstate o direction lookcdn false [||])
  done;
  o                             (* returning the modified oracle *)

  let build_ts_table (cr:compiled_regex) (str:string) (o:oracle) : int array =
    let len = String.length str in
    let table = Array.make (len + 1) (-1) in

    let maxlook = max_lookaround cr.main_ast in
    let maxcap = max_group cr.main_ast in
    let maxquant = max_quant cr.main_ast in

    let bytecode = cr.reversed_bc in
    let direction = Backward in
    let start_cp = init_cp direction (String.length str) in
    let initctx = cp_context start_cp str direction in
    let capture = Regs.init_regs (2*maxcap+2) in
    let lookmem = Regs.init_regs (maxlook+1) in
    let quant = Regs.init_regs (maxquant+1) in
    let initstate = init_state bytecode start_cp capture lookmem quant 0 initctx in

    ignore (find_match bytecode cr.main_ast str initstate o direction cr.main_cdns true table);
    table

(** * Finding the main match and reconstructing lookaround capture groups  *)

(* returns the register array if there is a match *)
(* also filters the return value for capture reset *)
let build_capture (cr:compiled_regex) (str:string) (o:oracle): (int Array.t) list =
  let max_look = max_lookaround cr.main_ast in
  let max_cap = max_group cr.main_ast in
  let max_quant = max_quant cr.main_ast in
  let capture = Regs.init_regs(2*max_cap+2) in
  let look = Regs.init_regs (max_look+1) in
  let quant = Regs.init_regs (max_quant+1) in
  let main_bytecode = cr.main_bc in
  let main_cdn = cr.main_cdns in
  (* performing the match of the main expression, with plus group reconstruction *)
  let main_result =
    find_match_plus main_bytecode cr.main_ast cr.plus_bc str o Forward 0 capture look quant 0 main_cdn in
  match main_result with
  | None -> []
  | Some thread ->
     (* we have a match and want to rebuild capture groups in lookarounds*)
     let capture = ref thread.capture_regs in
     let look = ref thread.look_regs in
     let quant = ref thread.quant_regs in
     for lid=1 to max_look do
       match (Regs.get_cp !look lid) with
       | None -> ()             (* the lookaround wasn't needed in the match *)
       | Some cp ->             (* the lookaround had a match at cp *)
          let looktype = cr.look_types.(lid) in
          if (capture_type looktype) then (* not for negative lookarounds *)
            let bytecode = cr.look_capture_bc.(lid) in
            let direction = capture_direction looktype in
            let lookcdn = cr.look_cdns.(lid) in
            let lookast = cr.look_ast.(lid) in
            let result = find_match_plus bytecode lookast cr.plus_bc str o direction cp !capture !look !quant 0 lookcdn in
            begin match result with
            | None -> failwith "result expected from the oracle"
            | Some t ->
               (* updating all registers *)
               capture := t.capture_regs;
               look := t.look_regs;
               quant := t.quant_regs
            end
     done;
     if !debug then
       begin
         Printf.printf "regs: %s\n%!" (Regs.to_string !capture);
         let (prefilter,preclocks) = Regs.to_arrays(!capture) in
         let (_,quantclocks) = Regs.to_arrays(!quant) in
         Printf.printf "pre-filtering regs: %s\n%!" (debug_regs [prefilter]);
         Printf.printf "pre-filtering clocks: %s\n%!" (debug_regs [preclocks]);
         Printf.printf "pre-filtering quant clocks: %s\n%!" (debug_regs [quantclocks]);
       end;
     let match_capture = filter_reset cr.main_ast !capture !look !quant (-1) in (* filtering old values *)
     if !debug then Printf.printf "filtered regs: %s\n%!" (debug_regs match_capture);
     match_capture



(** * The Full matcher  *)
(* builds the oracle, then matches the main expression and rebuilds missing capture groups, and filters *)

let matcher (cr:compiled_regex) (str:string) : (int Array.t) list =
  let o = build_oracle cr str in
  if !debug then
    Printf.printf "%s\n" (print_oracle o);
  let ca = build_capture cr str o in
  if !verbose then
    Printf.printf "%s\n" (print_result cr.main_ast str ca);
  ca

let full_match (raw:raw_regex) (str:string) : (int Array.t) list =
  let re = annotate raw in
  let cr = full_compilation re in
  matcher cr str

let get_linear_result (raw:raw_regex) (str:string) : string =
  let capop = full_match raw str in
  print_result (annotate raw) str capop

end
