(** * Finding All Matches  *)
(* Given a regex and a string, returns every non-overlapping match instead of *)
(* only the leftmost one.                                                     *)
(* Several algorithms can be used to do so: they are selected on the command  *)
(* line with [-all <algorithm>] and represented by the [algo] variant below   *)

open Regex
open Bytecode
open Compiler
open Interpreter
open Flags
open Oracle
open Anchors

(** * The Find-All Algorithms  *)

type algo =
  | Naive
  | TokenSkip

let all_algos : algo list = [Naive; TokenSkip]
let default_algo : algo = Naive

let string_of_algo (a:algo) : string =
  match a with
  | Naive -> "naive"
  | TokenSkip -> "tokenskip"

let algo_names : (string * algo) list =
  List.map (fun a -> (string_of_algo a, a)) all_algos


type match_result = int Array.t


(** * The Find-All Engine  *)
module type FINDALL = sig
  val find_all : algo -> raw_regex -> string -> match_result list
  val get_all_result : algo -> raw_regex -> string -> string
end

module FindAll (I:INTERP) : FINDALL = struct
  (* the boundaries of the entire match (group 0) *)
  let match_bounds (c:match_result) : (int * int) option =
    match (I.get_op c (start_reg 0)), (I.get_op c (end_reg 0)) with
    | Some mstart, Some mend -> Some (mstart, mend)
    | _, _ -> None

  let shift_regs (offset:int) (regs:match_result) : match_result =
    Array.map (fun v -> if v < 0 then v else v + offset) regs

  (* Todo: don't copy the whole string each time *)
  let find_all_naive (cr:compiled_regex) (str:string) : match_result list =
    let len = String.length str in
    let rec loop (idx:int) (acc:match_result list) : match_result list =
      if (idx > len) then List.rev acc
      else
        let remaining = String.sub str idx (len - idx) in
        if !debug then
          Printf.printf "\027[35mFindall:\027[0m searching from %d in %S\n%!" idx remaining;
        match I.matcher cr remaining with
        | [] -> List.rev acc      (* no match left in the suffix: we are done *)
        | regs :: _ ->
           let regs = shift_regs idx regs in
           begin match match_bounds regs with
           | None -> List.rev acc
           | Some (mstart, mend) ->
              if !verbose then
                Printf.printf "\027[35mFindall:\027[0m match at [%d,%d[\n%!" mstart mend;
              let next = if (mend > mstart) then mend else mend + 1 in
              loop next (regs::acc)
           end
    in
    loop 0 []


  let matcher_token_skip (cr:compiled_regex) (str:string) : match_result list =
    let o = I.build_oracle cr str in
    let tb = I.build_ts_table cr str o in
    Printf.printf "this is the table:\n";
    for i = 0 to (Array.length tb) - 1 do
      Printf.printf "%d " tb.(i)
    done;
    Printf.printf "\n";
    let ca = I.build_capture cr str o in
    ca

  let find_all_tokenskip (cr:compiled_regex) (str:string) : match_result list =
    matcher_token_skip cr str

  let find_all (a:algo) (raw:raw_regex) (str:string) : match_result list =
    if !verbose then
      Printf.printf "\027[33mFind-all algorithm:\027[0m %s\n" (string_of_algo a);
    let cr = full_compilation (annotate raw) in
    match a with
    | Naive -> find_all_naive cr str
    | TokenSkip -> find_all_tokenskip cr str

  let get_all_result (a:algo) (raw:raw_regex) (str:string) : string =
    match find_all a raw str with
    | [] -> "NoMatch\n"
    | l ->
       let max_groups = max_group (annotate raw) in
       let nb = List.length l in
       let b = Buffer.create 256 in
       Buffer.add_string b
         (Printf.sprintf "%d match%s\n" nb (if nb = 1 then "" else "es"));
       List.iteri
         (fun i c ->
           let position =
             match match_bounds c with
             | Some (mstart, mend) -> Printf.sprintf " [%d,%d]" mstart mend
             | None -> failwith "false match report"
           in
           Buffer.add_string b (Printf.sprintf "\nMatch %d%s:\n" (i+1) position);
           Buffer.add_string b (I.print_cap_regs c max_groups str))
         l;
       Buffer.contents b

end