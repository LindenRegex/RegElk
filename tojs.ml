(** * Building JS programs  *)
(* so that we can compare our algorithms to a JS engine *)

open Regex
open Sys
open Filename
open Interpreter
open Charclasses
open Flags
open Findall

(** * JS Regex pretty-printing  *)
(* printing regexes in the JS style so that we can compare our results to a JS engine *)
(* adding a non-capturing group to a string *)
let noncap (s:string) : string =
  "(?:" ^ s ^ ")"

(* we put non-capturing groups everywhere to ensure the non-ambiguity *)
(* we could be more clever and put less non-capturing groups *)
let rec print_js (ra:raw_regex) : string =
  match ra with
  | Raw_empty -> ""
  | Raw_character c -> print_character c
  | Raw_alt (r1, r2) -> noncap(print_js r1) ^ "|" ^ noncap(print_js r2)
  | Raw_con (r1, r2) -> noncap(print_js r1) ^ noncap(print_js r2)
  | Raw_quant (q, r1) -> noncap(print_js r1) ^ print_quant q
  | Raw_count (q, r1) -> noncap(print_js r1) ^ print_counted_quant q
  | Raw_capture r1 -> "(" ^ print_js r1 ^ ")"
  | Raw_lookaround (l, r1) -> "(" ^ print_lookaround l ^ print_js r1 ^ ")"
  | Raw_anchor a -> print_anchor a


(** * Calling the JS Matcher  *)

(* geting the result of a command as a strng *)
let string_of_command (command:string) : string =
 let tmp_file = Filename.temp_file "" ".txt" in
 let _ = Sys.command @@ command ^ " >" ^ tmp_file in
 let chan = open_in tmp_file in
 let output = ref "" in
 try
   while true do
     output := !output ^ input_line chan ^ "\n"
   done; !output
 with
   End_of_file ->
   close_in chan;
   !output


let shell_quote s = "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"
(* getting its result as a string *)
let get_js_result (raw:raw_regex) (str:string) (max_groups:int) : match_result list =
  let out = String.trim (string_of_command (Printf.sprintf
    "timeout 5s node scripts_bench/jsmatcher.js %s '' %d %s"
    (shell_quote (print_js raw)) max_groups (shell_quote str))) in
  match List.rev (String.split_on_char '\n' out) with
  | "END" :: _ when String.starts_with ~prefix:"Error" out -> failwith (Printf.sprintf "JS error: %s" out)
  | "END" :: rev_body -> 
      List.rev_map (fun l ->
        Array.of_list (List.map int_of_string (String.split_on_char ' ' l))
      ) (List.filter (fun l -> String.length l > 0) rev_body)
  | _ -> failwith "Timeout"

(* calling the JS timer that starts and ends its timer just before and after matching the regex *)
let get_time_js (raw:raw_regex) (str:string) : string =
  let js_regex = print_js raw in
  let js_regex = "'" ^ js_regex ^ "'" in (* adding quotes to escape special characters *)
  let js_command = "node scripts_bench/jstimer.js " ^ js_regex ^ " " ^ "'"^str^"'" in
  string_of_command(js_command)

(** *  Comparing JS engine with our engine *)

module Compare (Interpreter:INTERP)(FindAll: FINDALL): sig
  val compare_engines : raw_regex -> string -> bool
  val compare_engines_all : raw_regex -> string -> bool
end = struct

type compare_result =
  | Equal
  | Timeout
  | Error


let compare_js_ocaml (raw:raw_regex) (str:string) : compare_result =
  let dbg_save = !debug and ver_save = !verbose in
  debug := false; verbose := false;

  Printf.printf "\027[36mRegex:\027[0m %s || " (print_regex (annotate raw));
  Printf.printf "\027[36mJS Regex:\027[0m %s || " (print_js raw);
  Printf.printf "\027[36mString:\027[0m \"%s\"\n%!" str;
  Printf.printf "%s\n%!" (report_raw raw);

  let max_groups = max_group (annotate raw) in
  let js_matches = get_js_result raw str max_groups 
  in
  let js_result_str = Interpreter.print_cap_option js_matches max_groups str in
  Printf.printf "\027[35mJS result:\027[0m\n%s%!" js_result_str;
  flush stdout;

  let linear_matches = Interpreter.full_match raw str in
  let linear_result_str = Interpreter.print_cap_option linear_matches max_groups str in
  Printf.printf "\027[35mLinear result:\027[0m\n%s%!" linear_result_str;

  let result = if js_result_str = linear_result_str then Equal else Error in

  debug := dbg_save;
  verbose := ver_save;
  result


let get_js_all_matches (raw:raw_regex) (str:string) (max_groups:int) : match_result list =
  let out = String.trim (string_of_command (Printf.sprintf
    "timeout 5s node scripts_bench/jsallmatcher.js %s '' %d %s"
    (shell_quote (print_js raw)) max_groups (shell_quote str))) in
  (* Printf.printf "\027[35mJS all matches result:\027[0m\n%s\n%!" out; *)
  match List.rev (String.split_on_char '\n' out) with
  | "END" :: _ when String.starts_with ~prefix:"Error" out -> failwith (Printf.sprintf "JS error: %s" out)
  | "END" :: rev_body -> List.rev_map (fun l ->
       Array.of_list (List.map int_of_string (String.split_on_char ' ' l))) rev_body
  |_-> failwith (Printf.sprintf "Timeout")

let compare_js_ocaml_all ?(algo=Clemele) (raw:raw_regex) (str:string) : compare_result =
  let dbg_save = !debug and ver_save = !verbose in
  debug := false; verbose := false;

  Printf.printf "\027[36mRegex:\027[0m %s || " (print_regex (annotate raw));
  Printf.printf "\027[36mJS Regex:\027[0m %s || " (print_js raw);
  Printf.printf "\027[36mString:\027[0m \"%s\"\n%!" str;
  Printf.printf "%s\n%!" (report_raw raw);

  let max_groups = max_group (annotate raw) in
  let js_all_matches = get_js_all_matches raw str max_groups in
  let js_all_matches_str = FindAll.print_all_matches js_all_matches raw str in
  Printf.printf "\027[35mJS result:\027[0m\n%s%!" js_all_matches_str;
  flush stdout;
  let ocaml_all_matches = FindAll.find_all algo raw str in
  let ocaml_all_matches_str = FindAll.print_all_matches ocaml_all_matches raw str in
  Printf.printf "\027[35mLinear result:\027[0m\n%s%!" ocaml_all_matches_str;
   
  let result = if js_all_matches_str = ocaml_all_matches_str then Equal
               else Error in

    debug := dbg_save;
    verbose := ver_save;
    result

(* fails on errors, and returns false on timeouts (we couldn't verify the equality) *)
let compare_engines (raw:raw_regex) (str:string) : bool =
  let cr = compare_js_ocaml raw str in
  match cr with
  | Error -> failwith "Mismatch between backtracking and linear"
  | Timeout -> false
  | Equal -> true
  
let compare_engines_all (raw:raw_regex) (str:string) : bool =
  let cr = compare_js_ocaml_all raw str in
    match cr with
    | Error -> failwith "Mismatch between backtracking and linear"
    | Timeout -> false
    | Equal -> true
end
