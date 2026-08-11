(** * Building JS programs  *)
(* so that we can compare our algorithms to a JS engine *)

open Regex
open Sys
open Filename
open Interpreter
open Charclasses
open Flags

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


(* getting its result as a string *)
let get_js_result (raw:raw_regex) (str:string) : string =
  let js_regex = print_js raw in
  let js_regex = "'" ^ js_regex ^ "'" in (* adding quotes to escape special characters *)
  let js_command = "timeout 5s node scripts_bench/jsmatcher.js " ^ js_regex ^ " " ^ "'"^str^"'" in
  let result = string_of_command(js_command) in
  if (String.length result = 0) then "Timeout\n\n" else result

(* calling the JS timer that starts and ends its timer just before and after matching the regex *)
let get_time_js (raw:raw_regex) (str:string) : string =
  let js_regex = print_js raw in
  let js_regex = "'" ^ js_regex ^ "'" in (* adding quotes to escape special characters *)
  let js_command = "node scripts_bench/jstimer.js " ^ js_regex ^ " " ^ "'"^str^"'" in
  string_of_command(js_command)

(** *  Comparing JS engine with our engine *)

module Compare (Interpreter:INTERP): sig
  val compare_engines : raw_regex -> string -> bool
end = struct

type compare_result =
  | Equal
  | Timeout
  | Error


let compare_js_ocaml (raw:raw_regex) (str:string) : compare_result =
  (* saving the values of debug and verbose *)
  (* because this compares the output string, verbose and debug needs to be turned off *)
  let dbg_save = !debug in
  let ver_save = !verbose in
  debug := false;
  verbose := false;

  Printf.printf "\027[36mRegex:\027[0m %s || " (print_regex (annotate raw));
  Printf.printf "\027[36mJS Regex:\027[0m %s || " (print_js raw);
  Printf.printf "\027[36mString:\027[0m \"%s\"\n%!" str;
  Printf.printf "%s\n%!" (report_raw raw);
  let sjs = get_js_result raw str in
  Printf.printf "\027[35mJS result:\027[0m\n%s%!" sjs;
  let sl = Interpreter.get_linear_result raw str in
  Printf.printf "\027[35mLinear result:\027[0m\n%s%!" sl;
  let result = if (String.compare sjs "Timeout\n\n" = 0) then Timeout
               else if (String.compare sjs sl = 0) then Equal else Error in

  (* resetting flag values *)
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

end
