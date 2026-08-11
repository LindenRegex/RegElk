(** * Differential Fuzzer  *)
(* randomly generating regexes and strings and comparing the output of our algorithm VS the JS matcher *)

open Interpreter
open Tojs
open Regex
open Random
open Charclasses
open Flags
open Regs

module Interpreter = Interpreter(List_Regs)
module CMP = Tojs.Compare(Interpreter)

let random_seed = ref 0

(* we restrict ourselves to a small alphabet *)
let alphabet = ['a'; 'b'; '-']
(* with a dash (non-ascii) to test word boundaries *)

let max_depth = 50

let max_string = 100

let max_tests = ref 1000

let max_counted = 10

let max_class = 7

(** * Creating Random Regexes  *)

let random_char () : char =
  let idx = Random.int (List.length alphabet) in
  List.nth alphabet idx

let random_quant () : quantifier =
  match (Random.int 6) with
  | 0 -> Star
  | 1 -> LazyStar
  | 2 -> Plus
  | 3 -> LazyPlus
  | 4 -> QuestionMark
  | 5 -> LazyQuestionMark
  | _ -> failwith "random range error"

let random_counted_quant () : counted_quantifier =
  let min = Random.int max_counted in
  let greedy = Random.bool () in
  let max = if Random.bool() then None else Some (min + Random.int max_counted) in
  { min=min; max=max; greedy=greedy }


let random_look () : lookaround =
  match (Random.int 4) with
  | 0 -> Lookahead
  | 1 -> NegLookahead
  | 2 -> Lookbehind
  | 3 -> NegLookbehind
  | _ -> failwith "random range error"

let random_anchor () : anchor =
  match (Random.int 4) with
  | 0 -> BeginInput
  | 1 -> EndInput
  | 2 -> WordBoundary
  | 3 -> NonWordBoundary
  | _ -> failwith "random range error"

let random_group () : char_group =
  match (Random.int 6) with
  | 0 -> Digit
  | 1 -> NonDigit
  | 2 -> Word
  | 3 -> NonWord
  | 4 -> Space
  | 5 -> NonSpace
  | _ -> failwith "random range error"

let random_range () : char * char =
  let minc = Random.int 128 in
  let offset = Random.int (128-minc) in
  (char_of_int minc, char_of_int (minc+offset))

let random_elt () : char_class_elt =
  match (Random.int 3) with
  | 0 -> let x = random_char() in CChar x
  | 1 -> let r = random_range() in CRange (fst r, snd r)
  | 2 -> let g = random_group() in CGroup g
  | _ -> failwith "random range error"

let random_class () : char_class =
  let size = Random.int max_class in
  List.init size (fun _ -> random_elt())

let random_character () : character =
  match (Random.int 5) with
  | 0 -> let c = random_char() in Char c
  | 1 -> Dot
  | 2 -> let g = random_group() in Group g
  | 3 -> let cl = random_class() in Class cl
  | 4 -> let cl = random_class() in NegClass cl
  | _ -> failwith "random range error"

(* with a maximal number of recursion [depth] *)
(* the [look] boolean specifies if lookarounds are allowed *)
let rec random_regex (depth:int) (look:bool): raw_regex =
  let max = if look then 13 else 11 in
  let rand = if (depth=0) then Random.int 3 else Random.int max in
  match rand with
  | 0 -> Raw_empty
  | 1 | 2 | 3 -> let x = random_character() in Raw_character x
  | 4 -> let a = random_anchor() in Raw_anchor a
  | 5 ->
     let r1 = random_regex (depth-1) look in
     let r2 = random_regex (depth-1) look in
     Raw_alt (r1, r2)
  | 6 ->
     let r1 = random_regex (depth-1) look in
     let r2 = random_regex (depth-1) look in
     Raw_con (r1, r2)
  | 7 ->
     let r1 = random_regex (depth-1) look in
     let q = random_quant() in
     Raw_quant(q, r1)
  | 8 ->
     let r1 = random_regex (depth-1) look in
     let q = random_counted_quant() in
     Raw_count(q, r1)
  | 9 | 10 ->
     let r1 = random_regex (depth-1) look in
     Raw_capture(r1)
  | 11 | 12 ->
     let r1 = random_regex (depth-1) look in
     let l = random_look() in
     Raw_lookaround(l, r1)
  | _ -> failwith "random range error"


let random_raw () : raw_regex =
  let max = Random.int max_depth in
  random_regex max true

(** * Creating Random Strings  *)

let random_string () : string =
  let size = (Random.int max_string) in
  String.init size (fun _ -> random_char())


(** * The differential fuzzer itself  *)
let fuzzer () : unit =

  (* adding some statistics over nullable + *)
  let total_nn = ref 0 in
  let total_cdn = ref 0 in
  let total_cin = ref 0 in
  let total_lnn = ref 0 in
  let total_ln = ref 0 in
  let total_timeout = ref 0 in

  for i = 0 to !max_tests do
    let raw = random_raw() in
    let str = random_string() in
    let comp = CMP.compare_engines raw str in
    if (not comp) then total_timeout := !total_timeout +1;

    let (nn,cdn,cin,lnn,ln) = plus_stats (annotate raw) in
    total_nn := !total_nn + nn;
    total_cdn := !total_cdn + cdn;
    total_cin := !total_cin + cin;
    total_lnn := !total_lnn + lnn;
    total_ln := !total_ln + ln;

  done;

  Printf.printf "\n=====\n\n";
  Printf.printf "Total tests:                 %d\n" !max_tests;
  Printf.printf "NonNullable+:                %d\n" !total_nn;
  Printf.printf "ContextDependentNullable+:   %d\n" !total_cdn;
  Printf.printf "ContextIndependentNullable+: %d\n" !total_cin;
  Printf.printf "NonNullableLazy+:            %d\n" !total_lnn;
  Printf.printf "NullableLazy+:               %d\n" !total_ln;
  Printf.printf "JS Backtracking Timeouts:    %d\n" !total_timeout


(* calling the fuzzer *)
let main =

  verbose := false;
  debug := false;

  let speclist =
    [("-tests", Arg.Set_int max_tests, "Number of tests");
     ("-seed", Arg.Set_int random_seed, "Random seed");
    ] in

  let usage = "./fuzzer.native [-tests 1000] [-seed 0]" in
  Arg.parse speclist (fun _ -> ()) usage;
  Random.init !random_seed;
  fuzzer()
