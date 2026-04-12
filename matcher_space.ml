open Interpreter
open Oracle
open Regex
open Bytecode
open Compiler
open Cdn
open Tojs
open Toexp
open Charclasses
open Flags
open Regs
open Spacebench

module INTARRAY = Interpreter(Regs.Array_Regs) 
module INTLIST = Interpreter(Regs.List_Regs) 
module INTTREE = Interpreter(Regs.Map_Regs)
module INTVIRTUALTREE = Interpreter(Regs.Virtual_Tree_Regs)

let get_matcher (s:string) =
  if (s = "ArrayRegs") then INTARRAY.matcher
  else if (s = "MapRegs") then INTTREE.matcher
  else if (s = "VirtualTreeRegs") then INTVIRTUALTREE.matcher
  else INTLIST.matcher

let get_build_oracle (s:string) =
  if (s = "ArrayRegs") then INTARRAY.build_oracle
  else if (s = "MapRegs") then INTTREE.build_oracle
  else if (s = "VirtualTreeRegs") then INTVIRTUALTREE.build_oracle
  else INTLIST.build_oracle

let get_build_capture (s:string) =
  if (s = "ArrayRegs") then INTARRAY.build_capture
  else if (s = "MapRegs") then INTTREE.build_capture
  else if (s = "VirtualTreeRegs") then INTVIRTUALTREE.build_capture
  else INTLIST.build_capture

let get_engine_from_name (s:string) : string =
  match String.index_opt s '_' with
  | Some i -> String.sub s 0 i
  | None -> s

(** * Measuring The OCaml engine execution  *)
(* This executable is to be called directly by the benchmarks *)
   
(* Executing the OCaml linear engine on a regex and a string *)
(* Expects exactly 4 arguments:
- the regex
- the input string
- the number of warmup repetitions
- a string indicating the type of register implementation to use "ArrayRegs", "ListRegs" or "MapRegs" *)
(* Prints the total time in seconds *)

let input_str = ref ""
let input_regex = ref ""
let str_set = ref false
let rgx_set = ref false
let compare_js = ref false 
   
(* fails if the regex is not correct *)
let parse_raw (str:string) : raw_regex =
  let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
  assert (regex_wf r);
  r

  
let main =
  (* disabling debug/verbose output *)
  debug := false;
  verbose := false;
  
  let regex = Sys.argv.(1) in
  let string = Sys.argv.(2) in
  (*let warmups = int_of_string(Sys.argv.(3)) in
  let repetitions = int_of_string(Sys.argv.(4)) in*)
  let reg_implem = get_engine_from_name Sys.argv.(5) in

  (*let matcher = get_matcher reg_implem in*)
  let build_oracle = get_build_oracle reg_implem in
  let build_capture = get_build_capture reg_implem in

  (* building the regex *)
  let parsed_regex = parse_raw regex in
  (* annotating the regex *)
  let annotated_regex = annotate parsed_regex in
  (* compiling the regex *)
  let compiled_regex = full_compilation annotated_regex in

  (* Warmup *)
  (* this shouldn't change anything for this engine *)
  (* but we do it anyway to mirror our evaluation of the V8Linear engine *)
  (*for i=0 to (warmups-1) do
    ignore(matcher compiled_regex string)
  done;*)

  (* triggering garbage collector *)
  Gc.full_major();

  (* measuring matches *)
  (*let tstart = Timer.now() in*)
  (* get max_space for each run and sum them all *)
  (*for i = 0 to (repetitions - 1) do*) (*deterministic when using spacebench*)
  max_space := 0;
  let o = build_oracle compiled_regex string in
  ignore(build_capture compiled_regex string o);
  (*done;*)
  (*let time = Timer.elapsed tstart in*)
  
  Printf.printf ("%i\n") ((Gc.stat()).top_heap_words)

  (*Printf.printf ("%i\n") !max_space*)
 
