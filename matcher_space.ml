open Interpreter
open Oracle
open Regex
open Bytecode
open Compiler
open Cdn
open Tojs
open Toexp
open Charclasses
open Vdflags
open Regs
open Spacebench

(*open Core*)
open Core_bench
open Command_unix

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

let get_impl_from_name (s:string) : string =
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

  let argv = Sys.argv in
  let regex = argv.(1) in
  let string = argv.(2) in
  let reg_implem = get_impl_from_name argv.(5) in

  (*let matcher = get_matcher reg_implem in*)
  let build_oracle = get_build_oracle reg_implem in
  let build_capture = get_build_capture reg_implem in

  (* building the regex *)
  let parsed_regex = parse_raw regex in
  (* annotating the regex *)
  let annotated_regex = annotate parsed_regex in
  (* compiling the regex *)
  let compiled_regex = full_compilation annotated_regex in

  (* triggering garbage collector *)
  Gc.full_major();

  (* Using Jane Street's core bench *)

  (* Set the command line arguments to column names so core bench does not complain *)
  argv.(1) <- "time";
  argv.(2) <- "alloc";
  argv.(3) <- "gc";
  argv.(4) <- "percentage";
  argv.(5) <- "samples";

  let cmd = Bench.make_command [
      Bench.Test.create ~name:"Matcher"
        (fun () -> 
          let o = build_oracle compiled_regex string in
          ignore(build_capture compiled_regex string o)
        )
    ] in

  Command_unix.run cmd

  (* Using a counter *)

  (*max_space := 0;
  current_space := 0;

  let o = build_oracle compiled_regex string in
  ignore(build_capture compiled_regex string o);

  Printf.printf ("%i\n") !max_space*)