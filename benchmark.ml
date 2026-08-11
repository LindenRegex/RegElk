(** * Benchmarks  *)
(* This file is here to replicate the pictures in the paper *)
(* see also the file complexity_exp.ml for other experiments *)

open Regex
open Tojs
open Toexp
open Interpreter
open Sys
open Unix
open Gc
open Flags
open Arg
open Benchmark_vectors

(* path to V8 executable *)
(* this V8 executable needs to be patched in two ways: *)
(* - first, we augment the kMaxReplicationFactor, so that we can try more regexes to demonstrate regex-size-exponential complexity *)
(* second, we define performance.rdtsc() to return rdtsc counter *)
let old_v8_path = ref "~/v8/old/v8/out/x64.release/d8"
let new_v8_path = ref "~/v8/new/v8/out/x64.release/d8"
let v8_args = " --expose-gc --enable-experimental-regexp-engine "


(* where the results are stored *)
let bench_dir = "results_bench/"
(* where we store the argument to the d8 matcher *)
let param_path = "scripts_bench/v8params.js"
(* the V8 linear program that measures time *)
let v8lineartimer = "scripts_bench/v8lineartimer.js"
(* the irregexp program that measures time *)
let irregexptimer = "scripts_bench/irregexptimer.js"

(* the number of times we warmup before each test *)
let warmups = ref 10
(* the number of times we perform each test *)
let repetitions = ref 10


(* calling the matcher.native executable *)
(* returning the rdtsc measuring *)
let get_time_ocaml (bin: string) (r:raw_regex) (str:string) (impl:string): string =
  let regex_string = " \""^print_js r^"\" " in
  let input_string = " \""^str^"\" " in
  let sys = bin ^ " " ^ regex_string ^ input_string ^ string_of_int !warmups ^ " " ^ string_of_int !repetitions ^ " " ^ impl in
  string_of_command(sys)

(* modifies the parameter files that is read by the V8 D8 interpreter *)
let write_v8_params (r:raw_regex) (str:string): unit =
  let oc = open_out param_path in
  Printf.fprintf oc "const regex= \"%s\"\nconst string= \"%s\"\nconst warmups= %d\nconst repetitions =%d" (print_js r) str !warmups !repetitions;
  close_out oc

let get_time_oldv8linear (r:raw_regex) (str:string): string =
  write_v8_params r str;
  let sys = !old_v8_path ^ v8_args ^ v8lineartimer in
  string_of_command(sys)

let get_time_newv8linear (r:raw_regex) (str:string): string =
  write_v8_params r str;
  let sys = !new_v8_path ^ v8_args ^ v8lineartimer in
  string_of_command(sys)

let get_time_irregexp (r:raw_regex) (str:string): string =
  write_v8_params r str;
  let sys = !old_v8_path ^ v8_args ^ irregexptimer in
  string_of_command(sys)


(* measures rdtsc time for each engine *)
let get_time (e:engine) (r:raw_regex) (str:string) (impl:string): string =
  match e with
  | OCaml -> get_time_ocaml "./matcher.native" r str impl
  | OCamlBench -> failwith "get_time incompatible with OCamlBench engine"
  | OldV8Linear -> get_time_oldv8linear r str
  | NewV8Linear -> get_time_newv8linear r str
  | Irregexp -> get_time_irregexp r str
  | LinearBaseline -> get_time_ocaml "./linearbaseline.native" r str "ListRegs"


(* runs a regex-size benchmark on a single engine and prints the result to a csv file *)
let run_regex_config (ec:engine_conf) (param_regex:int->raw_regex) (str:string) (name:string) : unit =
  Printf.printf "Testing engine %s:\n%!" (engine_name ec.eng);
  let oc = open_out (bench_dir^name^"_"^(engine_name ec.eng)^".csv") in
  for i = ec.min_size to ec.max_size do
    Printf.printf " %s\r%!" (string_of_int i); (* live update *)
    let reg = param_regex i in
    let time = get_time ec.eng reg str name in
    Printf.fprintf oc "%d,%s%!" i time; (* printing to the csv file *)
  done;
  close_out oc;
  Printf.printf "\n%!";
  Unix.sleep 1

(* TODO add regex range in addition to str range in single function *)
let run_ocamlbench_config (ec:engine_conf) (param_str:int->string) (reg:raw_regex) (name:string) : unit =
  failwith "TODO"


let run_simple_config (ec:engine_conf) (param_str:int->string) (reg:raw_regex) (name:string) : unit =
  let oc = open_out (bench_dir^name^"_"^(engine_name ec.eng)^".csv") in
  for i = ec.min_size to ec.max_size do
    Printf.printf " %s\r%!" (string_of_int i); (* live update *)
    let str = param_str i in
    let time = get_time ec.eng reg str name in
    Printf.fprintf oc "%d,%s%!" i time; (* printing to the csv file *)
  done;
  close_out oc;
  Printf.printf "\n%!";
  Unix.sleep 1


(* runs a string-size benchmark on a single engine and prints the result to a csv file *)
let run_string_config (ec:engine_conf) (param_str:int->string) (reg:raw_regex) (name:string) : unit =
  Printf.printf "Testing engine %s:\n%!" (engine_name ec.eng);
  match ec.eng with
  | OCamlBench ->
     run_ocamlbench_config ec (param_str:int->string) (reg:raw_regex) (name:string)
  | _ ->
     run_simple_config ec (param_str:int->string) (reg:raw_regex) (name:string)

let run_regex_benchmark (rb:regex_benchmark) : unit =
  Printf.printf ("Generating .csv files for benchmark %s:\n") (rb.name);
  List.iter (fun rc -> run_regex_config rc rb.param_regex rb.input_str rb.name) rb.confs

let run_string_benchmark (sb:string_benchmark) : unit =
  Printf.printf ("Generating .csv files for benchmark %s:\n") (sb.name);
  List.iter (fun rc -> run_string_config rc sb.param_str sb.rgx sb.name) sb.confs




(** * Benchmark Executable  *)

let exec_bench (bench: benchmark) : unit =
  match bench with
  | RB b -> run_regex_benchmark b
  | SB b -> run_string_benchmark b


let main =
  debug := false;
  verbose := false;

  let bench_list = ref [] in

  let speclist =
    [("-oldv8", Arg.Set_string old_v8_path, "old V8 path");
     ("-newv8", Arg.Set_string new_v8_path, "new V8 path");
     ("-warmups", Arg.Set_int warmups, "Number of Warmup Repetitions per iteration");
     ("-repet", Arg.Set_int repetitions, "Number of Measured Repetitions");
    ] in
  let usage = "./benchmark.native [-oldv8 path_to_old_d8] [-newv8 path_to_new_d8] [-warmups 10] [-repet 10] benchmark list\n" in
  let full_usage = usage ^ "\nAvailable benchmarks: " ^ bench_names_string in
  Arg.parse speclist (fun s -> bench_list := s::!bench_list) full_usage;
  List.iter exec_bench (List.map benchmark_vector_of_string !bench_list)
