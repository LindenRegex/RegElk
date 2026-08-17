open Oracle
open Regex
open Bytecode
open Compiler
open Cdn
open Interpreter
open Findall
open Tojs
open Charclasses
open Flags
open Regs

(* different registers implementations *)
type reg_impl =
  | RegArray
  | RegList
  | RegTree

(* Executing the OCaml linear engine on a regex and a string *)

let input_str = ref ""
let input_regex = ref ""
let str_set = ref false
let rgx_set = ref false
let compare_js = ref false
let reg_implem = ref RegList    (* by default, use lists *)
let find_all = ref false
let all_algo = ref default_algo
   
(* fails if the regex is not correct *)
let parse_raw (str:string) : raw_regex =
  let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
  assert (regex_wf r);
  r

(* Appendix example *)
let appendix_regex : raw_regex =
  let behind = Raw_lookaround(Lookbehind,Raw_con(raw_char('c'),raw_star(Raw_capture(raw_char('a'))))) in
  let ahead = Raw_lookaround(Lookahead,Raw_con(raw_star(raw_char('a')),Raw_con(behind,raw_char('b')))) in
  Raw_con(Raw_capture(raw_char('c')),raw_star(Raw_con(raw_char('a'),ahead)))

let appendix_string : string = "caab"

(* choosing the right functions depending on the register implementation *)
let compare (ri:reg_impl) : raw_regex -> string -> bool =
  match ri with
  | RegArray -> let module INT = Interpreter(Regs.Array_Regs) in
                 let module CMP = Tojs.Compare(INT) in
                 CMP.compare_engines
  | RegList -> let module INT = Interpreter(Regs.List_Regs) in
               let module CMP = Tojs.Compare(INT) in
               CMP.compare_engines
  | RegTree -> let module INT = Interpreter(Regs.Map_Regs) in
               let module CMP = Tojs.Compare(INT) in
               CMP.compare_engines

let linear (ri:reg_impl) : raw_regex -> string -> string =
  match ri with
  | RegArray -> let module INT = Interpreter(Regs.Array_Regs) in
                INT.get_linear_result
  | RegList -> let module INT = Interpreter(Regs.List_Regs) in
               INT.get_linear_result
  | RegTree -> let module INT = Interpreter(Regs.Map_Regs) in
               INT.get_linear_result

let linear_all (ri:reg_impl) : algo -> raw_regex -> string -> string =
  match ri with
  | RegArray -> let module INT = Interpreter(Regs.Array_Regs) in
                let module ALL = Findall.FindAll(INT) in
                ALL.get_all_result
  | RegList -> let module INT = Interpreter(Regs.List_Regs) in
               let module ALL = Findall.FindAll(INT) in
               ALL.get_all_result
  | RegTree -> let module INT = Interpreter(Regs.Map_Regs) in
               let module ALL = Findall.FindAll(INT) in
               ALL.get_all_result


let main =

  let speclist =
    [("-regex", Arg.Tuple [Arg.Set_string input_regex; Arg.Set rgx_set], "Regex");
     ("-string", Arg.Tuple [Arg.Set_string input_str; Arg.Set str_set], "String");
     ("-v", Arg.Set verbose, "Verbose Mode");
     ("-d", Arg.Set debug, "Debug Mode");
     ("-cmp", Arg.Set compare_js, "Comparison with the Node engine");
     ("-array", Arg.Unit (fun _ -> reg_implem := RegArray), "Use Array registers");
     ("-tree", Arg.Unit (fun _ -> reg_implem := RegTree), "Use Tree registers");
     ("-list", Arg.Unit (fun _ -> reg_implem := RegList), "Use List registers");
     ("-all", Arg.Symbol (List.map fst algo_names,
                          fun s -> all_algo := List.assoc s algo_names; find_all := true),
      " Return all matches, using the given algorithm");
    ] in

  let usage = "./main.native [-regex \"(b)|.*\"] [-string \"abc\"] [-v] [-d] [-cmp] [-all "
              ^ string_of_algo default_algo ^ "]" in
  Arg.parse speclist (fun _ -> ()) usage;

  (* if no regex or string were provided, ask the user to input them *)
  if not !rgx_set then begin
      Printf.printf "\027[36mEnter your regex:\027[0m\n";
      input_regex := read_line ()
    end;

  let regex = parse_raw !input_regex in
  if !verbose then Printf.printf "\027[33mParsed Regex:\027[0m\n%s\n" (report_raw regex);

  if not !str_set then begin
      Printf.printf "\027[36mEnter your string:\027[0m\n";
      input_str := read_line ()
    end;


  if !compare_js then
    ignore ((compare !reg_implem) regex !input_str)
  else if !find_all then
    Printf.printf "%s" ((linear_all !reg_implem) !all_algo regex !input_str)
  else
    Printf.printf "%s" ((linear !reg_implem) regex !input_str)
