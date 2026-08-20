(** * Tests  *)
(* Some manual testing, and mostly tests from the fuzzer *)
(* that were found and fixed during development *)

open Oracle
open Regex
open Bytecode
open Compiler
open Cdn
open Interpreter
open Tojs
open Flags
open Regs

module PlayTests (R:REGS): sig
  val tests : unit -> unit
end = struct

  module Interpreter = Interpreter(R)
  module CMP = Tojs.Compare(Interpreter)

(** * Manual Testing *)

let oracle_tests () =
  let o = create_oracle 4 8 in
  assert (Array.length o = 5);
  assert (Array.length (o.(0)) = 8);
  assert (get_oracle o 1 4 = false);
  set_oracle o 1 4;
  Printf.printf "%s\n" (print_oracle o);
  assert (get_oracle o 1 4 = true);
  assert (get_oracle o 1 3 = false)

let regex_tests () =
  let raw = Raw_con(raw_char 'a', Raw_lookaround (Lookbehind, raw_char 'a')) in
  Printf.printf "%s\n" (print_raw raw);
  let re = annotate raw in
  Printf.printf "%s\n" (print_regex re);
  assert (get_lookaround re 2 = None);
  assert (get_lookaround re 0 = None);
  assert (get_lookaround re 1 = Some (Re_character (Char 'a'), Lookbehind));
  let rr = reverse_regex re in
  Printf.printf "%s\n" (print_regex rr);
  assert (rr = Re_capture (0, Re_con(Re_lookaround (1, Lookbehind, Re_character (Char 'a')), Re_character (Char 'a'))));
  let rc = remove_capture rr in
  Printf.printf "%s\n" (print_regex rc);
  assert (rc = Re_con(Re_lookaround (1, Lookbehind, Re_character (Char 'a')), Re_character (Char 'a')))

let bytecode_tests () =
  let code = [Jmp 1; Fork (0,2); Accept] in
  let bytecode = Array.of_list code in
  Printf.printf "%s\n" (print_code bytecode);
  assert (nb_epsilon bytecode = 3)

let compiler_tests () =
  let raw = Raw_con (Raw_quant (Star, raw_char 'a'), raw_char 'b') in
  let re = annotate raw in
  let code = compile_to_bytecode re in
  Printf.printf "%s\n" (print_code code);
  assert (Array.to_list code = [SetRegisterToCP 0; Fork (2,7); SetQuantToClock (1,false); BeginLoop; Consume (Single 'a'); EndLoop; Jmp 1; Consume (Single 'b'); SetRegisterToCP 1; Accept])

let interpreter_tests () =
  let raw = Raw_con (Raw_quant (Star, raw_char 'a'), raw_char 'b') in
  let str1 = "aab" in
  let str2 = "aaa" in
  assert (Interpreter.full_match raw str1 <> []);
  assert (Interpreter.full_match raw str2 = [])

let build_oracle_tests () =
  let raw = Raw_con(Raw_con (Raw_lookaround (Lookahead, raw_char 'a'), Raw_lookaround (Lookbehind, Raw_con (raw_char 'a',raw_char 'b'))), Raw_lookaround(Lookbehind, Raw_empty)) in
  let re = annotate raw in
  let cr = full_compilation re in
  let str = "aaab" in
  Printf.printf "%s\n" (print_regex re);
  let o = Interpreter.build_oracle cr str in
  Printf.printf "%s\n" (print_oracle o);
  assert (get_oracle o 4 2 = true);
  assert (get_oracle o 3 2 = false);
  assert (get_oracle o 2 1 = true);
  assert (get_oracle o 2 0 = false);
  assert (get_oracle o 4 1 = false)

let full_algo_tests () =
  let raw = Raw_con (raw_char 'a', Raw_lookaround (Lookahead, Raw_capture(raw_char 'b'))) in
  let str = "cab" in
  ignore(Interpreter.full_match raw str)

let compare_engines_tests() =
  ignore(CMP.compare_engines (Raw_con (Raw_quant (Star, Raw_capture (raw_char 'a')), raw_char 'b')) "aaab");
  ignore(CMP.compare_engines (raw_char 'a') "b");
  ignore(CMP.compare_engines (Raw_quant (Star, Raw_alt (Raw_capture(raw_char 'a'), Raw_capture(raw_char 'b')))) "ababab")


(** * Gathering some errors found with the fuzzer *)

let string_sub_errors : (raw_regex*string) list = (* FIXED! reverse registers for lookbehinds groups *)
  [(Raw_lookaround(Lookbehind,Raw_capture(raw_dot)),"bababaaabbacacbabbacabcccaaacaabccab");
   (Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_capture(Raw_capture(Raw_alt(Raw_empty,Raw_lookaround(Lookbehind,Raw_lookaround(NegLookahead,Raw_quant(LazyStar,Raw_alt(Raw_lookaround(NegLookahead,raw_dot),Raw_capture(raw_dot)))))))),Raw_lookaround(Lookbehind,Raw_capture(raw_dot))))),"bcacaaaacaabcbbcacaaacbbaabc");
   (Raw_lookaround(Lookbehind,Raw_capture(raw_dot)),"cabacbbccbacbcbbccbaccbaccabbbaaa");
   (Raw_lookaround(Lookbehind,Raw_capture(Raw_capture(raw_dot))),"bbacccabbcccbcccaabcbabcaaacacacbbbcabbc")]

let oracle_assert_errors : (raw_regex*string) list = (* FIXED! read str.cp - 1 backward *)
  [(Raw_con(Raw_lookaround(Lookbehind,Raw_empty),Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,raw_char('a')))),"ccbba")]

let expected_result_oracle_errors : (raw_regex*string) list = (* FIXED! read str.cp - 1 backward *)
  [(Raw_lookaround(Lookbehind,raw_char('b')),"bccaaacabcbcabaacbccaccbbbaaccccaabcac");
   (Raw_lookaround(Lookbehind,raw_char('a')),"cabbcabcccacbbabcb")]

let idk : (raw_regex*string) list = (* FIXED, by one of the changes above *)
  [(Raw_capture(Raw_con(Raw_con(Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_alt(Raw_alt(raw_char('b'),Raw_capture(Raw_empty)),raw_char('a')))),Raw_quant(Star,Raw_lookaround(NegLookbehind,Raw_capture(Raw_lookaround(NegLookahead,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,raw_char('c')))))))),Raw_quant(Plus,Raw_con(raw_dot,Raw_empty)))),"babcbcaacccbbcccabacaccaaccbabcbbbabbabbbabbcbcaa")]

let clear_mem : (raw_regex*string) list = (* FIXED! clear the lookaround memory in quantifiers *)
  [(Raw_quant(Star,Raw_alt(Raw_con(raw_char('a'),Raw_lookaround(Lookahead,Raw_capture(raw_char('b')))),raw_char('b'))),"abc")]

let empty_problem : (raw_regex*string) list = (* FIXED, by compiling Plus as Concatenation with Star when needed *)
  [(Raw_con(Raw_lookaround(NegLookbehind,raw_char('b')),Raw_quant(Plus,Raw_alt(Raw_empty,Raw_capture(raw_dot)))),"bbccbacbccbcabbcbcaccccba");
   (Raw_quant(Plus,Raw_alt(Raw_empty,Raw_con(Raw_alt(raw_dot,Raw_alt(raw_dot,Raw_empty)),Raw_alt(Raw_lookaround(Lookahead,raw_char('b')),Raw_quant(LazyStar,Raw_capture(Raw_lookaround(Lookbehind,Raw_alt(Raw_capture(Raw_empty),Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_con(Raw_con(raw_dot,Raw_capture(raw_dot)),Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_capture(Raw_quant(LazyStar,Raw_empty))))))))))))))),"bbccaaccbbbaccabccbabccababc");
   (Raw_alt(Raw_quant(Plus,Raw_alt(Raw_empty,raw_dot)),Raw_capture(Raw_alt(raw_dot,raw_char('c')))),"bbccacccabcbaaabbabbbccccccacbaabacbc")]

let double_quant : (raw_regex*string) list = (* FIXED, with another way to compile lazystar *)
  [(Raw_quant(Plus,Raw_alt(Raw_quant(LazyStar,Raw_lookaround(NegLookbehind,Raw_con(Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookbehind,Raw_alt(raw_char('c'),raw_char('c')))),Raw_lookaround(NegLookbehind,Raw_quant(Plus,raw_dot))))),raw_dot)),"abacaaaacabaccbcabcacabccbcaacbabaa");
   (Raw_capture(Raw_quant(Plus,Raw_quant(LazyStar,raw_dot))),"bacababaacbcabaabccccacca");
   (Raw_con(Raw_con(Raw_empty,Raw_capture(Raw_quant(Star,Raw_quant(LazyStar,raw_dot)))),raw_dot),"abcbbca")]

let empty_group : (raw_regex*string) list =
  [(Raw_quant(Star,Raw_alt(Raw_con(raw_char('a'),Raw_capture(Raw_empty)),raw_char('b'))),"ab")]


(* FIXED by preventing advance_epsilon from calling itself twice *)
let should_not_clear : (raw_regex*string) list =
  [(Raw_alt(Raw_con(Raw_quant(LazyStar,Raw_alt(Raw_capture(Raw_con(Raw_quant(Star,Raw_lookaround(Lookahead,Raw_empty)),Raw_empty)),Raw_capture(Raw_empty))),raw_char('a')),Raw_empty),"cbacacaabbcbaacbbababcbcaaa");
   (Raw_alt(Raw_con(Raw_alt(Raw_empty,Raw_lookaround(NegLookbehind,Raw_lookaround(Lookbehind,Raw_quant(LazyPlus,raw_char('b'))))),Raw_lookaround(Lookahead,raw_char('b'))),Raw_quant(LazyStar,raw_char('c'))),"cbcaaaacccabbaacccaacbcbacbbabcaccbbbbc");
   (Raw_con(Raw_capture(Raw_capture(Raw_con(Raw_con(Raw_alt(Raw_capture(Raw_con(Raw_alt(Raw_quant(LazyStar,raw_dot),Raw_capture(Raw_lookaround(NegLookbehind,Raw_capture(Raw_lookaround(NegLookahead,raw_char('c')))))),Raw_capture(raw_char('b')))),Raw_quant(Plus,Raw_con(raw_char('a'),Raw_alt(Raw_con(Raw_lookaround(NegLookbehind,Raw_lookaround(Lookbehind,Raw_capture(raw_dot))),Raw_empty),Raw_con(Raw_capture(Raw_lookaround(NegLookbehind,Raw_quant(LazyStar,raw_dot))),raw_char('a')))))),raw_char('b')),Raw_con(Raw_con(Raw_con(raw_dot,raw_dot),Raw_lookaround(NegLookahead,Raw_capture(raw_char('c')))),Raw_capture(Raw_con(Raw_capture(raw_dot),raw_dot)))))),Raw_alt(Raw_con(Raw_alt(Raw_lookaround(NegLookbehind,Raw_alt(raw_char('a'),Raw_empty)),Raw_quant(LazyPlus,Raw_empty)),Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_lookaround(NegLookahead,Raw_con(Raw_con(Raw_lookaround(Lookahead,raw_char('c')),Raw_lookaround(NegLookbehind,Raw_capture(Raw_alt(Raw_capture(Raw_empty),Raw_lookaround(NegLookbehind,raw_char('b')))))),raw_char('a'))))))),Raw_empty)),"cabababbccaaccbabbaaacbb");
   (Raw_capture(Raw_capture(Raw_quant(Plus,Raw_capture(Raw_con(Raw_con(Raw_quant(LazyPlus,Raw_lookaround(Lookbehind,Raw_lookaround(NegLookbehind,raw_char('b')))),Raw_con(raw_char('c'),Raw_empty)),Raw_capture(Raw_alt(Raw_con(Raw_quant(LazyPlus,Raw_empty),Raw_capture(Raw_capture(Raw_quant(LazyPlus,Raw_empty)))),Raw_capture(raw_dot)))))))),"ccbbbccababcababbbaccabccbacababbacbcababcabbaccccacbccbbbcbbcbcacacbaaacaaababccbbbbbbac")]

(* here we agree with Experimental, but Experimental does not agree with Irregexp! *)
(* The problem here is that we merge threads that may have a different future according to the JS semantics *)
(* this is now fixed, both here and in experimental *)
let empty_repetitions : (raw_regex*string) list =
  [(Raw_quant(Plus,Raw_con(Raw_alt(raw_char('a'),Raw_empty),Raw_quant(LazyStar,Raw_capture(raw_dot)))),"bab");
   (Raw_quant(Star,Raw_con(Raw_alt(raw_char('a'),Raw_empty),Raw_quant(LazyStar,raw_dot))),"ab"); (* simplified example *)
   (Raw_quant(Star,Raw_con(Raw_alt(raw_char('a'),Raw_empty),Raw_alt(Raw_empty,raw_char('b')))),"ab")] (* no lazy stars, just alternation *)

(* FIXED by preventing advance_epsilon from calling itself twice *)
let linear_stuck : (raw_regex*string) list =
  [(Raw_con(Raw_lookaround(Lookbehind,Raw_empty),Raw_capture(Raw_con(raw_dot,Raw_quant(Plus,Raw_capture(Raw_con(Raw_lookaround(NegLookbehind,Raw_quant(LazyPlus,Raw_quant(LazyPlus,Raw_quant(LazyPlus,Raw_capture(Raw_alt(Raw_capture(Raw_alt(Raw_alt(raw_dot,Raw_alt(Raw_lookaround(NegLookbehind,Raw_alt(Raw_empty,Raw_capture(Raw_empty))),Raw_con(Raw_alt(Raw_capture(raw_dot),Raw_capture(raw_char('b'))),Raw_empty))),Raw_quant(LazyStar,Raw_capture(Raw_con(Raw_empty,Raw_alt(Raw_capture(Raw_quant(Plus,Raw_capture(Raw_capture(Raw_capture(Raw_con(Raw_alt(Raw_empty,Raw_empty),Raw_capture(Raw_capture(Raw_con(Raw_capture(Raw_lookaround(Lookahead,Raw_lookaround(NegLookbehind,Raw_empty))),Raw_con(Raw_quant(Star,Raw_capture(Raw_con(Raw_alt(Raw_empty,Raw_capture(Raw_alt(Raw_capture(Raw_quant(LazyPlus,Raw_empty)),Raw_alt(Raw_capture(raw_char('a')),raw_dot)))),Raw_alt(raw_char('c'),Raw_empty)))),raw_char('b'))))))))))),Raw_con(Raw_lookaround(NegLookbehind,Raw_capture(Raw_quant(Star,Raw_lookaround(NegLookbehind,Raw_lookaround(Lookahead,Raw_con(Raw_capture(Raw_lookaround(Lookahead,Raw_lookaround(NegLookbehind,Raw_lookaround(Lookahead,Raw_empty)))),raw_dot)))))),Raw_empty))))))),Raw_lookaround(NegLookahead,Raw_empty))))))),Raw_empty)))))),"cbacaaaababbcaaaaababcabcabaccaaaacb")]

(* bugs when I switched to linear compilation of the nullable + *)
(* Fixed the first 2 by starting the original thread with a true for exit_allowed, otherwise it fails to take empty Plusses *)
let linear_plus : (raw_regex*string) list =
  [(Raw_alt(Raw_lookaround(NegLookbehind,Raw_capture(Raw_capture(Raw_con(Raw_quant(Star,Raw_empty),Raw_capture(Raw_quant(Plus,Raw_alt(Raw_capture(Raw_empty),Raw_lookaround(NegLookbehind,Raw_capture(Raw_capture(Raw_con(Raw_capture(Raw_lookaround(Lookbehind,Raw_alt(Raw_capture(Raw_quant(LazyStar,Raw_alt(Raw_lookaround(Lookbehind,Raw_con(raw_char('c'),raw_dot)),raw_dot))),Raw_empty))),Raw_quant(Star,Raw_lookaround(Lookahead,Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_quant(LazyStar,Raw_quant(LazyPlus,Raw_empty)),Raw_quant(LazyStar,Raw_capture(Raw_capture(Raw_empty)))))))))))))))))))),Raw_capture(Raw_empty)),"bacabacbcabcaacac");
   (Raw_quant(Plus,Raw_con(Raw_capture(Raw_lookaround(Lookbehind,Raw_alt(Raw_con(raw_char('b'),Raw_empty),Raw_capture(Raw_empty)))),Raw_empty)),"bbacaaaaccbcaaccaacaaababacccbcbbbccbccb");
   (Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_alt(raw_dot,Raw_capture(Raw_empty)))),"b"); (* simplified *)
   (Raw_alt(Raw_lookaround(Lookahead,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_con(Raw_lookaround(NegLookahead,Raw_capture(Raw_empty)),Raw_lookaround(Lookahead,Raw_capture(raw_dot)))))),Raw_lookaround(Lookahead,Raw_con(Raw_capture(Raw_alt(Raw_quant(Star,Raw_capture(Raw_con(Raw_alt(Raw_lookaround(Lookahead,raw_char('b')),Raw_lookaround(Lookahead,raw_char('c'))),Raw_capture(Raw_alt(Raw_lookaround(NegLookbehind,Raw_alt(Raw_lookaround(Lookbehind,raw_char('c')),raw_char('c'))),Raw_capture(Raw_con(raw_dot,raw_dot))))))),Raw_con(raw_dot,Raw_alt(raw_dot,raw_char('b'))))),Raw_quant(Star,Raw_con(Raw_quant(Plus,Raw_capture(Raw_lookaround(NegLookbehind,raw_char('b')))),Raw_quant(Star,Raw_con(raw_dot,raw_dot))))))),"accababbbabbccacabcaccaabcbbabcaaacbaaccabacababa")]

(* Fixed: we now reconstruct the empty groups inside the nullable plus *)
let plus_reconstruct : (raw_regex*string) list =
  [(Raw_con(Raw_quant(Plus,Raw_capture(Raw_empty)),raw_char('a')),"a");
   (Raw_quant(Plus,Raw_alt(Raw_con(Raw_lookaround(Lookbehind,Raw_lookaround(NegLookahead,Raw_empty)),Raw_capture(raw_char('b'))),Raw_alt(Raw_alt(Raw_lookaround(Lookahead,Raw_capture(Raw_alt(Raw_empty,raw_dot))),Raw_alt(Raw_lookaround(NegLookbehind,raw_dot),Raw_empty)),raw_char('c')))),"babcaccbbabcacacabcaaaababbccaccccbabbcabccbbbcaacbbababccabacbbbabcbacbaabcbbccbabbccbaa");
   (Raw_quant(LazyPlus,Raw_con(Raw_quant(LazyPlus,Raw_capture(raw_dot)),Raw_con(Raw_alt(Raw_quant(Plus,Raw_capture(Raw_empty)),Raw_quant(Star,Raw_quant(Star,raw_char('c')))),Raw_alt(Raw_lookaround(NegLookbehind,Raw_empty),Raw_alt(Raw_capture(Raw_capture(Raw_empty)),raw_char('c')))))),"abaabaccbcabaccabaacabccabbccacbbccbcbacabaacbaaacbacabbaacabaccabaacbbbbccaccaacaacabccccba");
   (Raw_quant(Plus,Raw_capture(Raw_capture(Raw_capture(Raw_capture(Raw_con(Raw_empty,Raw_empty)))))),"cccccacaccbccabbcabacbaacacabcacbbabcbcccacbcab");
   (Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_empty,Raw_quant(Plus,Raw_capture(Raw_empty))))),"a");
   (Raw_quant(Plus,Raw_capture(Raw_quant(LazyStar,Raw_empty))),"ccbcbbbbcacbcabbccaaccaccaacacabaacbbbcccbbaabaabccbacccac")]

(* more cin examples  *)
let cin_examples : (raw_regex*string) list =
  [(Raw_con(Raw_quant(Plus,Raw_alt(Raw_lookaround(Lookahead,Raw_capture(Raw_empty)),Raw_alt(Raw_empty,raw_char('a')))),raw_char('b')),"b");
   (Raw_con(Raw_quant(Star,Raw_quant(Plus,Raw_alt(Raw_lookaround(Lookahead,Raw_capture(Raw_empty)),Raw_alt(Raw_empty,raw_char('a'))))),raw_char('b')),"ab");
   (Raw_con(Raw_quant(Star,Raw_con(Raw_quant(Plus,Raw_alt(Raw_lookaround(Lookahead,Raw_capture(Raw_empty)),Raw_alt(Raw_empty,raw_char('a')))),raw_char('c'))),raw_char('b')),"accb")]

(* FIXED. bugs when I did not construct the CDN table yet *)
let cdn_empty: (raw_regex*string) list =
  [(Raw_quant(Plus,Raw_con(Raw_empty,Raw_lookaround(Lookbehind,Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_quant(Plus,raw_char('c'))))))),"bbaaaacaabaabbbaaacacaacbccbacaacaaaaaabccbbbbbcbcccacabbbaabccccacccabacccacbcbbaacbccbcaacb");
   (Raw_quant(Plus,Raw_lookaround(Lookbehind,raw_char('c'))),"ababaaaacbcaababaaccaccacbbbccabbabbcacaa");
   (Raw_quant(Plus,Raw_lookaround(Lookbehind,raw_char('b'))),"baabacaccabacabcaaacbbbabacaabbcbbcbaccccaacbacbbababcccbcccacbabababcabbaabacbbbbcaaa");
   (Raw_lookaround(Lookbehind,Raw_alt(Raw_lookaround(Lookahead,Raw_lookaround(NegLookbehind,Raw_alt(raw_dot,Raw_empty))),Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_quant(Plus,Raw_lookaround(Lookahead,Raw_quant(Plus,Raw_empty))))))),"babbccbbabacccabbabccccabacaacacbacabcaaccabbabccaca");
   (Raw_lookaround(NegLookahead,Raw_quant(Plus,Raw_lookaround(Lookahead,Raw_empty))),"b")]

(* fails the assertion "expected a nullable plus" *)
(* FIXED, when we don't forget to build a CDN table when reconstructing the + groups *)
let nullable_expected: (raw_regex*string) list =
  [(Raw_capture(Raw_con(Raw_capture(Raw_quant(Plus,Raw_capture(Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,raw_char('b'))))))),Raw_lookaround(Lookahead,Raw_lookaround(Lookbehind,raw_dot)))),"caabcbbcbcbcbacacaacbaccabaabbabbcbbbccbbbccac");
   (Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(NegLookbehind,raw_char('b')))),"cb");
   (Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_empty)))),"caccaacbaabaaabaaacccaacacbbcabbababbacabbabcacabbcaabcbcaaacbabbcccbbcbbbcabbcaccacbacbb");
   (Raw_con(Raw_lookaround(Lookbehind,Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(Lookahead,Raw_alt(Raw_capture(Raw_capture(Raw_lookaround(NegLookahead,raw_char('c')))),Raw_alt(raw_dot,Raw_quant(LazyPlus,Raw_capture(Raw_quant(LazyStar,Raw_lookaround(Lookbehind,raw_char('b'))))))))))),Raw_con(Raw_quant(Plus,Raw_lookaround(Lookahead,Raw_empty)),raw_char('b'))),"ababcbcaccbbcbbacccbcbccbbabaaaabbbbbabacacbccbabcbbbbabaacabaccabb");
   (Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_capture(raw_dot)))),"aaccbcbccccccbbbccccbcbaabbaccbcccaabbacacccabbccaabbcabb");
   (Raw_lookaround(Lookbehind,Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_con(raw_char('a'),Raw_con(raw_char('b'),raw_char('b'))))))),"bcaaabccbaabccaaaaababbaaaaaaaaaacbabacabcbbcbbaaabcbbaccabccbcacacc")]

(* testing the CDN formulas *)
let cdn_formulas: (raw_regex*string) list =
  [(Raw_quant(Plus,Raw_alt(Raw_quant(Plus,Raw_lookaround(Lookahead,raw_char('a'))),Raw_con(Raw_lookaround(Lookahead,raw_char('b')),Raw_lookaround(Lookahead,raw_char('c'))))),"abc")]

(* fixed by ensuring that the context is the right one when reconstructing lookaround groups *)
let anchor_context: (raw_regex*string) list =
  [(Raw_lookaround(Lookahead,Raw_anchor(NonWordBoundary)),"cac b");
   (Raw_alt(Raw_lookaround(Lookbehind,Raw_quant(LazyPlus,Raw_lookaround(Lookahead,Raw_anchor(BeginInput)))),Raw_con(raw_dot,Raw_lookaround(NegLookahead,Raw_capture(Raw_lookaround(Lookahead,Raw_anchor(BeginInput))))))
   ,"a c b")]

(* fixed by evaluating the CDN formulas in the current context *)
let anchor_cdn: (raw_regex*string) list =
  [(Raw_con(Raw_empty,Raw_quant(Plus,Raw_anchor(BeginInput))),"b  caacaac bcaabc bbcbbaa a  cacccb ab aacccbbb aabbb c bccbcaaaabaa");
   (Raw_quant(Plus,Raw_anchor(BeginInput))," cc c  a b acabbbaaaac babaabac bbca bba cccca cacaaabac  ")]

(* fixed: beginning and end of string are not word boundaries if the character next to it is not an ascii character *)
let anchor_mismatch: (raw_regex*string) list =
  [(Raw_con(Raw_capture(Raw_con(Raw_anchor(WordBoundary),Raw_anchor(WordBoundary))),raw_dot),"  cab bccbaac baabab bcbbaca cabca  c");
   (Raw_con(Raw_con(Raw_con(Raw_alt(Raw_capture(Raw_capture(Raw_empty)),Raw_alt(Raw_con(Raw_capture(raw_dot),Raw_capture(Raw_alt(Raw_capture(Raw_capture(Raw_capture(raw_char('a')))),Raw_capture(Raw_anchor(BeginInput))))),Raw_capture(Raw_quant(Star,Raw_capture(Raw_lookaround(NegLookbehind,raw_char('c'))))))),Raw_quant(LazyStar,raw_char(' '))),Raw_quant(LazyStar,raw_dot)),Raw_alt(Raw_alt(Raw_lookaround(NegLookahead,Raw_anchor(WordBoundary)),Raw_lookaround(NegLookahead,Raw_empty)),raw_char('a')))," abccb ab bb abcaaaa cca cbb a aaccbcab");
   (Raw_alt(Raw_alt(Raw_alt(Raw_capture(Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_con(raw_dot,Raw_capture(Raw_capture(raw_dot)))))),raw_char('a')),Raw_capture(Raw_capture(Raw_con(Raw_capture(Raw_anchor(WordBoundary)),Raw_quant(Star,Raw_lookaround(Lookahead,Raw_empty)))))),Raw_con(Raw_capture(Raw_empty),Raw_anchor(BeginInput)))," a  b bbc ");
   (Raw_alt(raw_char('c'),Raw_lookaround(Lookbehind,Raw_con(Raw_anchor(BeginInput),Raw_lookaround(NegLookbehind,Raw_capture(Raw_anchor(WordBoundary))))))," c  cc bcac c  a a  a  ababab  acbca  cbcb b cccccc cc  a  b  bcbaaaaaaa baaa");
   (Raw_con(Raw_lookaround(NegLookahead,Raw_capture(Raw_capture(Raw_anchor(NonWordBoundary)))),raw_dot)," cc cccc aabaaaa ba abcaabcbbb cacccc  bccbcb cababbabccac ")]

(* fixed, by making sure the forks in [repeat_optional] point to the correct next instruction *)
let counted_oob: (raw_regex*string) list =
  [(Raw_alt(Raw_capture(Raw_alt(Raw_anchor(WordBoundary),Raw_capture(raw_char('b')))),Raw_count({min=5;max=Some 9;greedy=true},raw_char('a'))),"ab--bbaabab-aab-b-a-aa-b-baa-bab-ba-ab-a--b-ba-a-ab-b--abbbbb-aabbbbba-b-aa---aa-");
   (Raw_alt(Raw_lookaround(NegLookbehind,Raw_lookaround(Lookbehind,raw_dot)),Raw_capture(Raw_count({min=8;max=Some 12;greedy=true},Raw_lookaround(Lookbehind,Raw_empty)))),"ab-a-aa-a-bb-baaba-a-aabbabaabb-b-aaabaa-ba-");
   (Raw_count({min=3;max=Some 12;greedy=true},Raw_lookaround(Lookbehind,Raw_alt(Raw_quant(LazyPlus,raw_dot),Raw_alt(Raw_count({min=4;max=None;greedy=true},Raw_anchor(BeginInput)),Raw_con(Raw_lookaround(Lookbehind,Raw_alt(Raw_con(Raw_count({min=8;max=None;greedy=true},Raw_con(Raw_capture(raw_char('a')),Raw_alt(Raw_capture(Raw_count({min=5;max=Some 5;greedy=false},Raw_quant(Plus,Raw_alt(Raw_anchor(WordBoundary),Raw_lookaround(NegLookahead,Raw_lookaround(NegLookahead,Raw_empty)))))),Raw_capture(Raw_empty)))),Raw_empty),raw_char('-'))),Raw_anchor(WordBoundary)))))),"a");
   (Raw_lookaround(Lookbehind,Raw_alt(Raw_quant(Plus,Raw_anchor(BeginInput)),Raw_quant(Plus,raw_char('a')))),"a");
   (Raw_lookaround(Lookahead,Raw_alt(Raw_count({min=1;max=Some 3;greedy=true},raw_dot),Raw_empty)),"a-abb-bbbb----bbaa--aabb-abaab---b-bab-b--ba--a--bb-babb-b");
   (Raw_count({min=4;max=Some 8;greedy=false},raw_dot),"abbb--a-ab-aa--ba--bb-aaa")]

(* fixed, by copying the registers correctly *)
let regs_mismatch: (raw_regex*string) list =
  [(Raw_alt(Raw_count({min=3;max=None;greedy=true},Raw_alt(Raw_con(Raw_empty,Raw_capture(Raw_alt(Raw_alt(Raw_quant(Star,raw_dot),raw_dot),Raw_capture(Raw_anchor(WordBoundary))))),Raw_quant(LazyStar,Raw_count({min=7;max=None;greedy=true},Raw_empty)))),Raw_count({min=8;max=Some 15;greedy=true},Raw_lookaround(Lookahead,Raw_anchor(EndInput)))),"a");
   (Raw_quant(Plus,Raw_capture(raw_char('a'))),"ab--bbb---ba-b-b--abababbaabab--b-")]

(* fixed, by updating the regs reference when reconstructing *)
let regs_oob: (raw_regex*string) list =
  [(Raw_con(Raw_capture(Raw_quant(Star,Raw_capture(Raw_capture(Raw_lookaround(Lookahead,raw_char('b')))))),Raw_capture(Raw_anchor(BeginInput))),"-baba-aa-bb-a-bba");
   (Raw_capture(Raw_con(Raw_capture(Raw_alt(Raw_alt(Raw_count({min=9;max=Some 13;greedy=true},Raw_capture(Raw_count({min=3;max=Some 7;greedy=false},Raw_capture(Raw_lookaround(Lookahead,raw_dot))))),Raw_con(raw_char('b'),Raw_empty)),Raw_con(raw_dot,Raw_con(raw_char('b'),Raw_con(Raw_count({min=2;max=Some 10;greedy=false},Raw_quant(LazyQuestionMark,Raw_lookaround(Lookbehind,Raw_con(Raw_lookaround(Lookahead,raw_char('a')),Raw_capture(Raw_lookaround(NegLookahead,Raw_empty)))))),Raw_anchor(NonWordBoundary)))))),Raw_capture(Raw_count({min=4;max=Some 10;greedy=true},Raw_quant(QuestionMark,Raw_anchor(NonWordBoundary)))))),"--a-bb-aa--a-a");
   (Raw_capture(Raw_alt(Raw_lookaround(Lookbehind,Raw_count({min=9;max=None;greedy=true},Raw_lookaround(Lookahead,Raw_alt(Raw_capture(Raw_capture(Raw_empty)),Raw_lookaround(NegLookbehind,raw_dot))))),Raw_capture(Raw_capture(Raw_empty)))),"----bb-a-ab");
   (Raw_capture(Raw_capture(Raw_anchor(BeginInput))),"--aa---a-a----baabb--b-aa--aabba-bb-aa-b---aba-a-abbaab-b-")]

(* fixed, by updating the regs reference when reconstructing *)
let start_no_end: (raw_regex*string) list =
  [(Raw_alt(Raw_anchor(EndInput),Raw_quant(LazyPlus,Raw_capture(Raw_lookaround(NegLookahead,Raw_con(Raw_capture(Raw_anchor(WordBoundary)),raw_dot))))),"b");
   (Raw_alt(raw_char('-'),Raw_capture(Raw_con(Raw_lookaround(Lookbehind,Raw_empty),Raw_lookaround(NegLookahead,raw_dot)))),"babb-bab-aabb-bb-abbabababaaa-abaab--b-bbb-b-")]

(* fixed by only calling next_char when we are not at the maximum character 255 *)
let class_next_char: (raw_regex*string) list =
  [(raw_class([CGroup(NonWord);CChar(char_of_int(45));CChar(char_of_int(98));CRange(char_of_int(168),char_of_int(246));CGroup(Word);CChar(char_of_int(97))]),"a")]

(* fixed by fixing the range negation *)
let class_negation: (raw_regex*string) list =
  [(Raw_capture(raw_neg_class([CRange(char_of_int(102),char_of_int(183));CRange(char_of_int(85),char_of_int(231));CChar(char_of_int(97));CGroup(NonWord);CRange(char_of_int(149),char_of_int(251));CChar(char_of_int(45));CGroup(Digit);CChar(char_of_int(98));CChar(char_of_int(98));CGroup(NonWord);CGroup(NonWord);CRange(char_of_int(151),char_of_int(245));CGroup(NonWord);CRange(char_of_int(198),char_of_int(213));CChar(char_of_int(45))])),"b");
   (raw_neg_class([CGroup(NonWord)]),"b")] (* simplified *)

(* fixed by escaping - and ^ inside character classes *)
let class_escape: (raw_regex*string) list =
  [(raw_class([CChar(char_of_int(98));CChar(char_of_int(45));CChar(char_of_int(98));CRange(char_of_int(243),char_of_int(244));CGroup(Word);CGroup(Space)]),"-baabbbb--ab-aab-baba-bab-a-aaaaa-baa--aaaaa-ab--a-baabb--abb-bb-aab--")]

(* fixed by exporting the NULL character (char code 0) to \x00 *)
(* and escaping every non-word character *)
let js_export_bug: (raw_regex*string) list =
  [(Raw_alt(Raw_lookaround(Lookahead,raw_group(Digit)),Raw_lookaround(Lookahead,Raw_capture(raw_neg_class([CGroup(Space);CGroup(NonWord);CRange(char_of_int(12),char_of_int(233));CRange(char_of_int(255),char_of_int(255));CRange(char_of_int(43),char_of_int(212));CRange(char_of_int(229),char_of_int(249));CGroup(Space);CRange(char_of_int(27),char_of_int(254));CGroup(Digit);CRange(char_of_int(0),char_of_int(151));CChar(char_of_int(98));CGroup(Word);CRange(char_of_int(129),char_of_int(207));CChar(char_of_int(98));CChar(char_of_int(97));CGroup(Word)])))),"a");
   (raw_neg_class([CRange(char_of_int(0),char_of_int(151))]),"a"); (* simplified *)
   (raw_neg_class([CRange(char_of_int(92),char_of_int(238));CChar(char_of_int(97));CChar(char_of_int(45));CRange(char_of_int(118),char_of_int(193));CChar(char_of_int(97));CGroup(NonWord)]),"a-a-aba---aaabbb--a-baabbabbaa-a-aab--bba");
   (raw_neg_class([CRange(char_of_int(92),char_of_int(120))]),"b"); (* simplified *)
   (raw_class([CGroup(Digit);CRange(char_of_int(55),char_of_int(195));CRange(char_of_int(239),char_of_int(254));CRange(char_of_int(80),char_of_int(166));CRange(char_of_int(109),char_of_int(250));CGroup(Digit);CChar(char_of_int(45));CChar(char_of_int(97));CGroup(NonSpace);CGroup(Digit);CRange(char_of_int(190),char_of_int(196));CRange(char_of_int(83),char_of_int(89));CGroup(Digit);CRange(char_of_int(43),char_of_int(64));CRange(char_of_int(7),char_of_int(93));CRange(char_of_int(2),char_of_int(205));CRange(char_of_int(146),char_of_int(166));CChar(char_of_int(98));CRange(char_of_int(195),char_of_int(205))]),"-b---abba-baab----a-abaabb----aa--b--b-abba-a-a--b-b-bb-a--aa-bbbaaab--aaaaaab");
   (raw_class([CGroup(NonSpace);CRange(char_of_int(7),char_of_int(93))]),"-");
   (raw_class([CChar(char_of_int(98));CRange(char_of_int(242),char_of_int(252));CRange(char_of_int(39),char_of_int(223));CRange(char_of_int(200),char_of_int(239));CGroup(Digit);CRange(char_of_int(234),char_of_int(242));CRange(char_of_int(230),char_of_int(254));CGroup(NonDigit);CGroup(Space);CGroup(Digit)]),"-babbababb-b--aaaaa-aaa-bab-b--")]

(* fixed. I called non-compiled code *)
let empty_bytecode: (raw_regex*string) list =
  [(Raw_capture(Raw_lookaround(Lookahead,Raw_quant(Plus,Raw_count({min=9;max=Some 10;greedy=false},Raw_anchor(NonWordBoundary))))),"-b-a--b-bababbaab-ba-aa--bb-a-bb-aaab-aababba---a-b-bbb-ab--ba-a-a-bba-ba-a--ba-ababb--baab-b--ba");
   (Raw_capture(Raw_lookaround(Lookahead,Raw_quant(Plus,Raw_count({min=1;max=None;greedy=false},Raw_anchor(NonWordBoundary))))),"-b")]

(* fixed! don't forget SetQuantToClock in the default CIN ReconstructNulled case *)
(* and don't forget to count the number of instructions so that instructions don't overlap *)
let cin_clock_mismatch: (raw_regex*string) list=
  [(Raw_count({min=9;max=None;greedy=true},Raw_count({min=9;max=Some 13;greedy=true},Raw_alt(Raw_character(Char('a')),Raw_capture(Raw_alt(Raw_anchor(NonWordBoundary),Raw_empty))))),"aabaaaabbaabb-aa-a-a-baba-bb-baba-a-abbaabaa");
   (Raw_count({min=2;max=None;greedy=true},Raw_count({min=2;max=Some 2;greedy=true},Raw_capture(Raw_empty))),"a");
   (Raw_count({min=2;max=None;greedy=true},Raw_count({min=2;max=Some 2;greedy=true},Raw_alt(Raw_character(Char('a')),Raw_capture(Raw_empty)))),"a")]

(* shows that we cannot use the standard Plus construction without duplication for lazy CIN/CDN plus *)
let lazy_cin: (raw_regex*string) list =
  [(Raw_con(Raw_quant(LazyPlus,Raw_capture(Raw_quant(LazyStar,Raw_quant(LazyPlus,Raw_quant(LazyPlus,Raw_quant(LazyPlus,Raw_quant(QuestionMark,Raw_quant(LazyPlus,Raw_character(Dot))))))))),Raw_character(Char('a'))),"b--b-ab-bb--bbbaaa--bbabab-aab-a-bb-a-abbb-bbbaa--bbabb--ba--bbab--aabb---ab-----bbb-bbbba--");
  (Raw_con(Raw_quant(LazyPlus,Raw_capture(Raw_quant(LazyStar,Raw_character(Dot)))),Raw_character(Char('a'))),"b-a")]

(* JS is stuck (timeout), but not our engine *)
(* I quickly stopped listing these, I found too many *)
let redos : (raw_regex*string) list =
  [(Raw_lookaround(Lookbehind,Raw_con(Raw_lookaround(NegLookahead,raw_dot),Raw_quant(LazyPlus,Raw_capture(Raw_con(Raw_quant(Star,raw_char('a')),Raw_con(Raw_alt(Raw_empty,raw_dot),raw_dot)))))),"cbabbccccbbcccaaaaaccabccbaabaabcaaacbca");
   (Raw_con(Raw_con(Raw_lookaround(Lookahead,Raw_capture(Raw_capture(raw_dot))),Raw_capture(Raw_alt(Raw_lookaround(Lookahead,raw_dot),Raw_alt(Raw_con(Raw_capture(Raw_quant(Star,raw_char('c'))),Raw_capture(raw_char('a'))),Raw_alt(Raw_capture(Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(Lookahead,Raw_empty)))),Raw_capture(Raw_alt(Raw_alt(Raw_lookaround(NegLookbehind,Raw_quant(LazyPlus,Raw_capture(Raw_alt(Raw_con(Raw_capture(raw_dot),Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_capture(Raw_capture(Raw_alt(raw_char('a'),Raw_quant(LazyPlus,Raw_con(Raw_lookaround(NegLookbehind,Raw_quant(LazyPlus,Raw_lookaround(Lookahead,Raw_empty))),Raw_quant(Plus,Raw_alt(raw_dot,Raw_capture(Raw_capture(raw_dot))))))))),Raw_empty)))),Raw_lookaround(Lookahead,raw_char('c')))))),raw_dot),raw_dot))))))),Raw_con(Raw_alt(Raw_con(Raw_alt(Raw_capture(Raw_quant(LazyStar,Raw_con(Raw_capture(raw_char('b')),Raw_alt(Raw_empty,raw_char('c'))))),raw_dot),raw_dot),Raw_lookaround(NegLookbehind,Raw_lookaround(Lookbehind,Raw_lookaround(NegLookbehind,Raw_empty)))),Raw_lookaround(NegLookahead,Raw_alt(Raw_alt(Raw_capture(Raw_capture(Raw_empty)),Raw_lookaround(Lookbehind,Raw_empty)),Raw_lookaround(Lookahead,Raw_empty))))),"ccaacababbbcccbbbabcbbbccaaabccacbcb");
   (Raw_capture(Raw_alt(Raw_alt(Raw_con(Raw_alt(Raw_lookaround(NegLookbehind,Raw_con(Raw_con(Raw_alt(Raw_quant(Star,Raw_lookaround(Lookahead,Raw_capture(Raw_lookaround(Lookahead,Raw_capture(Raw_lookaround(Lookbehind,Raw_capture(Raw_empty))))))),Raw_empty),Raw_alt(raw_char('a'),Raw_capture(Raw_quant(LazyPlus,Raw_lookaround(Lookahead,Raw_con(Raw_con(Raw_capture(Raw_alt(Raw_quant(Plus,Raw_capture(Raw_capture(Raw_capture(raw_char('a'))))),raw_char('b'))),Raw_empty),Raw_lookaround(Lookbehind,Raw_con(Raw_con(Raw_alt(Raw_capture(Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_lookaround(NegLookbehind,raw_dot),Raw_empty)))),Raw_quant(LazyPlus,Raw_con(raw_dot,Raw_quant(Star,Raw_alt(Raw_con(Raw_lookaround(NegLookahead,Raw_lookaround(Lookbehind,raw_char('a'))),Raw_capture(raw_dot)),Raw_capture(Raw_con(Raw_lookaround(NegLookahead,Raw_quant(LazyStar,Raw_empty)),Raw_capture(raw_char('c'))))))))),raw_char('b')),Raw_empty)))))))),Raw_lookaround(NegLookahead,Raw_con(Raw_capture(raw_char('a')),Raw_con(Raw_empty,Raw_lookaround(Lookahead,Raw_quant(Star,raw_char('c')))))))),Raw_alt(Raw_con(Raw_con(Raw_alt(Raw_lookaround(Lookbehind,Raw_capture(Raw_lookaround(Lookbehind,raw_dot))),Raw_con(Raw_con(Raw_capture(Raw_lookaround(NegLookbehind,raw_char('b'))),Raw_quant(LazyPlus,Raw_quant(Star,Raw_quant(LazyStar,Raw_alt(Raw_capture(Raw_capture(Raw_capture(Raw_capture(raw_dot)))),raw_dot))))),Raw_capture(Raw_capture(raw_char('c'))))),Raw_con(Raw_lookaround(NegLookahead,Raw_quant(Plus,Raw_capture(raw_char('b')))),Raw_capture(Raw_alt(Raw_con(raw_dot,Raw_quant(Star,raw_char('b'))),Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_lookaround(NegLookbehind,Raw_con(Raw_lookaround(NegLookahead,Raw_quant(Plus,raw_char('c'))),raw_dot)))))))),Raw_quant(Plus,Raw_empty)),Raw_quant(Plus,Raw_empty))),Raw_lookaround(Lookahead,Raw_capture(Raw_lookaround(NegLookbehind,Raw_empty)))),raw_char('b')),raw_char('b'))),"acccbcabbacbccbccbbcbbaa");
   (Raw_con(Raw_alt(raw_char('a'),Raw_con(Raw_quant(LazyStar,Raw_capture(Raw_quant(Star,Raw_alt(raw_dot,Raw_quant(Plus,Raw_empty))))),Raw_alt(raw_char('b'),Raw_lookaround(NegLookahead,Raw_empty)))),Raw_con(Raw_lookaround(NegLookahead,Raw_capture(raw_dot)),Raw_capture(Raw_capture(Raw_capture(raw_dot))))),"cabbcaacbaccccababcbcccbababacbcccabbaaacacacbcacccaaacbbccabaabbaacbcbcacaaacabaacaaaa")]

(** * Paper Examples  *)
(* Everytime, in the paper, there is a pair of a regex and a string, we put it in this list *)
(* we then check that on these regexes/string inputs, our Ocaml engine has the same result as Irregexp *)
let paper_tests : (raw_regex*string) list =
  [(Raw_con(Raw_quant(Star,raw_char('a')),raw_char('b')),"caabd"); (* (a* )b *)
   (Raw_capture(Raw_alt(raw_char('a'),Raw_quant(Star,raw_char('a')))),"aa"); (* (a|a* ) *)
   (Raw_con(Raw_lookaround(Lookbehind,raw_char('L')),raw_char('1')),"L1.2"); (* (?<=L)1 *)
   (Raw_con(Raw_lookaround(Lookbehind,raw_char('L')),raw_char('1')),"v1.2"); (* (?<=L)1 *)
   (Raw_con(Raw_lookaround(Lookbehind,Raw_con(Raw_con(Raw_con(Raw_character(Char('P')),Raw_character(Char('L'))),Raw_character(Char('D'))),Raw_character(Char('I')))),Raw_count({min=2;max=Some 4;greedy=true},Raw_character(Class([CRange(char_of_int(48),char_of_int(57))])))),"PLDI2024"); (* (?<=PLDI)[0-9]{2,4} *)
   (Raw_con(Raw_capture(Raw_alt(Raw_character(Char('a')),Raw_character(Dot))),Raw_character(Char('b'))),"ab"); (* (a|.)b *)
   (Raw_con(Raw_quant(Star,Raw_capture(Raw_quant(Plus,Raw_character(Char('a'))))),Raw_character(Char('b'))),"aaa"); (* (a+)*b *)
   (Raw_lookaround(Lookahead,Raw_capture(Raw_character(Char('c')))),"c"); (* (?=(c)) *)
   (Raw_quant(Star,Raw_capture(Raw_alt(Raw_capture(Raw_character(Char('a'))),Raw_capture(Raw_character(Char('b')))))),"ab");                     (* ((a)|(b))* *)
   (Raw_quant(Star,Raw_con(Raw_alt(Raw_character(Char('a')),Raw_empty),Raw_alt(Raw_empty,Raw_character(Char('b'))))),"ab"); (* (?:(?:a|)(?:|b))* *)
   (Raw_count({min=0;max=Some 7;greedy=true},Raw_con(Raw_alt(Raw_character(Char('a')),Raw_empty),Raw_alt(Raw_empty,Raw_character(Char('b'))))),"ab"); (* (?:(?:a|)(?:|b)){0,7} *)
   (Raw_quant(Star,Raw_alt(Raw_quant(Star,Raw_alt(Raw_capture(Raw_character(Char('a'))),Raw_character(Char('b')))),Raw_character(Char('c')))),"ac"); (* (?:(?:(a)|b)*|c)* *)
   (Raw_quant(Star,Raw_alt(Raw_quant(Star,Raw_capture(Raw_character(Char('a')))),Raw_quant(Star,Raw_alt(Raw_capture(Raw_character(Char('b'))),Raw_capture(Raw_character(Char('c'))))))),"abc"); (* (?:(a)*|(?:(b)|(c))* )* *)
   (Raw_con(Raw_con(Raw_con(Raw_character(Char('a')),Raw_character(Char('b'))),Raw_character(Char('c'))),Raw_lookaround(Lookbehind,Raw_con(Raw_con(Raw_con(Raw_character(Char('a')),Raw_character(Char('b'))),Raw_lookaround(Lookbehind,Raw_character(Char('b')))),Raw_character(Char('c'))))),"abc"); (* abc(?<=ab(?<=b)c) *)
   (Raw_quant(Plus,Raw_alt(Raw_empty,Raw_character(Dot))),"a"); (* (?:|.)+ *)
   (Raw_con(Raw_quant(Star,Raw_quant(Star,Raw_character(Char('a')))),Raw_lookaround(Lookahead,Raw_character(Char('b')))),"aaaaaaaaaaaaaa") (* (?:a* )*(?=b) *)
  ]

(* re-checking a list of previous bugs *)
let replay_bugs (l:(raw_regex*string) list) =
  List.iter (fun (raw,str) -> ignore(CMP.compare_engines raw str)) l

(* just checking that our engine is not stuck on the REDOS regexes *)
let replay_stuck (l:(raw_regex*string) list) =
  List.iter (fun (raw,str) -> ignore(Interpreter.full_match raw str)) l


(** * Running tests  *)

let tests () =
  Printf.printf "\027[32mTests: \027[0m\n\n";
  oracle_tests();
  regex_tests();
  bytecode_tests();
  compiler_tests();
  interpreter_tests();
  build_oracle_tests();
  full_algo_tests();
  compare_engines_tests();
  replay_bugs(string_sub_errors);
  replay_bugs(oracle_assert_errors);
  replay_bugs(expected_result_oracle_errors);
  replay_bugs(idk);
  replay_bugs(clear_mem);
  replay_bugs(empty_problem);
  replay_bugs(double_quant);
  replay_bugs(empty_group);
  replay_bugs(should_not_clear);
  replay_bugs(empty_repetitions);
  replay_bugs(linear_stuck);
  replay_bugs(linear_plus);
  replay_bugs(plus_reconstruct);
  replay_bugs(cin_examples);
  replay_bugs(cdn_empty);
  replay_bugs(nullable_expected);
  replay_bugs(cdn_formulas);
  replay_bugs(anchor_context);
  replay_bugs(anchor_cdn);
  replay_bugs(anchor_mismatch);
  replay_bugs(counted_oob);
  replay_bugs(regs_mismatch);
  replay_bugs(regs_oob);
  replay_bugs(start_no_end);
  replay_bugs(class_next_char);
  replay_bugs(class_negation);
  replay_bugs(class_escape);
  replay_bugs(js_export_bug);
  replay_bugs(empty_bytecode);
  replay_bugs(cin_clock_mismatch);
  replay_bugs(lazy_cin);
  replay_stuck(redos);
  replay_bugs(paper_tests);
  Printf.printf "\027[32mTests passed\027[0m\n"


end

(* Running all tests *)
let main =

  verbose := false;
  debug := false;

  (* testing for all register implementations *)
  let module T1 = PlayTests(Regs.Array_Regs) in
  T1.tests();

  let module T2 = PlayTests(Regs.List_Regs) in
  T2.tests();

  let module T3 = PlayTests(Regs.Map_Regs) in
  T3.tests()
