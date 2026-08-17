(* Stats about the regex corpora *)


open Regex
open Regex_parser
open Regex_lexer
open Yojson
open Yojson.Basic.Util

(** * Gathering Statistics  *)
let rec has_groups (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_groups r1 || has_groups r2
  | Raw_quant (_,r1) | Raw_count (_,r1) | Raw_lookaround (_,r1) -> has_groups r1
  | Raw_capture _ -> true

(* detects regexes with a nullable quantifiers *)
(* that could be vulnerable to the semantic bug we fixed *)
let rec has_nullable (r:regex) : bool =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> false
  | Re_capture (_,r1) | Re_lookaround (_,_,r1) -> has_nullable r1
  | Re_con(r1,r2) | Re_alt (r1,r2) -> has_nullable r1 || has_nullable r2
  | Re_quant (nul,_,_,r1) ->
     has_nullable r1 || nul != NonNullable

let has_nullable_quant (raw:raw_regex) : bool =
  let r = annotate raw in
  has_nullable r

(* detects regexes with capture groups inside quantifiers *)
let rec groups_in_quant (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> groups_in_quant r1 || groups_in_quant r2
  | Raw_capture r1 | Raw_lookaround (_,r1) -> groups_in_quant r1
  | Raw_quant(_,r1) | Raw_count (_,r1) -> has_groups r1

(* detects regexes with lookarounds *)
let rec has_lookaround (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_lookaround r1 || has_lookaround r2
  | Raw_capture r1 | Raw_quant(_,r1) | Raw_count (_,r1) -> has_lookaround r1
  | Raw_lookaround _ -> true

(* detects regexes with quantifiers where the body is non-nullable and min>0 *)
(* these are regexes where our v8 fix without duplication is useful *)
let rec has_nn (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_nn r1 || has_nn r2
  | Raw_capture r1 | Raw_lookaround (_,r1) -> has_nn r1
  | Raw_quant (q,r1) ->
     begin match q with
     | Plus | LazyPlus -> has_nn r1 || raw_nullable r1 = NonNullable
     | _ -> has_nn r1
     end
  | Raw_count (q,r1) ->
     has_nn r1 || (raw_nullable r1 = NonNullable && q.min > 0 && q.max=None)

(* detects regexes with CIN or CDN+ *)
(* regexes with a quantifier with nullable body and min>0 *)
(* but not the lazy ones, that we don't know how to handle linearly *)
let rec has_nullplus (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_nullplus r1 || has_nullplus r2
  | Raw_capture r1 | Raw_lookaround (_,r1) -> has_nullplus r1
  | Raw_quant (q,r1) ->
     begin match q with
     | Plus -> has_nullplus r1 || raw_nullable r1 = CINullable || raw_nullable r1 = CDNullable
     | _ -> has_nullplus r1
     end
  | Raw_count (q,r1) ->
     has_nullplus r1 || (raw_nullable r1 <> NonNullable && q.min > 0 && q.greedy)

(* same thing for the lazy nullable +?, that we don't know how to handle linearly *)
let rec has_lazy_nullplus (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_lazy_nullplus r1 || has_lazy_nullplus r2
  | Raw_capture r1 | Raw_lookaround (_,r1) -> has_lazy_nullplus r1
  | Raw_quant (q,r1) ->
     begin match q with
     | LazyPlus -> has_lazy_nullplus r1 || raw_nullable r1 = CINullable || raw_nullable r1 = CDNullable
     | _ -> has_lazy_nullplus r1
     end
  | Raw_count (q,r1) ->
     has_lazy_nullplus r1 || (raw_nullable r1 <> NonNullable && q.min > 0 && not(q.greedy))

(* whether the regex potentially uses captures just for grouping *)
(* there is of course no way to tell for certain what the intention was *)
let rec has_potentially_capture_just_for_grouping (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_potentially_capture_just_for_grouping r1 || has_potentially_capture_just_for_grouping r2
  | Raw_capture r1 ->
      begin match r1 with
      | Raw_alt (_,_) -> true (* capturing an alternation? probably used for grouping *)
      | _ -> has_potentially_capture_just_for_grouping r1
      end
  | Raw_quant (_,r1) | Raw_count (_,r1) ->
      begin match r1 with
      | Raw_capture _ -> true (* quantifying a capture? probably used for grouping *)
      | _ -> has_potentially_capture_just_for_grouping r1
      end
  | Raw_lookaround (_,r1) -> has_potentially_capture_just_for_grouping r1

(* has dot lazy star *)
let rec has_dls (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_dls r1 || has_dls r2
  | Raw_capture r1 -> has_dls r1
  | Raw_lookaround (_,r1) -> false
  | Raw_quant (q,r1) ->
     begin match r1 with
     | Raw_character Dot ->
        begin match q with
        | LazyStar -> true
        | _ -> has_dls r1
        end
     | _ -> has_dls r1
     end
  | Raw_count (q,r1) ->
     begin match r1 with
     | Raw_character Dot ->
        (q.min = 0 && q.max = None && q.greedy = false) || has_dls r1
     | _ -> has_dls r1
     end

(* has dot greedy star *)
let rec has_dgs (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_dgs r1 || has_dgs r2
  | Raw_capture r1 -> has_dgs r1
  | Raw_lookaround (_,r1) -> false
  | Raw_quant (q,r1) ->
     begin match r1 with
     | Raw_character Dot ->
        begin match q with
        | Star -> true
        | _ -> has_dgs r1
        end
     | _ -> has_dgs r1
     end
  | Raw_count (q,r1) ->
     begin match r1 with
     | Raw_character Dot ->
        (q.min = 0 && q.max = None && q.greedy = true) || has_dgs r1
     | _ -> has_dgs r1
     end


(** * Extracting literals from regexes  *)
type literal =
  | Prefix of string
  | Exact of string

let prefix (l:literal) : string =
  match l with
  | Prefix s -> s
  | Exact s -> s

let common_prefix (s1:string) (s2:string) : string =
  let min_len = min (String.length s1) (String.length s2) in
  let rec aux i =
    if i >= min_len then i
    else if s1.[i] = s2.[i] then aux (i+1)
    else i
  in
  let len = aux 0 in
  String.sub s1 0 len

let drop_first_n (s : string) (n : int) : string =
  let len = String.length s in
  if n >= len then ""
  else String.sub s n (len - n)

let merge (a:literal*int) (b:literal*int) : literal * int =
  match a, b with
  | (Exact s1, n1), (Exact s2, n2) ->
    let n = max n1 n2 in
    let s1' = drop_first_n s1 (n - n1) in
    let s2' = drop_first_n s2 (n - n2) in
    if s1' = s2' then (Exact s1', n)
    else (Prefix (common_prefix s1' s2'), n)
  | (Exact s1, n1), (Prefix s2, n2) | (Prefix s1, n1), (Exact s2, n2) | (Prefix s1, n1), (Prefix s2, n2) ->
    let n = max n1 n2 in
    let s1' = drop_first_n s1 (n - n1) in
    let s2' = drop_first_n s2 (n - n2) in
    (Prefix (common_prefix s1' s2'), n)
let chain (l1:literal*int) (l2:literal*int) : literal * int =
  match l1, l2 with
  | (Exact s1, n1), (Exact s2, 0) -> (Exact (s1 ^ s2), n1)
  | (Exact s1, n1), (Exact s2, _) -> (Prefix s1, n1)
  | (Exact s1, n1), (Prefix s2, 0) -> (Prefix (s1 ^ s2), n1)
  | (Exact s1, n1), (Prefix s2, _) -> (Prefix s1, n1)
  | (Prefix s1, n1), _ -> (Prefix s1, n1)

let rec extract_literal (r:raw_regex) : literal * int =
  match r with
  | Raw_empty -> (Exact "", 0)
  | Raw_character c ->
    begin match c with
    | Char ch -> (Exact (String.make 1 ch), 0)
    | _ -> (Exact "", 1)
    end
  | Raw_con(r1,r2) -> chain (extract_literal r1) (extract_literal r2)
  | Raw_alt(r1,r2) -> merge (extract_literal r1) (extract_literal r2)
  | Raw_capture r1 -> extract_literal r1
  | Raw_quant (q,r1) -> extract_literal (Raw_count (quant_canonicalize q, r1))
  | Raw_count ({min = mi; max = ma; greedy = g},r1) when mi > 1000 -> (Prefix "", 0) (* give up on huge unrolls *)
  | Raw_count ({min = 0; max = _; greedy = _},r1) -> (Prefix "", 0)
  | Raw_count ({min = mi; max = ma; greedy = g},r1) -> chain (extract_literal r1) (extract_literal (Raw_count ({min = mi-1; max = ma; greedy = g}, r1)))
  | Raw_lookaround (_,r1) -> (Exact "", 0)
  | Raw_anchor _ -> (Exact "", 0)

let rec has_impossible_literal (r:raw_regex) : bool =
    match r with
    | Raw_character (Class []) -> true
    | Raw_character c -> false
    | Raw_empty -> false
    | Raw_con(r1,r2) -> has_impossible_literal r1 || has_impossible_literal r2
    | Raw_alt(r1,r2) -> has_impossible_literal r1 && has_impossible_literal r2
    | Raw_capture r1 -> has_impossible_literal r1
    | Raw_quant (q,r1) -> has_impossible_literal (Raw_count (quant_canonicalize q, r1))
    | Raw_count ({min = mi; max = ma; greedy = g},r1) when mi > 0 -> has_impossible_literal r1
    | Raw_count (_,r1) -> false
    | Raw_lookaround (l,r1) ->
      begin match l with
      | Lookahead | Lookbehind -> has_impossible_literal r1
      | NegLookahead | NegLookbehind -> false
      end
    | Raw_anchor _ -> false

(* reverse regex *)
let rec rev_regex (r:raw_regex) : raw_regex =
  match r with
  | Raw_empty -> Raw_empty
  | Raw_character c -> Raw_character c
  | Raw_anchor a ->
    begin match a with
    | BeginInput -> Raw_anchor EndInput
    | EndInput -> Raw_anchor BeginInput
    | WordBoundary -> Raw_anchor WordBoundary
    | NonWordBoundary -> Raw_anchor NonWordBoundary
    end
  | Raw_con(r1,r2) -> Raw_con(rev_regex r2, rev_regex r1)
  | Raw_alt(r1,r2) -> Raw_alt(rev_regex r1, rev_regex r2)
  | Raw_capture r1 -> Raw_capture (rev_regex r1)
  | Raw_quant (q,r1) -> Raw_quant (q, rev_regex r1)
  | Raw_count (q,r1) -> Raw_count (q, rev_regex r1)
  | Raw_lookaround (l,r1) ->
    begin match l with
    | Lookahead -> Raw_lookaround (Lookbehind, rev_regex r1)
    | NegLookahead -> Raw_lookaround (NegLookbehind, rev_regex r1)
    | Lookbehind -> Raw_lookaround (Lookahead, rev_regex r1)
    | NegLookbehind -> Raw_lookaround (NegLookahead, rev_regex r1)
    end

(* whether there are zero length assertions *)
let rec has_asserts (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ -> false
  | Raw_anchor _ -> true
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_asserts r1 || has_asserts r2
  | Raw_capture r1 | Raw_quant (_,r1) | Raw_count (_,r1) -> has_asserts r1
  | Raw_lookaround _ -> true

(* detecting anchored regexes *)
let rec anchored (r:raw_regex) : bool =
  match r with
  | Raw_empty -> false
  | Raw_character _ -> false
  | Raw_anchor BeginInput -> true
  | Raw_anchor _ -> false
  | Raw_con(r1,r2) -> anchored r1 || anchored r2
  | Raw_alt(r1,r2) -> anchored r1 && anchored r2
  | Raw_capture r1 -> anchored r1
  | Raw_quant (q,r1) -> anchored (Raw_count (quant_canonicalize q, r1))
  | Raw_count (q,r1) -> if q.min = 0 then false else anchored r1
  | Raw_lookaround (l,r1) ->
        begin match l with
        | Lookahead -> anchored r1
        | Lookbehind | NegLookahead | NegLookbehind -> false
        end

(* detecting regexes that can be supported by the memoryless lookbehind only *)
(* they need to have lookbehinds without groups in them or negative lookbehinds *)
(* they also need to not have any lookaheads or positive lookbehinds with groups *)
exception NotMemoryLess

let rec lookbehind_only (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> lookbehind_only r1 || lookbehind_only r2
  | Raw_capture r1 | Raw_quant (_,r1) | Raw_count (_,r1) -> lookbehind_only r1
  | Raw_lookaround (l,r1) ->
     let _ = lookbehind_only r1 in (* possibly raising exceptions *)
     match l with
     | Lookahead | NegLookahead -> raise NotMemoryLess
     | NegLookbehind -> true
     | Lookbehind ->
        if (has_groups r1) then raise NotMemoryLess else true

let memoryless_lookbehind (r:raw_regex) : bool =
  try (lookbehind_only r) with NotMemoryLess -> false


(** * Parsing and analysing the Corpus  *)

type parse_result =
  | Unsupported
  | NotWF
  | ParseError
  | OK of raw_regex

(* Statistics record when analyzing a vast corpus of regexes *)
type support_stats = {
    mutable named:int;
    mutable hex:int;
    mutable unicode:int;
    mutable prop:int;
    mutable backref:int;
    mutable octal:int;
    mutable notwf:int;
    mutable errors:int;
    mutable parsed:int;
    mutable total:int;
    mutable null_quant:int;
    mutable quant_groups:int;
    mutable lookaround:int;
    mutable nn:int;
    mutable null_plus:int;
    mutable lazy_nullplus: int;
    mutable ml_behind:int;
    mutable front_only_literal:int;
    mutable back_only_literal:int;
    mutable offset_literal:int;
    mutable both_literal:int;
    mutable impossible_literal:int;
    mutable exact_literal:int;
    mutable exact_no_assert_literal:int;
    mutable exact_no_assert_and_no_groups_literal:int;
    mutable anchored:int;
    mutable reverse_anchored:int;
    mutable double_anchored:int;
    mutable captures_for_grouping:int;
    mutable no_captures:int;
    mutable dls:int;
    mutable dgs:int;
  }

let init_stats () : support_stats =
  { named=0; hex=0; unicode=0; prop=0; backref=0; notwf=0; octal=0;
    errors=0; parsed=0; total=0;
    null_quant=0; quant_groups=0; lookaround=0; nn=0; null_plus=0; lazy_nullplus=0; ml_behind=0;
    front_only_literal=0; back_only_literal=0; offset_literal=0;
        both_literal=0; exact_no_assert_literal=0; impossible_literal=0; exact_literal=0; exact_no_assert_and_no_groups_literal=0; anchored=0; reverse_anchored=0; double_anchored=0;
    captures_for_grouping=0; no_captures=0; dls=0; dgs=0; }

(* parsing a string for a regex *)
let parse (str:string) (stats:support_stats): parse_result =
  try
    stats.total <- stats.total + 1;
    let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
    if regex_wf r
    then
      begin
        try
          let rev_r = rev_regex r in
          let (front_lit, front_offset) = extract_literal r in
          let (back_lit, back_offset) = extract_literal rev_r in
          let has_asserts_ = has_asserts r in
          let has_captures = has_groups r in
          let is_anchored = anchored r in
          let is_rev_anchored = anchored rev_r in
          if (front_lit <> Prefix "" && back_lit = Prefix "" && front_offset = 0) then stats.front_only_literal <- stats.front_only_literal + 1;
          if (back_lit <> Prefix "" && front_lit = Prefix "" && back_offset = 0) then stats.back_only_literal <- stats.back_only_literal + 1;
          if (front_lit <> Prefix "" && back_lit <> Prefix "" && front_offset = 0 && back_offset = 0) then stats.both_literal <- stats.both_literal + 1;
          if (prefix front_lit <> "" && front_offset > 0 || prefix back_lit <> "" && back_offset > 0) then stats.offset_literal <- stats.offset_literal + 1;
          if (has_impossible_literal r) then stats.impossible_literal <- stats.impossible_literal + 1;
          if ((match front_lit with Exact _ -> true | _ -> false) && front_offset = 0) then stats.exact_literal <- stats.exact_literal + 1;
          if ((match front_lit with Exact _ -> true | _ -> false) && not (has_asserts_) && front_offset = 0) then stats.exact_no_assert_literal <- stats.exact_no_assert_literal + 1;
          if ((match front_lit with Exact _ -> true | _ -> false) && not (has_asserts_) && not (has_captures) && front_offset = 0) then stats.exact_no_assert_and_no_groups_literal <- stats.exact_no_assert_and_no_groups_literal + 1;
          if (is_anchored) then stats.anchored <- stats.anchored + 1;
          if (is_rev_anchored) then stats.reverse_anchored <- stats.reverse_anchored + 1;
          if (is_anchored && is_rev_anchored) then stats.double_anchored <- stats.double_anchored + 1;
          if (has_potentially_capture_just_for_grouping r) then stats.captures_for_grouping <- stats.captures_for_grouping + 1;
          if (not (has_captures)) then stats.no_captures <- stats.no_captures + 1;
          if (has_nullable_quant r) then stats.null_quant <- stats.null_quant + 1;
          if (groups_in_quant r) then stats.quant_groups <- stats.quant_groups + 1;
          if (has_lookaround r) then stats.lookaround <- stats.lookaround + 1;
          if (has_nn r) then stats.nn <- stats.nn + 1;
          if (has_nullplus r) then stats.null_plus <- stats.null_plus + 1;
          if (has_lazy_nullplus r) then stats.lazy_nullplus <- stats.lazy_nullplus + 1;
          if (memoryless_lookbehind r) then stats.ml_behind <- stats.ml_behind + 1;
          stats.parsed <- stats.parsed + 1;
          if (has_dls r) then stats.dls <- stats.dls + 1;
          if (has_dgs r) then stats.dgs <- stats.dgs + 1;
          OK r
        with e -> Printf.printf "Error while analyzing regex %s: %s\n%!" str (Printexc.to_string e); stats.errors <- stats.errors + 1; ParseError
      end
    else begin stats.notwf <- stats.notwf + 1; NotWF end
  with
  | Unsupported_named_groups -> stats.named <- stats.named + 1; Unsupported
  | Unsupported_hex -> stats.hex <- stats.hex + 1; Unsupported
  | Unsupported_unicode -> stats.unicode <- stats.unicode + 1; Unsupported
  | Unsupported_prop -> stats.prop <- stats.prop + 1; Unsupported
  | Unsupported_backref -> stats.backref <- stats.backref + 1; Unsupported
  | Unsupported_octal -> stats.octal <- stats.octal + 1; Unsupported
  | _ -> stats.errors <- stats.errors + 1; ParseError


let print_result (p:parse_result): string =
  match p with
  | Unsupported -> "Unsupported"
  | NotWF -> "NotWF"
  | ParseError -> "ParseError"
  | OK r -> report_raw r

(* fails if the regex is not correct *)
let parse_raw (str:string) : raw_regex =
  let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
  assert (regex_wf r);
  r

(* printing statistics results *)
let print_stats (s:support_stats) : string =
  "Note that some octal escapes may be counted as backrefs here. Anyway, both are unsupported." ^
  "\nUnsupported Named Groups: " ^ string_of_int s.named ^
  "\nUnsupported Hex Escapes: " ^ string_of_int s.hex ^
  "\nUnsupported Unicode Escapes: " ^ string_of_int s.unicode ^
  "\nUnsupported Unicode Properties: " ^ string_of_int s.prop ^
  "\nUnsupported Backreferences: " ^ string_of_int s.backref ^
  "\nUnsupported Octal: " ^ string_of_int s.octal ^
  "\nNot WellFormed: " ^ string_of_int s.notwf ^
  "\nErrors: " ^ string_of_int s.errors ^

  "\n\nMETA ENGINE" ^
  "\nRegexes with only a front literal: " ^ string_of_int s.front_only_literal ^
  "\nRegexes with only a back literal: " ^ string_of_int s.back_only_literal ^
  "\nRegexes with a offset literal: " ^ string_of_int s.offset_literal ^
  "\nRegexes with both front and back literals: " ^ string_of_int s.both_literal ^
  "\nRegexes with impossible literal: " ^ string_of_int s.impossible_literal ^
  "\nRegexes with exact literal: " ^ string_of_int s.exact_literal ^
  "\nRegexes with exact literal and no asserts: " ^ string_of_int s.exact_no_assert_literal ^
  "\nRegexes with exact literal and no asserts and no groups: " ^ string_of_int s.exact_no_assert_and_no_groups_literal ^
  "\nAnchored Regexes: " ^ string_of_int s.anchored ^
  "\nReverse Anchored Regexes: " ^ string_of_int s.reverse_anchored ^
  "\nDouble Anchored Regexes: " ^ string_of_int s.double_anchored ^
  "\nRegexes with captures probably only for grouping: " ^ string_of_int s.captures_for_grouping ^
  "\nRegexes with no captures at all: " ^ string_of_int s.no_captures ^

  "\n\nOTHER STATS:" ^
  "\nDot Lazy Star: " ^ string_of_int s.dls ^
  "\nDot Greedy Star: " ^ string_of_int s.dgs ^

  "\n\nNUMBERS FOR FIGURE16:" ^
  "\nPARSED REGEXES / TOTAL REGEXES: " ^ string_of_int s.parsed ^ " / " ^ string_of_int s.total ^
  "\nNullable Quantifiers: " ^ string_of_int s.null_quant ^
  "\nCapture Groups in Quantifiers: " ^ string_of_int s.quant_groups ^
  "\nLookarounds: " ^ string_of_int s.lookaround ^
  "\nNonNullable, min>0 quantifiers (Non-nullable +): " ^ string_of_int s.nn ^
  "\nNullable Greedy min>0 quantifiers (CIN&CDN greedy +): " ^ string_of_int s.null_plus ^
  "\nNullable NonGreedy min>0 quantifiers (CIN&CDN lazy +?): " ^ string_of_int s.lazy_nullplus ^
  "\nMemoryLess Lookbehinds without groups (Captureless lookbehinds): " ^ string_of_int s.ml_behind ^
  "\n"

let print_stats_csv (s:support_stats) : string =
  let csv_header = "named,hex,unicode,prop,backref,octal,notwf,errors,parsed,total,null_quant,quant_groups,lookaround,nn,null_plus,lazy_nullplus,ml_behind,front_only_literal,back_only_literal,offset_literal,both_literal,impossible_literal,exact_literal,exact_no_assert_literal,exact_no_assert_and_no_groups_literal,anchored,reverse_anchored,double_anchored,captures_for_grouping,no_captures" in
  let csv_values = Printf.sprintf "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d"
    s.named s.hex s.unicode s.prop s.backref s.octal s.notwf s.errors s.parsed s.total
    s.null_quant s.quant_groups s.lookaround s.nn s.null_plus s.lazy_nullplus s.ml_behind
    s.front_only_literal s.back_only_literal s.offset_literal
    s.both_literal s.impossible_literal s.exact_literal s.exact_no_assert_literal s.exact_no_assert_and_no_groups_literal
    s.anchored s.reverse_anchored s.double_anchored s.captures_for_grouping s.no_captures in
  csv_header ^ "\n" ^ csv_values

let analyze_regex (regex_str:string) (stats:support_stats) =
  let result = parse regex_str stats in
  match result with
  | OK r -> if false then   (* select what you want to print *)
              begin Printf.printf "\n\027[36m%s\027[0m\n%!" regex_str;
                    Printf.printf "%s\n%!" (print_result result)
              end
  | ParseError -> ()
  | _ -> ()

let analyze_corpus (filename:string) (single:bool) (st:support_stats option) : string =
  let stats =
    match st with
    | None -> init_stats()
    | Some s -> s in
  let chan = open_in filename in
  try
    while true; do
      let str = input_line chan in
      (* the list of all patterns defined on that line *)
      let regex_str = try
          let json_str = Yojson.Basic.from_string str in
          if single then
            [json_str |> member "pattern" |> to_string]
          else
            List.map (to_string) (json_str |> member "patterns" |> to_list)
        with
        | Yojson.Json_error _ -> []
        | Yojson.Basic.Util.Type_error _ -> []
      in
      List.iter (fun str -> analyze_regex str stats) regex_str
    done; ""
  with End_of_file ->
    close_in chan;
    "\nCorpus \027[33m" ^ filename ^ "\027[0m:\n" ^ print_stats stats ^ "\n"

let analyze_single_corpus filename single: unit =
  let result = analyze_corpus filename single None in
  Printf.printf ("%s\n%!") result


let main =
  let corpora = [("corpus/npm-uniquePatterns.json",true);
                 ("corpus/pypi-uniquePatterns.json",true);
                 ("corpus/internetSources-regExLib.json",false);
                 ("corpus/internetSources-stackoverflow.json",false);
                 ("corpus/uniq-regexes-8.json",true)] in

  let csv = ref false in
  let speclist = [
    ("--csv", Arg.Set csv, "Output CSV format for total stats")
  ] in
  let usage_msg = "stats [--csv]" in
  let () = Arg.parse speclist (fun _ -> ()) usage_msg in

  if !csv then begin
    let stats = init_stats() in
    List.iter (fun (f,b) -> ignore(analyze_corpus f b (Some stats))) corpora;
    Printf.printf ("%s\n") (print_stats_csv stats)
  end else begin
    (* individual stats *)
    List.iter (fun (f,b) -> analyze_single_corpus f b) corpora;

    (* getting total stats *)
    let stats = init_stats() in
    List.iter (fun (f,b) -> ignore(analyze_corpus f b (Some stats))) corpora;
    Printf.printf ("\027[33mAll Corpus\027[0m:\n%s\n") (print_stats stats)
  end
