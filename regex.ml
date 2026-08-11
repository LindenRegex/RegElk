(** * Regexes  *)
(* Defining the type of regexes that we match, including both capture groups and lookarounds *)

open Charclasses

(** * Parsing Exceptions  *)
(* constructs that are unsupported in our regex AST *)
exception Unsupported_named_groups
exception Unsupported_hex
exception Unsupported_unicode
exception Unsupported_prop
exception Unsupported_backref
exception Unsupported_octal

(** * Quantifiers  *)

(* Usual quantifiers *)
(* all of these can be compiled for regex-linear matching, except lazyplus *)
type quantifier =
  | Star
  | LazyStar
  | Plus
  | LazyPlus
  | QuestionMark
  | LazyQuestionMark

(* The more generic type of quantifiers, that can do non-linear counted repetition *)
(* In the general case, this may require repeating the bytecode of the body, and non-regex-linear matching *)
type counted_quantifier = {
    min: int;
    max: int option;            (* None represents infinity *)
    greedy: bool;
  }

(* turning the usual quantifiers into the more generic type *)
let quant_canonicalize (q:quantifier) : counted_quantifier =
  match q with
  | Star -> { min = 0; max = None; greedy = true }
  | LazyStar -> { min = 0; max = None; greedy = false }
  | Plus -> { min = 1; max = None; greedy = true }
  | LazyPlus -> { min = 1; max = None; greedy = false }
  | QuestionMark -> { min = 0; max = Some 1; greedy = true }
  | LazyQuestionMark -> { min = 0; max = Some 1; greedy = false }

(** * Lookarounds  *)
type lookaround =
  | Lookahead
  | NegLookahead
  | Lookbehind
  | NegLookbehind

(** * Anchors  *)
(* Also called 0-width assertion *)
(* Assertions about the current position that can be determined only from the immediate context (characters around the cp) *)
(* never consumes anything *)

type anchor =
  | EndInput
  | BeginInput
  | WordBoundary
  | NonWordBoundary
(* note that if we implemented some flags, like multiline, then we should add other anchors, like EndLine and BeginLine *)
(* for now we do not implement such a flag *)

(** * Character Characterizations  *)
(* different ways of representing a character *)

type character =
  | Char of char                (* a simple character *)
  | Dot                         (* any character *)
  | Group of char_group         (* PERL character classes \s, \w... *)
  | Class of char_class         (* a character class *)
  | NegClass of char_class      (* a negated character class *)


(** * Raw Regexes  *)
(* Input regexes, before they get annotated with capture groups identifiers and lookaround identifiers *)
type raw_regex =
  | Raw_empty
  | Raw_character of character
  | Raw_alt of raw_regex * raw_regex
  | Raw_con of raw_regex * raw_regex
  | Raw_quant of quantifier * raw_regex
  | Raw_count of counted_quantifier * raw_regex
  | Raw_capture of raw_regex
  | Raw_lookaround of lookaround * raw_regex
  | Raw_anchor of anchor


(* some shortcuts for simpler ASTs *)
let raw_char (x:char): raw_regex  = Raw_character (Char x)
let raw_dot: raw_regex = Raw_character Dot
let raw_star (r:raw_regex) : raw_regex = Raw_quant(Star, r)
let raw_plus (r:raw_regex) : raw_regex = Raw_quant(Plus, r)
let raw_qmark (r:raw_regex) : raw_regex = Raw_quant(QuestionMark, r)
let raw_group (g:char_group) : raw_regex = Raw_character (Group g)
let raw_class (c:char_class) : raw_regex = Raw_character (Class c)
let raw_neg_class (c:char_class) : raw_regex = Raw_character (NegClass c)

(** * Nullable Regexes  *)
(* Nullable quantifiers can be compiled differently *)
(* here we simply identify nullable regexes *)

(* There are 3 different types of nullability *)
(* Either a regex is non-nullable, meaning that all its paths lead to a Consume *)
(* Or it is Context-Independent Nullable, meaning that there is for sure a nullable path that does not depend on the string positiion *)
(* Or it is context-dependent nullable, for instance if its only nullable path goes through a lookaround or anchor *)

(* Note that sometimes we can classify CIN regexes as CDN (for instance if we go through a lookaround that is always true), *)
(* but our solution for CDN regexes also works for CIN (more general but slower) *)

type nullability =
  | NonNullable                 (* it is impossible for the regex to be nulled *)
  | CDNullable                  (* teh regex can be nulled depending on the current string position *)
  | CINullable                  (* the regex can be nulled, and this does not depend on the current string position *)

(** * Annotated Regexes  *)

(* capture unique identifiers *)
type capture = int
(* lookaround unique identifiers *)
type lookid = int
(* quantifiers unique identifiers *)
type quantid = int

type regex =
  | Re_empty
  | Re_character of character
  | Re_alt of regex * regex
  | Re_con of regex * regex
  (* each quantifier is given an unique id *)
  (* all quantifiers are transformed into counted quantifiers *)
  | Re_quant of nullability * quantid * counted_quantifier * regex
  | Re_capture of capture * regex
  | Re_lookaround of lookid * lookaround * regex
  | Re_anchor of anchor


(** * Computing Nullability  *)

let null_or (n1:nullability) (n2:nullability): nullability =
  match n1 with
  | NonNullable -> n2
  | CDNullable -> begin match n2 with
                  | CINullable -> CINullable
                  | _ -> CDNullable end
  | CINullable -> CINullable

let null_and (n1:nullability)  (n2:nullability): nullability =
  match n1 with
  | NonNullable -> NonNullable
  | CDNullable -> begin match n2 with
                  | NonNullable -> NonNullable
                  | _ -> CDNullable end
  | CINullable -> n2

let rec nullable (r:regex) : nullability =
  match r with
  | Re_empty -> CINullable
  | Re_character _ -> NonNullable
  | Re_alt (r1,r2) -> null_or (nullable r1) (nullable r2)
  | Re_con (r1,r2) -> null_and (nullable r1) (nullable r2)
  | Re_quant (_,_,q,r1) ->
     if (q.min = 0) then CINullable else nullable r1
  | Re_capture (_,r1) -> nullable r1
  | Re_lookaround (_,_,_) -> CDNullable
  | Re_anchor _ -> CDNullable


let rec raw_nullable (r:raw_regex) : nullability =
  match r with
  | Raw_empty -> CINullable
  | Raw_character _ -> NonNullable
  | Raw_alt (r1,r2) -> null_or (raw_nullable r1) (raw_nullable r2)
  | Raw_con (r1,r2) -> null_and (raw_nullable r1) (raw_nullable r2)
  | Raw_quant (q,r1) ->
     begin match q with
     | Star | LazyStar | QuestionMark | LazyQuestionMark -> CINullable
     | Plus | LazyPlus -> raw_nullable r1
     end
  | Raw_count (q,r1) ->
     if (q.min = 0) then CINullable else raw_nullable r1
  | Raw_capture (r1) -> raw_nullable r1
  | Raw_lookaround (_,_) -> CDNullable
  | Raw_anchor _ -> CDNullable


(** * Regex Pretty-printing  *)

(* WARNING: this is wrong in the sense that we don't print non-capturing groups *)
(* Meaning that there will be no difference between [{ab} alt c] and [a alt {bc}] *)
(* If you want to send this to an engine, add non-capturing groups first (see tojs.ml for instance) *)

let print_quant (q:quantifier) : string =
  match q with
  | Star -> "*"
  | LazyStar -> "*?"
  | Plus -> "+"
  | LazyPlus -> "+?"
  | QuestionMark -> "?"
  | LazyQuestionMark -> "??"

(* Prints things like {4,5}, {6,} or {3,8}? *)
let print_counted_quant (q:counted_quantifier) : string =
  let min = string_of_int q.min in
  let max = begin match q.max with
            | None -> ""
            | Some m -> string_of_int m end in
  let lzy = if q.greedy then "" else "?" in
  "{"^min^","^max^"}"^lzy


let print_lookaround (l:lookaround) : string =
  match l with
  | Lookahead -> "?="
  | NegLookahead -> "?!"
  | Lookbehind -> "?<="
  | NegLookbehind -> "?<!"

let print_anchor (a:anchor) : string =
  match a with
  | BeginInput -> "^"
  | EndInput -> "$"
  | WordBoundary -> "\\b"
  | NonWordBoundary -> "\\B"

let print_character (c:character) : string =
  match c with
  | Char x -> String.make 1 x
  | Dot -> "."
  | Group g -> print_group g
  | Class c -> "[" ^ print_class c ^ "]"
  | NegClass c -> "[^" ^ print_class c^ "]"


let rec print_raw (ra:raw_regex) : string =
  match ra with
  | Raw_empty -> ""
  | Raw_character c -> print_character c
  | Raw_alt (r1, r2) -> print_raw r1 ^ "|" ^ print_raw r2
  | Raw_con (r1, r2) -> print_raw r1 ^ print_raw r2
  | Raw_quant (q, r1) -> print_raw r1 ^ print_quant q
  | Raw_count (q, r1) -> print_raw r1 ^ print_counted_quant q
  | Raw_capture r1 -> "(" ^ print_raw r1 ^ ")"
  | Raw_lookaround (l, r1) -> "(" ^ print_lookaround l ^ print_raw r1 ^ ")"
  | Raw_anchor a -> print_anchor a

let rec print_regex (r:regex) : string =
  match r with
  | Re_empty -> ""
  | Re_character c -> print_character c
  | Re_alt (r1, r2) -> print_regex r1 ^ "|" ^ print_regex r2
  | Re_con (r1, r2) -> print_regex r1 ^ print_regex r2
  | Re_quant (_, qid, q, r1) -> print_regex r1 ^ print_counted_quant q ^ "\027[31m" ^ string_of_int qid ^ "\027[0m"
  | Re_capture (cid, r1) -> "(" ^ print_regex r1 ^ ")" ^ "\027[33m" ^ string_of_int cid ^ "\027[0m"
  | Re_lookaround (lid, l, r1) -> "(" ^ "\027[36m" ^ string_of_int lid ^ "\027[0m" ^ print_lookaround l ^ print_regex r1 ^ ")"
  | Re_anchor a -> print_anchor a


(** * Annotating Regexes  *)

(* Adds annotation, identifiers for each capture group and lookaround *)
(* Also returning the next fresh capture identifier and next fresh lookaround identifier *)
(* Also canonicalizes every quantifier *)
let rec annotate_regex (ra:raw_regex) (c:capture) (l:lookid) (q:quantid) : regex * capture * lookid  * quantid =
  match ra with
  | Raw_empty -> (Re_empty, c, l, q)
  | Raw_character r -> (Re_character r, c, l, q)
  | Raw_alt (r1, r2) ->
     let (ar1, c1, l1, q1) = annotate_regex r1 c l q in
     let (ar2, c2, l2, q2) = annotate_regex r2 c1 l1 q1 in
     (Re_alt (ar1, ar2), c2, l2, q2)
  | Raw_con (r1, r2) ->
     let (ar1, c1, l1, q1) = annotate_regex r1 c l q in
     let (ar2, c2, l2, q2) = annotate_regex r2 c1 l1 q1 in
     (Re_con (ar1, ar2), c2, l2, q2)
  | Raw_quant (quant, r1) ->
     let (ar1, c1, l1, q1) = annotate_regex r1 c l (q+1) in
     (Re_quant (nullable ar1, q, quant_canonicalize quant, ar1), c1, l1, q1)
  | Raw_count (quant, r1) ->
     let (ar1, c1, l1, q1) = annotate_regex r1 c l (q+1) in
     (Re_quant (nullable ar1, q, quant, ar1), c1, l1, q1)
  | Raw_capture r1 ->
     let (ar1, c1, l1, q1) = annotate_regex r1 (c+1) l q in
     (Re_capture (c, ar1), c1, l1, q1)
  | Raw_lookaround (look, r1) ->
     let (ar1, c1, l1, q1) = annotate_regex r1 c (l+1) q in
     (Re_lookaround (l, look, ar1), c1, l1, q1)
  | Raw_anchor a -> (Re_anchor a, c, l, q)

(* adding annotations and adding a capture group on the entire regex *)
(* the external capture group starts at 0 *)
(* lookarounds start at 1 *)
(* quants start at 1: we want to put the lazy star first *)
let annotate (ra:raw_regex) : regex =
  let (re,_,_,_) = annotate_regex (Raw_capture ra) 0 1 1 in re

(* Adds a .*? at the beginning of a regex so that it does not have to be matched at the beginning *)
let lazy_prefix (r:regex) : regex =
  Re_con (Re_quant (NonNullable, 0,{min=0;max=None;greedy=false}, Re_character Dot), r)
(* TODO: if there is a BeginInput at the beginning of the regex, we could remove the lazy star *)

(** * Regex Manipulation  *)

(* Reversing a regex when we want to execute it backward *)
(* Note that we only need to reverse the concatenation *)
(* For the alternation, the left subexpr still has priority over the right one when looking for groups *)
(* For instance, in [ab(?<=(ab|b))] over "ab", group 1 contains "ab", not "b" *)


(* we also need to reverse anchors: the end of input becomes the beginning and vice-versa *)
(* let reverse_anchor (a:anchor) : anchor =
 *   match a with
 *   | BeginInput -> EndInput
 *   | EndInput -> BeginInput
 *   | _ -> a *)
(* deprecated: we no longer reverse anchors. instead, the behavior of the anchor changes depending on the interpreter direction *)

let rec reverse_regex (r:regex) : regex =
  match r with
  | Re_empty | Re_character _ -> r
  | Re_alt (r1, r2) -> Re_alt (reverse_regex r1, reverse_regex r2)
  | Re_con (r1, r2) -> Re_con (reverse_regex r2, reverse_regex r1) (* reversing concatenation *)
  | Re_quant (nul, qid, quant, r1) -> Re_quant (nul, qid, quant, reverse_regex r1)
  | Re_capture (cid, r1) -> Re_capture (cid, reverse_regex r1)
  | Re_lookaround (lid, look, r1) -> Re_lookaround (lid, look, reverse_regex r1)
  | Re_anchor a -> Re_anchor a


(* during the 1st stage of the algorithm, we don't care about extracting capture groups *)
(* so we can clean the regex to remove capture groups *)
(* We also remove the annotations in the quantifiers because there is no need to clear capture registers *)
let rec remove_capture (r:regex) : regex =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> r
  | Re_alt (r1, r2) -> Re_alt (remove_capture r1, remove_capture r2)
  | Re_con (r1, r2) -> Re_con (remove_capture r1, remove_capture r2)
  | Re_quant (nul, qid, quant, r1) ->
     Re_quant (nul, qid, quant, remove_capture r1)
  (* TODO: we could have, instead of a quant identifier, an option *)
  (* when it's None, it means we don't have to remember the last iteration in a memory *)
  (* this would be useful in the first stage, or when the body has no capture group, *)
  (* or for the lazy star at the beginning *)
  | Re_capture (cid, r1) -> remove_capture r1 (* removing the group entirely *)
  | Re_lookaround (lid, look, r1) -> Re_lookaround (lid, look, remove_capture r1)


(** * Lookaround Manipulation  *)

(* Extracting the lookaround type and inner subexpression of a given lookaround identifier *)
let rec get_lookaround (r:regex) (lid:lookid) : (regex * lookaround) option =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> None
  | Re_alt (r1, r2) | Re_con (r1, r2) -> (* the order does not matter since each identifier is unique *)
     begin match (get_lookaround r1 lid) with
     | Some le -> Some le
     | None -> get_lookaround r2 lid
     end
  | Re_quant (_, _, _, r1) | Re_capture (_, r1)->
     get_lookaround r1 lid
  | Re_lookaround (l, look, r1) ->
     if (l = lid) then Some (r1, look)
     else get_lookaround r1 lid

(* we should always be able to find a lookahead in the right range *)
let get_look (r:regex) (lid:lookid) : regex * lookaround =
  match (get_lookaround r lid) with
  | Some r -> r
  | _ -> failwith "Cannot find lookaround"

(* extracting a quantifier body given its quantifier id *)
let rec get_quantifier (r:regex) (qid:quantid) : (regex * counted_quantifier) option =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> None
  | Re_alt (r1, r2) | Re_con (r1, r2) ->
     begin match (get_quantifier r1 qid) with
     | Some qr -> Some qr
     | None -> get_quantifier r2 qid
     end
  | Re_lookaround (_, _, r1) | Re_capture (_, r1) ->
     get_quantifier r1 qid
  | Re_quant (nul,id,quant,r1) ->
     if (id = qid) then Some (r1, quant)
     else get_quantifier r1 qid

let get_quant (r:regex) (qid:quantid) : regex * counted_quantifier =
  match (get_quantifier r qid) with
  | Some q -> q
  | _ -> failwith "Cannot find quantifier"

(* Returns the maximum used lookaround in a regex *)
let rec max_lookaround (r:regex) : lookid =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> 0
  | Re_alt (r1, r2) | Re_con (r1, r2) -> max (max_lookaround r1) (max_lookaround r2)
  | Re_quant (_, _,_,r1) | Re_capture (_,r1) -> max_lookaround r1
  | Re_lookaround (lid, look, r1) -> max lid (max_lookaround r1)

(* maximum capture group *)
let rec max_group (r:regex) : capture =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> 0
  | Re_alt (r1, r2) | Re_con (r1, r2) -> max (max_group r1) (max_group r2)
  | Re_quant (_,_,_,r1) | Re_lookaround (_,_,r1) -> max_group r1
  | Re_capture (cid, r1) -> max cid (max_group r1)

(* maximum quantifier *)
let rec max_quant (r:regex) : quantid =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> 0
  | Re_alt (r1, r2) | Re_con (r1, r2) -> max (max_quant r1) (max_quant r2)
  | Re_lookaround (_,_,r1) | Re_capture  (_,r1) -> max_quant r1
  | Re_quant(_,qid,_,r1) -> max qid (max_quant r1)

(* Returns the list of nullable plus quantifier identifiers *)
(* ordered from lowest to highest *)
(* we consider every counted repetition that ends in a nullable greedy plus (min>0,max=None,greedy) *)
let rec nullable_plus_quantid' (r:regex) (lq:quantid list) : quantid list =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> lq
  | Re_alt (r1, r2) | Re_con (r1, r2) ->
     nullable_plus_quantid' r2 (nullable_plus_quantid' r1 lq)
  | Re_lookaround (_,_,r1) | Re_capture (_, r1) -> nullable_plus_quantid' r1 lq
  | Re_quant(nul,qid,quant,r1) ->
     begin match nul with
     | CDNullable | CINullable ->
        if (quant.min > 0 && quant.max = None && quant.greedy) then nullable_plus_quantid' r1 (qid::lq)
        else nullable_plus_quantid' r1 lq
     | _ -> nullable_plus_quantid' r1 lq
     end

let nullable_plus_quantid (r:regex) : quantid list =
  List.rev (nullable_plus_quantid' r [])

(* returns the list of all CDN plus *)
(* ordered from highest to lowest *)
(* we consider every counted repetition that ends in a nullable greedy plus (min>0,max=None,greedy) *)
let rec cdn_plus_list' (r:regex) (lq:quantid list) : quantid list =
match r with
| Re_empty | Re_character _ | Re_anchor _ -> lq
| Re_alt (r1, r2) | Re_con (r1, r2) ->
   cdn_plus_list' r2 (cdn_plus_list' r1 lq)
| Re_lookaround (_,_,r1) | Re_capture (_, r1) -> cdn_plus_list' r1 lq
| Re_quant (nul,qid,quant,r1) ->
   if (nul = CDNullable && quant.min > 0 && quant.max = None && quant.greedy)
   then cdn_plus_list' r1 (qid::lq)
   else cdn_plus_list' r1 lq

let cdn_plus_list (r:regex) : quantid list =
  cdn_plus_list' r []


(** * Error Reporting  *)
(* we want to be able to print exactly the AST to the console so that we can copy paste it when the fuzzer finds a crash *)
let report_quant (q:quantifier) : string =
  match q with
  | Star -> "Star"
  | LazyStar -> "LazyStar"
  | Plus -> "Plus"
  | LazyPlus -> "LazyPlus"
  | QuestionMark -> "QuestionMark"
  | LazyQuestionMark -> "LazyQuestionMark"

let report_counted_quant (q:counted_quantifier) : string =
  let min = string_of_int q.min in
  let max = begin match q.max with
            | None -> "None"
            | Some x -> "Some "^string_of_int x end in
  let greedy = string_of_bool q.greedy in
  "{min="^min^";max="^max^";greedy="^greedy^"}"

let report_look (l:lookaround) : string =
  match l with
  | Lookahead -> "Lookahead"
  | NegLookahead -> "NegLookahead"
  | Lookbehind -> "Lookbehind"
  | NegLookbehind -> "NegLookbehind"

let report_anchor (a:anchor) : string =
  match a with
  | BeginInput -> "BeginInput"
  | EndInput -> "EndInput"
  | WordBoundary -> "WordBoundary"
  | NonWordBoundary -> "NonWordBoundary"

let report_group (g:char_group) : string =
  match g with
  | Digit -> "Digit"
  | NonDigit -> "NonDigit"
  | Word -> "Word"
  | NonWord -> "NonWord"
  | Space -> "Space"
  | NonSpace -> "NonSpace"

(* when reporting chars that may require escaping *)
let report_char (c:char) : string =
  "char_of_int("^string_of_int(int_of_char c)^")"

let report_class_elt (e:char_class_elt) : string =
  match e with
  | CChar x -> "CChar("^report_char x^")"
  | CRange (c1,c2) -> "CRange("^report_char c1^","^report_char c2^")"
  | CGroup g -> "CGroup("^report_group g^")"

let rec rep_class (c:char_class) : string =
  match c with
  | [] -> ""
  | e::[] -> report_class_elt e
  | e::next -> report_class_elt e^";"^rep_class next

let report_class (c:char_class) : string =
  "["^rep_class c^"]"

let report_character (c:character) : string =
  match c with
  | Char x -> "Char(\'"^String.make 1 x^"\')"
  | Dot -> "Dot"
  | Group g -> "Group("^report_group g^")"
  | Class cl -> "Class("^report_class cl^")"
  | NegClass cl -> "NegClass("^report_class cl^")"

let rec report_raw (raw:raw_regex) : string =
  match raw with
  | Raw_empty -> "Raw_empty"
  | Raw_character c -> "Raw_character("^report_character c^")"
  | Raw_alt (r1,r2) -> "Raw_alt("^report_raw r1^","^report_raw r2^")"
  | Raw_con (r1,r2) -> "Raw_con("^report_raw r1^","^report_raw r2^")"
  | Raw_quant (q,r1) -> "Raw_quant("^report_quant q^","^report_raw r1^")"
  | Raw_count (q,r1) -> "Raw_count("^report_counted_quant q^","^report_raw r1^")"
  | Raw_capture r1 -> "Raw_capture("^report_raw r1^")"
  | Raw_lookaround (l,r1) -> "Raw_lookaround("^report_look l^","^report_raw r1^")"
  | Raw_anchor a -> "Raw_anchor("^report_anchor a^")"


(** * Regex Plus Statistics  *)
(* returns (nn,cdn,cin,lnn,ln) where nn is the number of non-nullable +, *)
(* cdn is the number of context-dependent nullable + *)
(* cin is the number of context-independent nullable + *)
(* lnn is the number of non-nullable lazy + *)
(* ln is the number of nullable lazy + *)
let rec plus_stats (r:regex) : int * int * int * int * int =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> (0,0,0,0,0)
  | Re_alt (r1,r2) | Re_con (r1,r2) ->
     let (nn1,cdn1,cin1,lnn1,ln1) = plus_stats r1 in
     let (nn2,cdn2,cin2,lnn2,ln2) = plus_stats r2 in
     (nn1+nn2,cdn1+cdn2,cin1+cin2,lnn1+lnn2,ln1+ln2)
  | Re_lookaround (_,_,r1) | Re_capture (_,r1) ->
     plus_stats r1
  | Re_quant(nul,_,quant,r1) ->
     let (nn1,cdn1,cin1,lnn1,ln1) = plus_stats r1 in
     if (quant.min > 0 && quant.max = None && quant.greedy && nul = NonNullable)
     then (nn1+1,cdn1,cin1,lnn1,ln1)
     else if (quant.min > 0 && quant.max = None && quant.greedy && nul = CDNullable)
     then (nn1,cdn1+1,cin1,lnn1,ln1)
     else if (quant.min > 0 && quant.max = None && quant.greedy && nul = CINullable)
     then (nn1,cdn1,cin1+1,lnn1,ln1)
     else if (quant.min > 0 && quant.max = None && not quant.greedy && nul = NonNullable)
     then (nn1,cdn1,cin1,lnn1+1,ln1)
     else if (quant.min > 0 && quant.max = None && not quant.greedy && (nul = CDNullable || nul = CINullable))
     then (nn1,cdn1,cin1,lnn1,ln1+1)
     else (nn1,cdn1,cin1,lnn1,ln1) (* not a plus *)


(** * Regex Well-Formedness  *)
(* Checking that a regex is well-formed *)
(* In practice, this means checking that ranges are well defined (the max is greater than the min) *)
(* and that the counted repetitions are well defined as well when there is a maximum *)
(* We also check that this only uses ascii characters < 128 *)

(* maybe I should check this during parsing instead? *)
(* I like having a separate function for now *)

let char_wf (c:char) : bool =
  int_of_char c < 128

let class_elt_wf (e:char_class_elt) : bool =
  match e with
  | CChar c -> char_wf c
  | CGroup _ -> true
  | CRange (c1,c2) -> c1 <= c2

let class_wf (cl:char_class) : bool =
  List.fold_left (&&) true (List.map class_elt_wf cl)

let rec regex_wf (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_anchor _ -> true
  | Raw_alt(r1,r2) | Raw_con(r1,r2) ->
     regex_wf r1 && regex_wf r2
  | Raw_quant (_,r1) | Raw_capture r1 | Raw_lookaround(_,r1) ->
     regex_wf r1
  | Raw_count (c,r1) ->
     let ok_range = begin match c.max with
                    | None -> true
                    | Some m -> c.min <= m
                    end in
     ok_range && regex_wf r1
  | Raw_character c ->
     begin match c with
     | Char c -> char_wf c
     | Dot | Group _ -> true
     | Class cl | NegClass cl -> class_wf cl
     end
