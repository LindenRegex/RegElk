(** Representing Classes of Characters *)
(* including character ranges and negated classes *)

(* NOTE: OCaml chars go from 0 to 255 *)
(* While JS chars go to 65535 *)
(* Here we only deal with the first 256 chars *)

(* https://tc39.es/ecma262/#ASCII-word-characters *)
let is_ascii_word_character (c:char) : bool =
  let n = int_of_char c in
  (n>=65 && n<=90) ||           (* uppercase *)
    (n>=97 && n<=122) ||        (* lowercase *)
      (n>=48 && n<=57) ||       (* numbers *)
        (n=95)                  (* '_' *)

let min_char : char = char_of_int 0
let max_char : char = char_of_int 255

let prev_char (c:char) = char_of_int ((int_of_char c) -1)
let next_char (c:char) = char_of_int ((int_of_char c) +1)
let char_max (c1:char) (c2:char) = if c1 > c2 then c1 else c2

type char_expectation =
  | All                         (* expects any character *)
  | Single of char              (* a particular character *)
  | Ranges of (char * char) list (* several ranges of characters *)
(* In the case of Ranges, we expect the list to be ordered *)
(* Both chars defined the bounds of the range, and are both included *)

(* Note: I could express eveerything in terms of range *)
(* But I feel like the bytecode would be more readable with these 2 special cases *)


(** * Range Negation  *)

(* computes the ordered negation of an ordered list of ranges *)
let rec range_negation (l:(char*char) list) (min:char): (char*char) list =
  match l with
  | [] -> [(min,max_char)]
  | (r1,r2)::l' ->
     let next = if (r2 = max_char) then [] else (range_negation l' (next_char r2)) in
     if (min < r1) then
       (min,prev_char r1)::next
     else
       next

let range_neg (l:(char*char) list) : (char*char) list =
  range_negation l min_char

(* negates a char expectation *)
let negation (ce:char_expectation) : char_expectation =
  match ce with
  | All -> Ranges []
  | Single x -> Ranges (range_neg [(x,x)])
  | Ranges l -> Ranges (range_neg l)

(** * Usual Ranges - stopped at 255 *)
(* \d *)
let digit : (char * char) list =
  [(char_of_int 48,char_of_int 57)]

(* \w *)
let word : (char * char) list =
  [(char_of_int 48,char_of_int 57);
   (char_of_int 65,char_of_int 90);
   (char_of_int 95,char_of_int 95);
   (char_of_int 97,char_of_int 122)]

(* \s *)
let space : (char * char) list =
  [(char_of_int 9,char_of_int 13);
   (char_of_int 32,char_of_int 32);
   (char_of_int 160,char_of_int 160)]
(* for JS, also missing 5760, 8192-8202, 8232-8233, 8239, 8287, 12288, 65279 *)


(** * Character Groups  *)
(* usual character classes *)

type char_group =
  | Digit                       (* \d *)
  | NonDigit                    (* \D *)
  | Word                        (* \w *)
  | NonWord                     (* \W *)
  | Space                       (* \s *)
  | NonSpace                    (* \S *)

let group_to_range (g:char_group) : (char*char) list =
  match g with
  | Digit -> digit
  | NonDigit -> range_neg digit
  | Word -> word
  | NonWord -> range_neg word
  | Space -> space
  | NonSpace -> range_neg space


(** * Character Classes  *)

type char_class_elt =
  | CChar of char
  | CRange of char * char       (* e.g. "a-e" *)
  | CGroup of char_group        (* e.g. "\w" *)

(* this is the contents you can find between [] or [^] *)
(* it may be out of order *)
type char_class = char_class_elt list

(* flattening everything to a list of unordered ranges *)
let rec class_flatten (c:char_class) : (char*char) list =
  match c with
  | [] -> []
  | (CChar x)::c' -> (x,x)::(class_flatten c')
  | (CRange (c1,c2))::c' -> (c1,c2)::(class_flatten c')
  | (CGroup g)::c'-> (group_to_range g) @ (class_flatten c')

(* used by the parser *)
(* what happens when two elements are separated with a dash? *)
(* if it's two characters, it becomes a range *)
(* otherwise, the dash needs to be interpreted as a '-' character *)
let make_range (e1:char_class_elt) (e2:char_class_elt) : char_class =
  match e1, e2 with
  | CChar c1, CChar c2 -> [CRange (c1,c2)]
  | _, _ -> [e1; CChar '-'; e2]



(** * Character Acceptance  *)

let rec is_in_range (c:char) (l:(char*char) list) : bool =
  match l with
  | [] -> false
  | (ch1,ch2)::l' ->
     if (c < ch1) then false    (* the list is assumed to be ordered *)
     else if (c >= ch1 && c <= ch2) then true
     else is_in_range c l'

(* is a read character accepted by an expectation *)
let is_accepted (read:char option) (ce:char_expectation): bool =
  match read,ce with
  | None, _ -> failwith "expected a character when consuming blocked thread"
  | _, All -> true
  | Some r, Single e -> r = e
  | Some r, Ranges l -> is_in_range r l


(** * Range Construction  *)

(* assumes that the list is ordered (according to the first element of the pairs *)
(* and that current has the smallest first element *)
(* assumes that pairs are correctly set (the first element is smaller) *)
let rec build_range (current:char*char) (next:(char*char) list) : (char*char) list =
  let (cstart,cend) = current in
  if cend = max_char then [current] else
    begin match next with
    | [] -> [current]
    | (nstart,nend)::next' ->
       (* we know nstart >= ctsart *)
       if (nstart > next_char cend) then
         (* disjoint ranges *)
         current::(build_range (nstart,nend) next')
       else
         (* extend from the end *)
         build_range (cstart, char_max cend nend) next'
    end

let class_to_range (c:char_class) : (char*char) list =
  let lranges = class_flatten c in
  let lsort = List.sort (fun (start1,e1) (start2,e2) -> if start1 > start2 then 1 else -1) lranges in
  match lsort with
  | [] -> []
  | head::tail -> build_range head tail



(** * Pretty Printing  *)

(* for printing inside character classes *)
let print_class_char (c:char) : string =
  if (int_of_char c = 0) then "\\x00"
  else if (c = '\'') then "\\x27" (* to avoid weird bash issues *)
  else if (c = '\"') then "\\x22" (* to avoid weird bash issues *)
  else if (is_ascii_word_character c) then String.make 1 c
  else "\\"^String.make 1 c        (* escaping everything but word characters *)


let rec ranges_to_string (l:(char*char) list) : string =
  match l with
  | [] -> ""
  | (cstart,cend)::[] -> "("^print_class_char cstart^","^print_class_char cend^")"
  | (cstart,cend)::next -> "("^print_class_char cstart^","^print_class_char cend^");"^
                             ranges_to_string next

(* for debugging *)
let rec ranges_to_int_string (l:(char*char) list) : string =
  match l with
  | [] -> ""
  | (cstart,cend)::[] -> "("^string_of_int(int_of_char cstart)^","^string_of_int(int_of_char cend)^")"
  | (cstart,cend)::next -> "("^string_of_int(int_of_char cstart)^","^string_of_int(int_of_char cend)^");"^
                             ranges_to_int_string next


let expectation_to_string (ce:char_expectation) : string =
  match ce with
  | All -> "All"
  | Single x -> "Single " ^ String.make 1 x
  | Ranges l -> "Ranges [" ^ ranges_to_string l ^ "] [" ^ ranges_to_int_string l^"]"

let print_group (g:char_group) : string =
  match g with
  | Digit -> "\\d"
  | NonDigit -> "\\D"
  | Word -> "\\w"
  | NonWord -> "\\W"
  | Space -> "\\s"
  | NonSpace -> "\\S"


let print_class_elt (e:char_class_elt) : string =
  match e  with
  | CChar x -> print_class_char x
  | CRange (cstart,cend) ->
     print_class_char cstart ^ "-" ^ print_class_char cend
  | CGroup g -> print_group g

let print_class (c:char_class) : string =
  let res = ref "" in
  List.iter (fun e -> res := !res ^ print_class_elt e) c;
  !res
