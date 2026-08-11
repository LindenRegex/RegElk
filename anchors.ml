(** * Anchors  *)
(* here we describe how to match the different Anchors *)
(* Anchors need a two-character context: which characters are *)
(* just before and just after the current position *)

open Regex
open Charclasses


(* our algorithms can go through the string in two possible directions *)
(* this changes the behaviors of the anchors *)
type direction =
  | Forward
  | Backward


(** * Char Contexts  *)
(* a context here describes the surrounding characters of a position (or None, if we reached the end/begin of the input) *)
(* this surrounding context is what the interpreter needs to evaluate anchors, and advance threads that reached a CONSUME instruction *)

type char_context =
  {
    mutable prevchar : char option;
    mutable nextchar : char option;
  }
(* NOTE: when going backward in the string, the index of nextchar is smaller than the one of prevchar *)

let update_context (ctx:char_context) (newchar:char option): unit =
  ctx.prevchar <- ctx.nextchar;
  ctx.nextchar <- newchar

let print_context (ctx:char_context) : string =
  "\027[36mContext:\027[0m " ^
    begin match ctx.prevchar,ctx.nextchar with
    | None,None -> "{None,None}"
    | None,Some x -> "{None,"^String.make 1 x^"}"
    | Some x,None -> "{"^String.make 1 x^",None}"
    | Some x, Some y -> "{"^String.make 1 x^","^String.make 1 y^"}"
    end ^ "\n"


(** * Checking Anchor Assertions  *)

let is_boundary (ctx:char_context) : bool =
  match ctx.prevchar, ctx.nextchar with
  | None, None -> false
  | None, Some c -> is_ascii_word_character c
  | Some c, None -> is_ascii_word_character c
  | Some prev, Some next ->
     (is_ascii_word_character prev) <> (is_ascii_word_character next) (* xor *)

let is_satisfied (a:anchor) (ctx:char_context) (dir:direction): bool =
  match a,dir with
  | BeginInput,Forward -> ctx.prevchar = None
  | BeginInput,Backward -> ctx.nextchar = None
  | EndInput,Forward -> ctx.nextchar = None
  | EndInput,Backward -> ctx.prevchar = None
  | WordBoundary,_ -> is_boundary ctx
  | NonWordBoundary,_ -> not (is_boundary ctx)
