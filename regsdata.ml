(** * Register data *)
(* A compressible data type for register updates *)

open Array

module Regsdata : sig
  (* Data type *)
  type t = 
  | Complete of {
      a_cp: int Array.t;
      a_clk: int Array.t
    }
  | Incomplete of {
      size: int;
      l: (int * int * int) list
    }
  (* Parameter type *)
  type p = int

  (* Virtual tree required functions *)
  val compress : p -> t -> t -> t
  val copy : t -> t

  (* Helpers for regs *)
  val to_arrays : int -> t -> int Array.t * int Array.t
  val get_cp_at : t -> int -> int
  val get_clk_at : t -> int -> int

  (* Debugging purposes *)
  val to_string : t -> string
end = struct

  (* Data type *)
  (* Represents a partial register state: a state is complete once all its partial states are compressed
  ** The incomplete form contains register updates 
  ** The complete form contains register arrays *)
  (* The register values can be:
  ** -2: undefined
  ** -1: invalid (or cleared)
  ** A non-negative integer: a valid value *)
  type t = 
  | Complete of {
      (* arrays of register values *)
      a_cp: int Array.t;
      a_clk: int Array.t
    }
  | Incomplete of {
      size: int;
      (* list of updates *)
      (** first int: the key *)
      (** second int: the cp value *)
      (** third int: the clock value *)
      l: (int * int *int) list
    }

  (* Parameter type *)
  (* Here, the parameter represents the registers size *)
  type p = int

  (* Merges the cp arrays together and the clk arrays together *)
  (* The new arrays overwrite the old arrays *)
  (* Modifies and returns the a_old_cp and a_old_clk arrays *)
  let merge_arrays (a_old_cp: int Array.t) (a_new_cp: int Array.t) 
                   (a_old_clk: int Array.t) (a_new_clk: int Array.t) : int Array.t * int Array.t =
    let update_oldest_cp (i: int) (new_cp: int): unit =
      if (new_cp <> -2) then a_old_cp.(i) <- new_cp (* overwrite only if new_cp is defined *)
      else () in
    let update_oldest_clk (i: int) (new_clk: int): unit =
      if (new_clk <> -2) then a_old_clk.(i) <- new_clk (* overwrite only if new_clk is defined *)
      else () in

    Array.iteri update_oldest_cp a_new_cp;
    Array.iteri update_oldest_clk a_new_clk;
    (a_old_cp, a_old_clk)

  (* Merge a list of updates with a more recent array *)
  (* The list only overwrites undefined array values *)
  (* Modifies the a_cp and a_clk arrays *)
  let rec fill_newer_array (l:(int*int*int) list) (a_cp: int Array.t) (a_clk: int Array.t) : unit =
    match l with
    | [] -> ()
    | (i,cp,clk)::l' -> (* l' contains older updates *)
        (* only update register values that are undefined *)
        if (a_cp.(i) = -2) then a_cp.(i) <- cp;
        if (a_clk.(i) = -2) then a_clk.(i) <- clk;
        fill_newer_array l' a_cp a_clk

  (* Merge a list of updates with an older array *)
  (* The list overwrites the array *)
  (* Modifies the a_cp and a_clk arrays *)
  let rec fill_older_array (l:(int*int*int) list) (a_cp: int Array.t) (a_clk: int Array.t) : unit =
    match l with
    | [] -> ()
    | (i,cp,clk)::l' -> (* l' contains older updates *)
        (* updating the array with older updates first *)
        fill_older_array l' a_cp a_clk;
        (* overwriting older values if the new values are defined *)
        if (cp <> -2) then a_cp.(i) <- cp;
        if (clk <> -2) then a_clk.(i) <- clk        

  (* Merge a list of updates with more recent arrays *)
  (* Modifies and returns a_cp and a_clk *)
  let compress_new_arrays_old_list (a_cp: int Array.t) (a_clk: int Array.t) (l: (int * int * int) list):
       int Array.t * int Array.t =
    fill_newer_array l a_cp a_clk;
    (a_cp, a_clk)

  (* Merge a list of updates with older arrays *)
  (* Modifies and returns a_cp and a_clk *)
  let compress_new_list_old_arrays (a_cp: int Array.t) (a_clk: int Array.t) (l: (int * int * int) list): int Array.t * int Array.t =
    fill_older_array l a_cp a_clk;
    (a_cp, a_clk)

  (* Converts two lists of updates to registers arrays of size regs_size *)
  let lists_to_arrays (regs_size: p) (l_old: (int * int * int) list) (l_new: (int * int * int) list) : int Array.t * int Array.t =
    let a_cp = Array.make regs_size (-2) in
    let a_clk = Array.make regs_size (-2) in
    fill_newer_array l_new a_cp a_clk;
    fill_newer_array l_old a_cp a_clk;
    (a_cp, a_clk)

  (* Compression function *)
  (* t_old is the oldest value, and t_new is the most recent *)
  (* regs_size is the size of the registers *)
  let compress (regs_size: p) (t_old: t) (t_new: t): t = 
    match t_old, t_new with
    | Complete a_old, Complete a_new -> (* merge arrays *)
      let (a_cp, a_clk) = merge_arrays a_old.a_cp a_new.a_cp a_old.a_clk a_new.a_clk in
      Complete({a_cp=a_cp; a_clk=a_clk})
    | Complete a_old, Incomplete l_new -> (* l_new overwrites a_old *)
      let (a_cp, a_clk) = compress_new_list_old_arrays a_old.a_cp a_old.a_clk l_new.l in
      Complete({a_cp=a_cp; a_clk=a_clk})
    | Incomplete l_old, Complete a_new -> (* incorporate l_old into a_new where a_new is -2 ("undefined") *)
      let (a_cp, a_clk) = compress_new_arrays_old_list a_new.a_cp a_new.a_clk l_old.l in
      Complete({a_cp=a_cp; a_clk=a_clk}) 
    | Incomplete l_old, Incomplete l_new ->
      let size = l_new.size + l_old.size in
      (* if result has size larger that the registers size, convert to complete form *)
      if size >= regs_size then (
        let (a_cp, a_clk) = lists_to_arrays regs_size l_old.l l_new.l in
        Complete({a_cp=a_cp; a_clk=a_clk})
      ) else (
        (* concatenate, most recent updates go first *)
        Incomplete({size=size; l=(l_new.l @ l_old.l)})
      )

  (* Copies a value of type t so that the copy can be modified without changes to the original *)
  let copy (t: t) : t =
    match t with
    | Complete arrays -> (* arrays are mutable *)
      Complete({a_cp = Array.copy arrays.a_cp; a_clk = Array.copy arrays.a_clk})
    | Incomplete _ -> t (* lists are immutable *)

  (* Converts a value of type t to registers in their "array" form *)
  let to_arrays (size: int) (v: t) : int Array.t * int Array.t =
    match v with 
    | Complete arrays -> (arrays.a_cp, arrays.a_clk)
    | Incomplete l -> compress_new_list_old_arrays (Array.make size (-2)) (Array.make size (-2)) l.l

  (* Getter for most recent cp value at index k in t *)
  (* Returns -2 if that value is undefined *)
  let get_cp_at (t: t) (k: int) : int =
    match t with
    | Incomplete l -> (* check if l contains an update for k *)
      let rec get_cp_rec (l:(int*int*int) list) : int =
        match l with
        | [] -> -2 (* l has no updates for index k, return "undefined" *)
        | (kl,cp,clk)::l' ->
           if (kl = k) then cp
           else get_cp_rec l' in
      get_cp_rec l.l
    | Complete a -> a.a_cp.(k)

  (* Getter for most recent clk value at index k in t *)
  (* Returns -2 if that value is undefined *)
  let get_clk_at (t: t) (k: int) : int =
    match t with
    | Incomplete l -> (* check if l contains an update for k *)
      let rec get_clk_rec (l:(int*int*int) list) : int =
        match l with
        | [] -> -2 (* l has no updates for index k, return "undefined" *)
        | (kl,cp,clk)::l' ->
           if (kl = k) then clk
           else get_clk_rec l' in
      get_clk_rec l.l
    | Complete a -> a.a_clk.(k)

  (* Converts a value of type t to a string *)
  let to_string (v: t) : string =
    match v with 
    | Complete arrays ->
      let s = ref "CP: " in
      for c = 0 to (Array.length arrays.a_cp) - 1 do
        s := !s ^ string_of_int c ^ ": " ^ string_of_int (arrays.a_cp.(c)) ^ " | "
      done;
      s := !s ^ " CLK: ";
      for c = 0 to (Array.length arrays.a_clk) - 1 do
        s := !s ^ string_of_int c ^ ": " ^ string_of_int (arrays.a_clk.(c)) ^ " | "
      done;
      !s
    | Incomplete l ->
      let rec to_string_rec (l:(int*int*int) list) : string =
      match l with
      | [] -> ""
      | (i,cp,clk)::l' ->
         "(" ^ string_of_int i ^ "," ^ string_of_int cp ^ "," ^ string_of_int clk ^ ")::" ^ to_string_rec l'
      in
      to_string_rec l.l

end