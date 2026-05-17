
open Array

module Regsdata2 : sig
  type t = 
  | Complete of {
      a_cp: int Array.t;
      a_clk: int Array.t
    }
  | Incomplete of {
      size: int;
      l: (int * int *int) list
    }

  type p = int

  val neutral_element : t
  val compress : p -> t -> t -> t
  val to_string : t -> string
  val size_of : t -> int
  val to_arrays : int -> t -> int Array.t * int Array.t
  val get_cp_at : t -> int -> int
  val get_clk_at : t -> int -> int
end = struct

  type t = 
  | Complete of {
      a_cp: int Array.t;
      a_clk: int Array.t
    } (*arrays*)

  | Incomplete of {
      size: int;
      (* first int: the key *)
      (* second int: the cp value *)
      (* third int: the clock value *)
      l: (int * int *int) list
    }

  type p = int

  let neutral_element : t = Incomplete({size=0; l=[]})

  let to_string (v: t) : string =
    match v with 
    | Complete arrays ->
      let s = ref "CP: " in
      for c = 0 to (Array.length arrays.a_cp)-1 do
        s := !s ^ string_of_int c ^ ": " ^ string_of_int (arrays.a_cp.(c)) ^ " | "
      done;
      s := !s ^ " CLK: ";
      for c = 0 to (Array.length arrays.a_clk)-1 do
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

  let size_of (v: t) : int =
    match v with 
    | Complete arrays -> (Array.length arrays.a_cp) (* all three arrays have the same length *)
    | Incomplete l -> l.size (* each element of the list contains 3 integers *)

  (* a2 is more recent than a1 *)
  let merge_arrays (a1_cp: int Array.t) (a2_cp: int Array.t) (a1_clk: int Array.t) (a2_clk: int Array.t) : int Array.t * int Array.t =
    (* update a1 *)
    (* how can we distinguish between undefined and cleared ? Do we need to ?*)
    (* -1 : invalid   -2: undefined *)
    let update_oldest_cp (i: int) (new_cp: int): unit =
      if (new_cp <> -2) then a1_cp.(i) <- new_cp
      else () in
    let update_oldest_clk (i: int) (new_clk: int): unit =
      if (new_clk <> -2) then a1_clk.(i) <- new_clk
      else () in

    Array.iteri update_oldest_cp a2_cp;
    Array.iteri update_oldest_clk a2_clk;
    (a1_cp, a1_clk)

  let rec fill_newer_array (l:(int*int*int) list) (a_cp: int Array.t) (a_clk: int Array.t) : unit =
    match l with
    | [] -> ()
    | (i,cp,clk)::l' -> (*l' contains older updates *)
        (* only setting reg values that haven't been set yet *)
        if (a_cp.(i) = -2) then a_cp.(i) <- cp;
        if (a_clk.(i) = -2) then a_clk.(i) <- clk;
        fill_newer_array l' a_cp a_clk

  let rec fill_older_array (l:(int*int*int) list) (a_cp: int Array.t) (a_clk: int Array.t) : unit =
    match l with
    | [] -> ()
    | (i,cp,clk)::l' -> (* l' contains older updates*)
        fill_older_array l' a_cp a_clk; (* filling with older updates in list first *)
        (* overwriting older updates if defined *)
        if (cp <> -2) then a_cp.(i) <- cp;
        if (clk <> -2) then a_clk.(i) <- clk        

  (* array is more recent than list: list cannot overwrite array *)
  let compress_new_arrays_old_list (a_cp: int Array.t) (a_clk: int Array.t) (l: (int * int * int) list):
       int Array.t * int Array.t =
    fill_newer_array l a_cp a_clk;
    (a_cp, a_clk)

  let compress_old_arrays_new_lists (a_cp: int Array.t) (a_clk: int Array.t) (l_old: (int * int * int) list) (l_new: (int * int * int) list): 
      int Array.t * int Array.t =
    fill_older_array l_old a_cp a_clk;
    fill_older_array l_new a_cp a_clk;
    (a_cp, a_clk)

  (* list is more recent than array, list overwrites array *)
  (* Problem: how to know if we overwrite array or list ? *)
  (* Solution 1: use doubly linked list like in Théo's paper *)
  (* Solution 2: convert l to array and merge arrays *)
  let compress_new_list_old_arrays (a_cp: int Array.t) (a_clk: int Array.t) (l: (int * int * int) list): int Array.t * int Array.t =
    fill_older_array l a_cp a_clk;
    (a_cp, a_clk)

    (*let size = Array.length a_cp in
    let (l_cp, l_clk) = compress_arrays_and_list (Array.make size (-1)) (Array.make size (-1)) l in
    merge_arrays a_cp l_cp a_clk l_clk*)

  (* t1 is "oldest", t2 is "most recent" *)
  let compress (regs_size: p) (t1: t) (t2: t): t = 
    match t1, t2 with
    | Complete a1, Complete a2 -> (* merge arrays *)
      let (a_cp, a_clk) = merge_arrays a1.a_cp a2.a_cp a1.a_clk a2.a_clk in
      Complete({a_cp=a_cp; a_clk=a_clk})
    | Complete a1, Incomplete l2 -> (* incorporate l2 in a1 *)
      let (a_cp, a_clk) = compress_new_list_old_arrays a1.a_cp a1.a_clk l2.l in
      Complete({a_cp=a_cp; a_clk=a_clk})
    | Incomplete l1, Complete a2 -> (* incorporate l1 into a2 *)
      let (a_cp, a_clk) = compress_new_arrays_old_list a2.a_cp a2.a_clk l1.l in
      Complete({a_cp=a_cp; a_clk=a_clk}) 
    | Incomplete l1, Incomplete l2 ->
      (* concatenate *)
      let size = l2.size + l1.size in
      (* if result has size larger that |r|, convert to complete form *)
      if size >= regs_size then (
        let (a_cp, a_clk) = compress_old_arrays_new_lists (Array.make regs_size (-2)) (Array.make regs_size (-2)) l1.l l2.l in
        Complete({a_cp=a_cp; a_clk=a_clk})
      ) else 
        Incomplete({size=size; l=(l2.l @ l1.l)})

  let to_arrays (size: int) (v: t) : int Array.t * int Array.t =
    match v with 
    | Complete arrays -> (arrays.a_cp, arrays.a_clk)
    | Incomplete l -> compress_new_list_old_arrays (Array.make size (-2)) (Array.make size (-2)) l.l

  let get_cp_at (t: t) (k: int) : int =
    match t with
    | Incomplete l -> (* check if l contains an update for k *)
      let rec get_rec (l:(int*int*int) list) : int =
        match l with
        | [] -> -1
        | (kl,cp,clk)::l' ->
           if (kl = k) then cp
           else get_rec l' in
      get_rec l.l
    | Complete a -> a.a_cp.(k)

  let get_clk_at (t: t) (k: int) : int =
    match t with
    | Incomplete l -> (* check if l contains an update for k *)
      let rec get_rec (l:(int*int*int) list) : int =
        match l with
        | [] -> -1
        | (kl,cp,clk)::l' ->
           if (kl = k) then clk
           else get_rec l' in
      get_rec l.l
    | Complete a -> a.a_clk.(k)

end