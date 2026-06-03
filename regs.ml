(** * Thread register implementations  *)
(* hold information for either capture groups, lookarounds or quantifiers *)
(* for each of these, we want to remember both a cp and a clock value *)

(* Each of these  data-structures will contain values for at most O(r) keys, *)
(* be it capture groups, lookarounds or quantifiers *)

(* For each of these data-structures, there will be at most O(r*s) insertions *)
(* And there will be at most O(r*r) different active values at the same time *)

open Array
open Map
open Spacebench


module IntMap = Map.Make(struct type t = int let compare = compare end)


module type REGS =
  sig
    type regs
    val init_regs: int -> regs
    val set_reg: regs -> int -> int option -> int -> regs
    val clear_reg: regs -> int -> regs
    (* we might remove get_cp and get_clock and always convert to an array first for filtering *)
    val get_cp: regs -> int -> int option
    val get_clock: regs -> int -> int option
    val copy: regs -> regs
    val delete: regs -> unit
    val to_arrays: regs -> int Array.t * int Array.t
    val to_string: regs -> string (* for debugging purposes *)
    val name: string
  end

(* several implementations represent None with -1 *)
(* all other integer values for cp and clock are positive *)
let int_of_opt (o: int option): int =
  match o with
  | None -> -1
  | Some x -> x

let opt_of_int (i:int) : int option =
  if i < 0 then None else Some i
   
module Array_Regs =
  struct
    (* all the values stored are positive *)
    (* -1 represents the absence of a value *)
    type regs =
      { a_cp: int Array.t;
        a_clk: int Array.t }

    (* O(r) *)
    let init_regs (size:int) : regs =
      space_increment size;
      { a_cp = Array.make size (-1); a_clk = Array.make size (-1)}

    (* O(1) *)
    let set_reg (regs:regs) (k:int) (cp:int option) (clk:int) : regs =
      regs.a_cp.(k) <- int_of_opt cp;
      regs.a_clk.(k) <- clk;
      regs

    (* O(1) *)
    let clear_reg (regs:regs) (k:int) : regs =
      regs.a_cp.(k) <- -1;
      regs.a_clk.(k) <- -1;
      regs

    (* O(1) *)
    let get_cp (regs:regs) (k:int) : int option =
      let v = regs.a_cp.(k) in
      opt_of_int v

    (* O(1) *)
    let get_clock (regs:regs) (k:int) : int option =
      let v = regs.a_clk.(k) in
      opt_of_int v

    (* O(r) *)
    let copy (regs:regs) : regs =
      space_increment (Array.length regs.a_cp);
      { a_cp = Array.copy regs.a_cp; a_clk = Array.copy regs.a_clk }

    (* O(1) *)
    let delete (regs:regs) : unit = 
      space_decrement (Array.length regs.a_cp)

    (* O(1) *)
    let to_arrays (regs:regs) : int Array.t * int Array.t =
      (regs.a_cp, regs.a_clk)

    let to_string (regs:regs) : string =
      let s = ref "" in
      for c = 0 to (Array.length regs.a_cp)-1 do
        s := !s ^ string_of_int c ^ ": " ^ string_of_int (regs.a_cp.(c)) ^ " | "
      done;
      !s

    let name : string = "ArrayRegs"
  end

module List_Regs =
  struct
    type regs =
      (* first int: the key *)
      (* second int: the cp value *)
      (* third int: the clock value *)
      { mutable setlist: (int * int * int) list;
        size: int }

    (* O(1) *)
    let init_regs (size:int) : regs =
     {setlist = []; size = size}

    (* O(1) *)
    let set_reg (regs:regs) (k:int) (cp:int option) (clk:int): regs =
      regs.setlist <- (k,int_of_opt cp,clk)::regs.setlist;
      regs

    (* O(1) *)
    let clear_reg (regs:regs) (k:int) : regs =
      regs.setlist <- (k,-1,-1)::regs.setlist;
      regs

    (* O(r*s) *)
    let get_cp (regs:regs) (k:int) : int option =
      let rec get_rec (l:(int*int*int) list) : int option =
        match l with
        | [] -> None
        | (kl,cp,clk)::l' ->
           if (kl = k) then opt_of_int cp
           else get_rec l' in
      get_rec regs.setlist

    (* O(r*s) *)
    let get_clock (regs:regs) (k:int) : int option =
      let rec get_rec (l:(int*int*int) list) : int option =
        match l with
        | [] -> None
        | (kl,cp,clk)::l' ->
           if (kl = k) then opt_of_int clk
           else get_rec l' in
      get_rec regs.setlist

    (* O(1) *)
    let copy (regs:regs) : regs =
      { setlist = regs.setlist; size = regs.size }

    (* O(1) *)
    let delete (regs:regs) : unit = ()

    (* O(r*s) *)
    let to_arrays (regs:regs) : int Array.t * int Array.t =
      let a_cp = Array.make regs.size (-1) in
      let a_clk = Array.make regs.size (-1) in
      let rec fill_array (l:(int*int*int) list) : unit =
        match l with
        | [] -> ()
        | (k,cp,clk)::l' ->
           (* only setting reg values that haven't been set yet *)
           if (a_cp.(k) = -1) then a_cp.(k) <- cp;
           if (a_clk.(k) = -1) then a_clk.(k) <- clk;
           fill_array l' in

      (*let rec fill_array2 (l: (int*int*int) list) : unit =
        match l with 
        | [] -> ()
        | (k, cp, clk) :: l' ->
          fill_array2 l'; (* fill with less recent updates first *)
          a_cp.(k) <- cp;
          a_clk.(k) <- clk in
      fill_array2 regs.setlist;*)
      fill_array regs.setlist;
      (a_cp, a_clk)

    let to_string (regs:regs) : string =
      let rec to_string_rec (l:(int*int*int) list) : string =
      match l with
      | [] -> ""
      | (k,cp,clk)::l' ->
         "(" ^ string_of_int k ^ "," ^ string_of_int cp ^ ")::" ^ to_string_rec l'
      in
      to_string_rec regs.setlist

    let name : string = "ListRegs"
  end


module Map_Regs =
  struct
    type regs =
      (* first int: cp value, second int: clk value *)
      { mutable valmap : (int * int) IntMap.t;
        size : int }

    (* O(1) *)
    let init_regs (size:int) : regs =
      { valmap = IntMap.empty; size = size }

    (* O(log r) *)
    let set_reg (regs:regs) (k:int) (cp:int option) (clk:int) : regs =
      regs.valmap <- IntMap.add k (int_of_opt cp, clk) regs.valmap;
      regs

    (* O(log r) *)
    let clear_reg (regs:regs) (k:int) : regs =
      regs.valmap <- IntMap.remove k regs.valmap;
      regs

    (* O(log r) *)
    let get_cp (regs:regs) (k:int) : int option =
      match (IntMap.find_opt k regs.valmap) with
      | None -> None
      | Some (cp,clk) -> opt_of_int cp

    (* O(log r) *)
    let get_clock (regs:regs) (k:int) : int option =
      match (IntMap.find_opt k regs.valmap) with
      | None -> None
      | Some (cp,clk) -> opt_of_int clk

    (* O(1) *)
    let copy (regs:regs) : regs =
      { valmap = regs.valmap; size = regs.size }

    (* O(1) *)
    let delete (regs:regs) : unit = ()

    (* O(r) *)
    let to_arrays (regs:regs) : int Array.t * int Array.t =
      let a_cp = Array.make regs.size (-1) in
      let a_clk = Array.make regs.size (-1) in
      IntMap.iter (fun k (cp,clk) ->
          a_cp.(k) <- cp;
          a_clk.(k) <- clk) regs.valmap;
      (a_cp, a_clk)
            
    let to_string (regs:regs) : string =
      let s = ref "" in
      for c = 0 to regs.size do
        s := !s ^ string_of_int c ^ ": " ^ string_of_int (int_of_opt (get_cp regs c)) ^ " | "
      done;
      !s

    let name : string = "MapRegs"
  end


(* ///////////////////////////////////////////////////////////////// *)

open Virtual_tree
open Regsdata2

module Regs_Vt = Virtual_tree(Regsdata2)

module Virtual_Tree_Regs = 
  struct
    type regs = 
    {
      mutable leaf: Regs_Vt.tree;
      size: int
    }

    let init_regs (size: int): regs =
      { leaf = Regs_Vt.empty size; size = size}

    let set_reg (regs:regs) (k:int) (cp:int option) (clk:int) : regs =
      (match (Regs_Vt.get_unshared_data regs.leaf) with
      | Some d -> (
        match d with
        | Complete arrays -> (* reuse memory : directly update the array *)
          arrays.a_cp.(k) <- int_of_opt cp;
          arrays.a_clk.(k) <- clk
        | Incomplete _ -> (* we must insert here *)
          let size_inserted = Regs_Vt.insert regs.leaf (Regsdata2.Incomplete({size=1; l=[(k, int_of_opt cp, clk)]})) in
          space_increment size_inserted
      )
      | None -> 
        (* the leaf has no unshared data: must insert k cp and clk in tree *)
        let size_inserted = Regs_Vt.insert regs.leaf (Regsdata2.Incomplete({size=1; l=[(k, int_of_opt cp, clk)]})) in
        space_increment size_inserted
      );
      (*let size_inserted = Regs_Vt.insert regs.leaf (Regsdata2.Incomplete({size=1; l=[(k, int_of_opt cp, clk)]})) in
      space_increment size_inserted;*)
      regs

    let clear_reg (regs:regs) (k:int) : regs = 
      (* insert -1 in tree *)
      let size_inserted = Regs_Vt.insert regs.leaf (Regsdata2.Incomplete({size=1; l=[(k, -1, -1)]})) in
      space_increment size_inserted;
      regs

    (* we might remove get_cp and get_clock and always convert to an array first for filtering *)
    let get_cp (regs: regs) (k: int) : int option =
      (* get deepest (most recent) such that it is defined *)
      match (Regs_Vt.get_deepest_such_that regs.leaf (fun x -> (Regsdata2.get_cp_at x k) <> -2)) with
      | Some d -> opt_of_int (Regsdata2.get_cp_at d k)
      | None -> None

    let get_clock (regs: regs) (k: int) : int option =
      match (Regs_Vt.get_deepest_such_that regs.leaf (fun x -> (Regsdata2.get_clk_at x k) <> -2)) with
      | Some d -> opt_of_int (Regsdata2.get_clk_at d k)
      | None -> None

    let copy (regs: regs): regs =
      (* call split and return result *)
      { leaf = Regs_Vt.split regs.leaf; size = regs.size}

    let delete (regs: regs): unit =
      let size_deleted = Regs_Vt.delete regs.leaf in
      space_decrement size_deleted

    let to_arrays (regs: regs): int Array.t * int Array.t =
      match (Regs_Vt.get_compressed_data regs.leaf) with
      | Some dt -> Regsdata2.to_arrays regs.size dt
      | None -> (Array.make regs.size (-1), Array.make regs.size (-1))

    let to_string (regs: regs): string =
      Regs_Vt.to_string regs.leaf

    let name: string = "VirtualTreeRegs"
  end
