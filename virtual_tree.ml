
module Virtual_tree (Data : sig
  type t
  type p
  (*val neutral_element : t*)
  (* argument 1: extra parameter for compress *)
  (* argument 2: oldest t value (closest to root) *)
  (* argument 3: most recent t value (closest to leaf) *)
  val compress : p -> t -> t -> t

  val to_string : t -> string (* debugging purposes *)
  val size_of : t -> int (* benchmarking purposes *)
end) : sig 
  type tree
  val empty : Data.p -> tree
  val split : tree -> tree
  val insert : tree -> Data.t -> int
  val delete : tree -> int
  val is_empty : tree -> bool
  val get_compressed_data : tree -> Data.t option
  val get_deepest_such_that : tree -> (Data.t -> bool) -> Data.t option

  (* debugging *)
  val print : tree -> unit
  val to_string : tree -> string
  val depth : tree -> int
  val node_depth : tree -> int
end = struct

  let next_id =
    let counter = ref 0 in
    fun () ->
      incr counter;
      !counter

  type data = Data.t

  type tree = 
  | Root of Data.p
  | Node of {
    id : int;
    param: Data.p;
    mutable parent : tree; (* mutable so we can reroute when merging with a branch tree node *)
    mutable child : tree;
    mutable data : data (* needs to be mutable so we can compress it *)
  }
  | Branch of { (* ensures binary tree with unlimited splits *)
    id : int;
    param: Data.p;
    mutable parent : tree;
    mutable left : tree;
    mutable right : tree
  }
  | Leaf of { (* handling point for user *) (* should know if left/right/single child ?? bad idea *)
    id : int;
    mutable parent : tree (* mutable because copy needs to return only one element *)
  }

  let get_id (t: tree): int = 
    match t with 
    | Root _ -> 0
    | Node n -> n.id
    | Branch b -> b.id
    | Leaf l -> l.id

  let equal (t1: tree) (t2: tree): bool = (get_id t1) = (get_id t2)

  let is_empty (t: tree): bool =
    match t with 
    | Root _ -> invalid_arg "t cannot be Root"
    | Node _ | Branch _ -> false
    | Leaf l ->
      match l.parent with 
      | Root _ -> true
      | _ -> false

  let rec print (t: tree): unit =
    match t with 
    | Root _ -> Printf.printf "Root\n"
    | Node n -> 
      Printf.printf "Node %d (%s) -> " n.id (Data.to_string n.data);
      print n.parent
    | Branch b -> 
      Printf.printf "Branch %d -> " b.id;
      print b.parent
    | Leaf l -> 
      Printf.printf "Leaf %d -> " l.id; 
      print l.parent

  let rec to_string (t: tree): string =
    match t with 
    | Root _ -> "Root"
    | Node n -> (Printf.sprintf "Node %d (%s) -> " n.id (Data.to_string n.data)) ^ (to_string n.parent)
    | Branch b -> (Printf.sprintf "Branch %d -> " b.id) ^ (to_string b.parent)
    | Leaf l -> (Printf.sprintf "Leaf %d -> " l.id) ^ (to_string l.parent)

  let empty (param: Data.p) : tree = 
    Leaf({id=next_id(); parent=Root(param)})

  let rec depth (t: tree) : int =
    match t with 
    | Root _ -> 0
    | Node n -> 1 + depth n.parent
    | Branch b -> 1 + depth b.parent
    | Leaf l -> depth l.parent

  let rec node_depth (t: tree) : int =
    match t with 
    | Root _ -> 0
    | Node n -> 1 + node_depth n.parent
    | Branch b -> node_depth b.parent
    | Leaf l -> node_depth l.parent

  let is_leaf (t : tree) : bool =
    match t with 
    | Leaf _ -> true
    | _ -> false

  let is_node (t : tree) : bool =
    match t with 
    | Node _ -> true
    | _ -> false

  let get_parent (child: tree) : tree =
    match child with 
    | Root _ -> child
    | Node n -> n.parent
    | Branch b -> b.parent
    | Leaf l -> l.parent

  let get_param (t: tree) : Data.p =
    match t with
    | Root p -> p 
    | Node n -> n.param 
    | Branch b -> b.param 
    | Leaf _ -> invalid_arg "Leaves do not know the Data parameter"

  let get_data (t: tree) : Data.t =
    match t with
    | Root p -> invalid_arg "Root does not store data"
    | Node n -> n.data
    | Branch b -> invalid_arg "Branches do not store data"
    | Leaf _ -> invalid_arg "Leaves do not store data"

  let update_parent_in_child (child : tree) (new_parent : tree) : unit =
    match child with
    | Root _ -> ()
    | Node n -> n.parent <- new_parent
    | Branch b -> b.parent <- new_parent
    | Leaf l -> l.parent <- new_parent

  let update_child_in_parent (old_child : tree) (new_child : tree) : unit =
    match (get_parent old_child) with 
    | Root _ -> ()
    | Node n -> 
      if (equal n.child old_child) then n.child <- new_child
      else ()
    | Branch b -> 
      if (equal b.left old_child) then (b.left <- new_child)
      else if (equal b.right old_child) then (b.right <- new_child)
      else ()
    | Leaf l -> failwith "Illegal state: a Leaf cannot be a parent."

  let split (leaf : tree) : tree = 
    match leaf with 
    | Root _ | Node _ | Branch _  -> invalid_arg "leaf must be a Leaf."
    | Leaf l -> 
      let new_leaf = Leaf({id=next_id(); parent=l.parent}) in
      let b = Branch({id=next_id(); param=(get_param l.parent); parent=l.parent; left=leaf; right=new_leaf}) in
      update_child_in_parent leaf b; (* now leaf's parent's child is b *)
      update_parent_in_child new_leaf b; (* now new_leaf's parent is b*)
      l.parent <- b;
      new_leaf

  let insert (leaf : tree) (new_data : data) : int =
    match leaf with 
    | Root _ | Node _ | Branch _ -> invalid_arg "leaf must be a Leaf."
    | Leaf l -> (
      match l.parent with 
      | Root p -> (* create a node pointing to Root, reroute leaf to that node *)
        (*let dt : data = Data.compress p Data.neutral_element new_data in TODO debug: compress modifies neutral_element*)
        let new_node = Node({id=next_id(); param=p; parent=l.parent; child=leaf; data=new_data}) in
        update_parent_in_child leaf new_node; (* now leaf's parent is new_node *)
        Data.size_of new_data
      | Node n -> (* update n's data: compress it with new_data *)
        let deleted_size = Data.size_of n.data in
        n.data <- Data.compress n.param n.data new_data;
        (Data.size_of n.data) - deleted_size;
      | Branch b -> (* create a node pointing to b, reroute leaf and b to that node *)
        let new_node = Node({id=next_id(); param=b.param; parent=l.parent; child=leaf; data=new_data}) in
        update_child_in_parent leaf new_node; (* now leaf's parent's child is new_node *)
        update_parent_in_child leaf new_node; (* now leaf's parent is new_node *)
        Data.size_of new_data
      | Leaf _ -> failwith "Illegal state: a Leaf cannot be a parent."
    )

  (* 
    Compress child and parent into one node 
    The parent becomes the new node
  *)
  let merge_and_compress_nodes (parent : tree) (child : tree) : unit =
    match parent, child with 
    | Node n1, Node n2 -> (* compress n2's data into n1, reroute n2's child to n1 *)
      n1.data <- Data.compress n1.param n1.data n2.data;
      update_parent_in_child n2.child parent; (* now n2's child's parent is n1 *)
      n1.child <- n2.child (* now n1's child in n2's child*)
    | _, _ -> ()

  let delete_from_branch (to_del : tree) (branch : tree) : int =
    match branch with 
    | Root _ | Node _ -> invalid_arg "branch must be a Branch."
    | Branch b -> 
      let size_deleted = ref 0 in
      let other_child = if (equal b.left to_del) then b.right else if (equal b.right to_del) then b.left 
                  else invalid_arg "Argument to_del is not a child of branch."
                in
      if is_node other_child && is_node b.parent then ((* must merge the two nodes and compress their data *)
        size_deleted := (Data.size_of (get_data other_child)) + (Data.size_of (get_data b.parent));
        merge_and_compress_nodes b.parent other_child;
        size_deleted := !size_deleted - (Data.size_of (get_data b.parent))
      ) else (
        update_child_in_parent branch other_child; (* now branch's parent's child is other_child *)
        update_parent_in_child other_child b.parent (* now other_child's parent is b.parent*)
      );
      !size_deleted
    | Leaf _ -> invalid_arg "branch must be a Branch."

  (* deletes all parents until it gets to a Branch *)
  (* if that Branch's parent is a node, compress node's data with branch's other child *)
  (* if there is no branch in the tree, to_delete becomes the empty tree *)
  let rec delete (to_delete : tree) : int =
    match to_delete with 
    | Root _ -> 0
    | Node n -> (
      match n.parent with
      | Root _ -> (* make child (leaf) point to root *)
        update_parent_in_child n.child n.parent;
        Data.size_of n.data
      | Node n -> (* Impossible: to_delete and its parent (a Node) should be compressed into one Node *)
        failwith "Illegal state: A Node's parent cannot be a Node." 
      | Branch b ->
        let deleted_from_compression = delete_from_branch to_delete n.parent in
        deleted_from_compression + (Data.size_of n.data)
      | Leaf _ -> failwith "Illegal state: a Leaf cannot be a parent."
    )
    | Branch _ -> 0 (* branches cannot be deleted, must delete its children separately *)
    | Leaf l -> (
      match l.parent with
      | Root _ -> 0
      | Node n -> (* Delete the node recursively. The node parent is either Root or a Branch *)
        delete l.parent
      | Branch b -> (* Replace the branch with its other child. Compress if possible. *)
        delete_from_branch to_delete l.parent
      | Leaf _ -> failwith "Illegal state: a Leaf cannot be a parent."
    )

  let rec get_compressed_data (t: tree): data option =
    match t with 
    | Root _ -> None
      (*Data.neutral_element TODO debug here: neutral element gets modified by compress, just remove it *)
    | Node n -> (
      match (get_compressed_data n.parent) with
      | Some dt -> Some (Data.compress n.param dt n.data)
      | None -> Some (n.data)
    )
    | Branch b -> get_compressed_data b.parent
    | Leaf l -> get_compressed_data l.parent

  let rec get_deepest_such_that (t: tree) (f: Data.t -> bool): Data.t option =
    match t with 
    | Root _ -> None
    | Node n -> 
      if (f n.data) then Some(n.data)
      else get_deepest_such_that n.parent f
    | Branch b -> get_deepest_such_that b.parent f
    | Leaf l -> get_deepest_such_that l.parent f

end