
module Virtual_tree (CData : sig
  type t
  type p

  (* argument 1: extra parameter for compress *)
  (* argument 2: oldest t value (closest to root) *)
  (* argument 3: most recent t value (closest to leaf) *)
  val compress : p -> t -> t -> t
  val copy : t -> t

  val to_string : t -> string (* debugging purposes *)
end) : sig 
  type tree
  val empty_tree : CData.p -> tree
  val initial_tree : CData.p -> tree
  val is_empty : tree -> bool
  val is_minimal_usable : tree -> bool

  val split : tree -> tree
  val insert : tree -> CData.t -> unit
  val delete : tree -> unit

  val get_compressed_data : tree -> CData.t option
  val get_deepest_such_that : tree -> (CData.t -> bool) -> CData.t option
  val get_unshared_data : tree -> CData.t option

  (* debugging *)
  val print : tree -> unit
  val to_string : tree -> string
  val depth : tree -> int
  val node_depth : tree -> int
end = struct

  (* Virtual tree type *)
  type tree = 
  (* A tree has a unique Root, the ancestor of all other elements of the tree *)
  | Root of CData.p
  (* Nodes contain data *)
  | Node of {
    id : int;
    param: CData.p;
    mutable parent : tree;
    mutable child : tree;
    mutable data : CData.t
  }
  (* Branches split into two subtrees and express the tree structure *)
  | Branch of {
    id : int;
    param: CData.p;
    mutable parent : tree;
    mutable left : tree;
    mutable right : tree
  }
  (* Leaves identify a path in the tree and are the handling points of the tree *)
  | Leaf of {
    id : int;
    mutable parent : tree
  }

  let get_id (t: tree): int = 
    match t with 
    | Root _ -> 0
    | Node n -> n.id
    | Branch b -> b.id
    | Leaf l -> l.id

  let next_id =
    let counter = ref 1 in (* id 0 is reserved for Root *)
    fun () ->
      incr counter;
      !counter

  (* Returns an empty tree *)
  let empty_tree (param: CData.p) : tree = 
    Root(param)

  (* Returns a minimal usable tree *)
  let initial_tree (param: CData.p) : tree = 
    Leaf({id=next_id(); parent=Root(param)})

  let is_empty (t: tree): bool =
    match t with 
    | Root _ -> true
    | Node _ | Branch _ | Leaf _ -> false

  let is_minimal_usable (t: tree): bool =
    match t with 
    | Root _ | Node _ | Branch _ -> false
    | Leaf l ->
      match l.parent with 
      | Root _ -> true
      | _ -> false

  (* Two tree elements are equal is they have the same id *)
  let equal (t1: tree) (t2: tree): bool = (get_id t1) = (get_id t2)

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

  let get_param (t: tree) : CData.p =
    match t with
    | Root p -> p 
    | Node n -> n.param 
    | Branch b -> b.param 
    | Leaf _ -> invalid_arg "Leaves do not know the Data parameter"

  let get_data (t: tree) : CData.t =
    match t with
    | Root p -> invalid_arg "The Root does not store data"
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

  (* Splits a leaf into two subtrees
  ** Replaces the leaf with a branch whose children are the input leaf and a new leaf
  ** Throws [Invalid_argument] if the input tree is not a Leaf
  ** Throws [Failure] if the tree is in an illegal state *)
  let split (leaf : tree) : tree = 
    match leaf with 
    | Root _ | Node _ | Branch _  -> invalid_arg "The argument tree must be a Leaf."
    | Leaf l -> 
      let new_leaf = Leaf({id=next_id(); parent=l.parent}) in
      let b = Branch({id=next_id(); param=(get_param l.parent); parent=l.parent; left=leaf; right=new_leaf}) in
      update_child_in_parent leaf b; (* now leaf's parent's child is b *)
      update_parent_in_child new_leaf b; (* now new_leaf's parent is b*)
      l.parent <- b; (* now leaf's parent is b *)
      new_leaf

  (* Inserts some new data into the tree for a leaf 
  ** Compresses the new data with existing data if possible
  ** Throws [Invalid_argument] if the input tree is not a Leaf
  ** Throws [Failure] if the tree is in an illegal state *)
  let insert (leaf : tree) (new_data : CData.t) : unit =
    match leaf with 
    | Root _ | Node _ | Branch _ -> invalid_arg "The argument tree must be a Leaf."
    | Leaf l -> (
      match l.parent with 
      | Root p -> (* create a node pointing to Root, reroute leaf to that node *)
        let new_node = Node({id=next_id(); param=p; parent=l.parent; child=leaf; data=new_data}) in
        l.parent <- new_node (* now leaf's parent is new_node *)
      | Node n -> (* update n's data: compress it with new_data *)
        n.data <- CData.compress n.param n.data new_data (* no need to copy data: reusing memory *)
      | Branch b -> (* create a node pointing to b, reroute leaf and b to that node *)
        let new_node = Node({id=next_id(); param=b.param; parent=l.parent; child=leaf; data=new_data}) in
        update_child_in_parent leaf new_node; (* now leaf's parent's child is new_node *)
        l.parent <- new_node (* now leaf's parent is new_node *)
      | Leaf _ -> failwith "Illegal state: a Leaf cannot be a parent."
    )

  (* Compress two Nodes into one by compressing their data and modifying the tree structure
  ** Used when the input Nodes are separated by a Branch to be deleted
  ** n_old is closer to the Root, n_new is closer to the leaves
  ** Modifies and returns the n_old *)
  let merge_and_compress_nodes (n_old : tree) (n_new : tree) : unit =
    match n_old, n_new with 
    | Node n1, Node n2 ->
      n1.data <- CData.compress n1.param n1.data n2.data; (* no need to copy data: reusing memory *)
      update_parent_in_child n2.child n_old; (* now n2's child's parent is n1 *)
      n1.child <- n2.child (* now n1's child in n2's child*)
    | _, _ -> ()

  (* Delete to_del subtree and branch from the tree.
  ** to_del must be a subtree of branch.
  ** Branch is replaced by its other child.
  ** Throws [Invalid_argument] if to_del is not a child of branch *)
  let delete_from_branch (to_del : tree) (branch : tree) : unit =
    match branch with 
    | Root _ | Node _ -> ()
    | Branch b -> 
      let other_child = if (equal b.left to_del) 
                        then b.right 
                        else  if (equal b.right to_del) 
                              then b.left 
                              else invalid_arg "Argument to_del is not a child of branch."
        in
      if is_node other_child && is_node b.parent then ((* must merge the two nodes and compress their data *)
        merge_and_compress_nodes b.parent other_child
      ) else (
        update_child_in_parent branch other_child; (* now branch's parent's child is other_child *)
        update_parent_in_child other_child b.parent (* now other_child's parent is b.parent*)
      )
    | Leaf _ -> ()

  (* Deletes the stem for the input leaf.
  ** Deletes the leaf itself and its ancestors until the first Branch, which is replaced by its other child.
  ** If the leaf has no Branch ancestor, only the data is deleted (the Node ancestor) and the tree becomes minimal.
  ** Deleting might trigger a compression higher in the tree.
  ** Throws [Invalid_argument] if the input tree is not a Leaf.
  ** Throws [Failure] if the tree is in an illegal state. *)
  let delete (leaf: tree) : unit =
    match leaf with
    | Root _ | Node _ | Branch _ -> invalid_arg "The argument tree must be a Leaf."
    | Leaf l -> (
      match l.parent with 
      | Root _ -> () (* the tree stays unchanged in this case *)
      | Node n -> (
        match n.parent with 
        | Root _ -> (* delete only the data: make leaf point to Root *)
          l.parent <- n.parent
        | Node n -> failwith "Illegal state: A Node's parent cannot be a Node." 
        | Branch b -> (* Replace the Branch with its other child. Compress if possible. *)
          delete_from_branch l.parent n.parent
        | Leaf _ -> failwith "Illegal state: a Leaf cannot be a parent."
      )
      | Branch b -> (* Replace the Branch with its other child. Compress if possible. *)
        delete_from_branch leaf l.parent
      | Leaf _ -> failwith "Illegal state: a Leaf cannot be a parent."
    )

  (* Returns the data for the input leaf 
  ** The data for a leaf is the compresson of the data of all of its ancestors from the Root to the leaf itself
  ** Returns None if none the leaf's ancestors are Nodes
  ** This operation does not modify the tree
  ** Throws [Invalid_argument] if the input tree is not a Leaf. *)
  let get_compressed_data (leaf: tree): CData.t option =
    match leaf with
    | Root _ | Node _ | Branch _ -> invalid_arg "The argument tree must be a Leaf."
    | Leaf _ -> 
      let rec get_rec (t: tree): CData.t option =
        match t with 
        | Root _ -> None
        | Node n -> (
          match (get_rec n.parent) with
          (* the data must be copied otherwise it might get modified by the compression operation *)
          | Some dt -> Some (CData.compress n.param dt (CData.copy n.data))
          | None -> Some (CData.copy n.data)
        )
        | Branch b -> get_rec b.parent
        | Leaf l -> get_rec l.parent 
      in
      get_rec leaf

  (* Returns the data of the first ancestor of the input leaf satisfying the condition f, if it exists.
  ** Throws [Invalid_argument] if the input tree is not a Leaf. *)
  let get_deepest_such_that (leaf: tree) (f: CData.t -> bool): CData.t option =
    match leaf with
    | Root _ | Node _ | Branch _ -> invalid_arg "The argument tree must be a Leaf."
    | Leaf _ ->
      let rec get_rec (t: tree) (f: CData.t -> bool): CData.t option =
        match t with 
        | Root _ -> None
        | Node n -> 
          if (f n.data) then Some n.data (* no copy : might get modified by user *)
          else get_rec n.parent f
        | Branch b -> get_rec b.parent f
        | Leaf l -> get_rec l.parent f
      in 
      get_rec leaf f

  (* Returns the data for the input leaf that is shared with no other leaf, if it exists.
  ** Throws [Invalid_argument] if the input tree is not a Leaf. *)
  let get_unshared_data (leaf: tree) : CData.t option =
    match leaf with
    | Root _ | Node _ | Branch _ -> invalid_arg "The argument tree must be a Leaf."
    | Leaf l -> (
      match l.parent with
      | Root _ -> None
      | Node n -> Some n.data (* no copy : might get modified by user *)
      | _ -> None
    )

  let rec print (t: tree): unit =
    match t with 
    | Root _ -> Printf.printf "Root\n"
    | Node n -> 
      Printf.printf "Node %d (%s) -> " n.id (CData.to_string n.data);
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
    | Node n -> (Printf.sprintf "Node %d (%s) -> " n.id (CData.to_string n.data)) ^ (to_string n.parent)
    | Branch b -> (Printf.sprintf "Branch %d -> " b.id) ^ (to_string b.parent)
    | Leaf l -> (Printf.sprintf "Leaf %d -> " l.id) ^ (to_string l.parent)

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

end