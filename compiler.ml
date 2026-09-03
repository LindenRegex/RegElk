(** * Bytecode Compiler  *)
(* compile an (annotated) regex to bytecode *)

open Regex
open Bytecode
open Charclasses
open Cdn

(** * Registers *)

(* defining registers corresponding to a given capture group *)
let start_reg (c:capture) : register = 2 * c
let end_reg (c:capture) : register = (2*c) + 1

(** * Compilation data-structure  *)
(* to avoid quadratic-time compilation by concatenating lists, we do the following: *)
(* we build a tree of lists of instructions during compilation *)
(* then we flatten this to a list of instructions *)
(* finally, we'll convert this to an arry of instructions that will be used by the interpreter *)
(* each of these steps should have linear time complexity in the regex *)

type 'x treelist =
  | Leaf of 'x list
  | Concat of ('x treelist) * ('x treelist)

let (@@) x y = Concat (x,y)

let next_id = ref 0
type graph_node = {
  id : int;
  instruction : instruction option;
  mutable neighbors : graph_node list;
  mutable priorities : (int, int) Hashtbl.t;
}
let create_node (instruction: instruction): graph_node =
  let id = !next_id in
  incr next_id;
  {
    id = id;
    instruction = Some instruction;
    neighbors = [];
    priorities = Hashtbl.create 1;
  }
let create_empty_node (): graph_node =
  let id = !next_id in
  incr next_id;
  {
    id = id;
    instruction = None;
    neighbors = [];
    priorities = Hashtbl.create 1;
  }
let reverse_connect (node1:graph_node) (node2:graph_node): graph_node = 
  (* node1 -> node2; *)
  node2.neighbors <- node1::node2.neighbors;
  Hashtbl.replace node1.priorities node2.id (Hashtbl.length node1.priorities);
  node2
let  get_priority src dst =
  Hashtbl.find src.priorities dst.id
let has_priority (source_node: graph_node) (node1: graph_node) (node2: graph_node) : bool =
  let p1 = get_priority source_node node1 in
  let p2 = get_priority source_node node2 in
  p1 < p2
  

(* transforms a treelist into a list *)
(* linear time in the total number of 'x elements in t *)
let rec tl_flatten (t:'x treelist) (tail:'x list): 'x list =
  match t with
  | Leaf l -> l@tail            (* here we will iterate over the elements of l *)
  | Concat (t1,t2) ->
     let l2 = tl_flatten t2 tail in
     tl_flatten t1 l2

(** * Regex to Bytecode Compilation  *)

(* Compilation types *)
type comp_type =
  (* normal compilation type. making progress in the input string (Consume) is allowed *)
  | Progress
  (* Reconstructs the groups of a nulled regex. Recursively compile the nested +  *)
  | ReconstructNulled


(* Recursively compiles a regex *)
(* [fresh] is the next available instruction label *)
(* also returns the next fresh label after compilation *)
(* if the option progress is false, then this generates code that cannot make progress in the string *)
let rec compile_rev (r:regex) (fresh:label) (nextrev:label) (ctype:comp_type) (priority:int Array.t): label * (instruction treelist * label)   =
  match r with
  | Re_empty -> (fresh, (Leaf [], nextrev))
  | Re_character c ->
     if (ctype = ReconstructNulled) then
      ( fresh+1, (Leaf [Fail], nextrev-1))
     else
       begin match c with
       | Char ch -> (fresh+1, (Leaf [Consume (Single ch)], nextrev-1))
       | Dot -> (fresh+1, (Leaf [Consume All], nextrev-1))
       | Group g -> (fresh+1, (Leaf [Consume (Ranges (group_to_range g))], nextrev-1))
       | Class cl -> (fresh+1, (Leaf [Consume (Ranges (class_to_range cl))], nextrev-1))
       | NegClass cl -> (fresh+1, (Leaf [Consume (Ranges (range_neg (class_to_range cl)))], nextrev-1))
       end
  | Re_con (r1, r2) ->
     let (f1,(l1_rev,n1)) = compile_rev r1 fresh nextrev ctype priority in
     let (f2,(l2_rev,n2)) = compile_rev r2 f1 n1 ctype priority in
     (f2, (l2_rev @@ l1_rev, n2))
  | Re_alt (r1, r2) ->
     let (f1,(l1_rev,n1)) = compile_rev r1 (fresh+1) nextrev ctype priority in
     let (f2,(l2_rev,n2)) = compile_rev r2 (f1+1) (n1-1) ctype priority in
     priority.(nextrev) <- nextrev - 1;
     (f2, (Leaf [Fork (n1+1, n2+1)] @@ l2_rev @@ Leaf [Jmp (nextrev+1)] @@ l1_rev, n2-1))

  | Re_quant (_, qid, quant, r1) when ctype = Progress ->
     (* removed special cases for now for simplicity also assumed no min or max are used*)
       
    let (iter_fresh,(iter_code_rev, iter_fresh_rev)) = compile_rev r1 (fresh+3) (nextrev-2) ctype priority in
    let fork = if quant.greedy then Fork(iter_fresh_rev-1, nextrev+1)
                else Fork (nextrev+1,iter_fresh_rev-1) in
    priority.(iter_fresh_rev-2) <- nextrev;
    (* priority doesnt matter in reverse *)
    (iter_fresh+2,(Leaf [fork; SetQuantToClock (qid, false); BeginLoop] @@ iter_code_rev @@ Leaf [EndLoop; Jmp (iter_fresh_rev-2)],iter_fresh_rev-3))
       
  | Re_quant (_, _, _,_) ->
    (fresh, (Leaf [], nextrev))

  | Re_capture (cid, r1) ->
     let (f1,(l1_rev,n1)) = compile_rev r1 (fresh+1) (nextrev-1) ctype priority in
     (f1+1,(Leaf [SetRegisterToCP (start_reg cid)] @@ l1_rev @@ Leaf [SetRegisterToCP (end_reg cid)], n1-1))
     (* compile_rev r1 (fresh) (nextrev) ctype priority *)
  | Re_lookaround (_, _, _) ->
     (fresh, (Leaf [], nextrev))
  | Re_anchor a -> (fresh+1, (Leaf [AnchorAssertion a], nextrev-1)
)
and compile_to_bytecode_rev (r:regex) (code_size:int): code * (int Array.t) =
  (* -1 for starting from 0 and -1 for the accepting state*)
  let priority = Array.make code_size (-1) in
  let (_,(c, _)) = compile_rev r 0 (code_size-2) Progress priority in
  let full_c = tl_flatten c [Accept] in
  (Array.of_list full_c, priority)

and compile_to_graph (r:regex) (start_node: graph_node) (ctype:comp_type) (connect: graph_node->graph_node->graph_node): graph_node  =
  match r with
  | Re_empty -> start_node
  | Re_character c ->
     if (ctype = ReconstructNulled) then connect start_node (create_node Fail)
     else
       begin match c with
       | Char ch -> connect start_node (create_node (Consume (Single ch)))
       | Dot -> connect start_node (create_node (Consume All))
       | Group g -> connect start_node (create_node (Consume (Ranges (group_to_range g))))
       | Class cl -> connect start_node (create_node (Consume (Ranges (class_to_range cl))))
       | NegClass cl -> connect start_node (create_node (Consume (Ranges (range_neg (class_to_range cl)))))
       end
  | Re_con (r1, r2) ->
     let end_node = compile_to_graph r1 start_node ctype connect in
     compile_to_graph r2 end_node ctype connect
  | Re_alt (r1, r2) ->
     let end_node1 = compile_to_graph r1 start_node ctype connect in
     let end_node2 = compile_to_graph r2 start_node ctype connect in
     let final_node = create_empty_node () in
     ignore (connect end_node1 final_node);
     ignore (connect end_node2 final_node);
     final_node

  | Re_quant (nul, qid, quant, r1) when ctype = Progress ->
     if (quant.min > 0 && quant.max = None && nul = NonNullable) then
       begin
         let min_node = repeat_min_graph (quant.min-1) qid r1 start_node ctype connect in
         let body_start_node = create_empty_node () in
         ignore(connect min_node body_start_node);
         let body_end_node = compile_to_graph r1 min_node ctype connect in
         let final_node = create_empty_node () in
         ignore(connect body_end_node final_node);

         if quant.greedy then begin
          ignore(connect body_end_node body_start_node);
          ignore(connect body_end_node final_node );
         end
         else begin
          ignore(connect body_end_node final_node );
          ignore(connect body_end_node body_start_node);
         end;

         final_node
       end
     else if (quant.min > 0 && quant.max = None && nul = CINullable && quant.greedy) then
       begin
         let min_node = repeat_min_graph (quant.min-1) qid r1 start_node ctype connect in
         let fork_node = create_empty_node () in
         ignore(connect min_node fork_node);
         let begin_node = create_node (BeginLoop) in
         ignore(connect fork_node begin_node);
         let body_node = compile_to_graph r1 begin_node ctype connect in
         let end_node = create_node (EndLoop) in
         ignore(connect body_node end_node);
         ignore(connect end_node fork_node);
         fork_node
       end
     else if (quant.min > 0 && quant.max = None && nul = CDNullable && quant.greedy) then
       begin
         let min_node = repeat_min_graph (quant.min-1) qid r1 start_node ctype connect in
         let begin_node = create_node (BeginLoop) in
         let checknull_node = create_node (CheckNullable qid) in
         ignore(connect min_node begin_node);
         ignore(connect min_node checknull_node);

         let body_node = compile_to_graph r1 begin_node ctype connect in
         let end_node = create_node (EndLoop) in
         ignore(connect body_node end_node);
         let final_node = create_empty_node () in
         ignore(connect end_node begin_node);
         ignore(connect end_node final_node);
         ignore(connect checknull_node final_node);
         final_node
       end
     else
       begin
         let min_node = repeat_min_graph quant.min qid r1 start_node ctype connect in
         begin match quant.max with
         | None ->
            let fork_node = create_empty_node () in
            let final_node = create_empty_node () in
            ignore(connect min_node fork_node);
            let begin_node = create_node (BeginLoop) in
            let end_node = create_node (EndLoop) in
            let iter_node = compile_to_graph r1 begin_node ctype connect in
            ignore(connect iter_node end_node);
            ignore(connect end_node fork_node);
            if quant.greedy then begin
              ignore(connect fork_node begin_node);
              ignore(connect fork_node final_node);
            end
            else begin
              ignore(connect fork_node final_node);
              ignore(connect fork_node begin_node);
            end;
            
            final_node
         | Some max ->
            let opt_node = repeat_optional_graph (max-quant.min) qid r1 min_node ctype quant.greedy connect in
            opt_node
         end
       end
  | Re_quant (_, _, _, _) ->
     (* suppose this case wont happen in the reverse_graph creation *)
      start_node
  | Re_capture (_, r1) ->
     compile_to_graph r1 start_node ctype connect
  | Re_lookaround (_, _, _) ->
    (* Todo: should still check the oracle at the position to see if it has the condition *)
     start_node
  | Re_anchor a -> connect start_node (create_node (AnchorAssertion a))

and repeat_min_graph (min:int) (qid:quantid) (r:regex) (start_node:graph_node) (ctype:comp_type) (connect: graph_node->graph_node->graph_node): graph_node =
  if min = 0 then start_node
  else
    let end_node = compile_to_graph r start_node ctype connect in
    let final_node = repeat_min_graph (min-1) qid r end_node ctype connect in
    final_node

and repeat_optional_graph (nb:int) (qid:quantid) (r:regex) (start_node:graph_node) (ctype:comp_type) (greedy:bool) (connect: graph_node->graph_node->graph_node): graph_node =
  if nb = 0 then start_node
  else
    let begin_loop = create_node (BeginLoop) in
    let end1 = compile_to_graph r begin_loop ctype connect in
    let end_loop = create_node (EndLoop) in
    ignore (connect end1 end_loop);
    
    let end2 = repeat_optional_graph (nb-1) qid r end_loop ctype greedy connect in
    let final_node = create_empty_node () in
    ignore (connect end2 final_node);
    
    if greedy then begin
      ignore (connect start_node begin_loop);
      ignore (connect start_node final_node);
    end
    else begin
      ignore (connect start_node final_node);
      ignore (connect start_node begin_loop);
    end;

    final_node

and compile_reverse_graph (r:regex): graph_node * int =
  (* start node is the original accepting point that has pc=0 because it's the first that is being created *)
  let start_node = create_empty_node () in 
  let accept_node = create_node Accept in
  let end_node = compile_to_graph r accept_node Progress reverse_connect in
  ignore (reverse_connect end_node start_node);
  (start_node , end_node.id +1)
and compile (r:regex) (fresh:label) (ctype:comp_type): instruction treelist * label  =
  match r with
  | Re_empty -> (Leaf [], fresh)
  | Re_character c ->
     if (ctype = ReconstructNulled) then (Leaf [Fail], fresh+1)
     else
       begin match c with
       | Char ch -> (Leaf [Consume (Single ch)], fresh+1)
       | Dot -> (Leaf [Consume All], fresh+1)
       | Group g -> (Leaf [Consume (Ranges (group_to_range g))], fresh+1)
       | Class cl -> (Leaf [Consume (Ranges (class_to_range cl))], fresh+1)
       | NegClass cl -> (Leaf [Consume (Ranges (range_neg (class_to_range cl)))], fresh+1)
       end
  | Re_con (r1, r2) ->
     let (l1, f1) = compile r1 fresh ctype in
     let (l2, f2) = compile r2 f1 ctype in
     (l1 @@ l2, f2)
  | Re_alt (r1, r2) ->
     let (l1, f1) = compile r1 (fresh+1) ctype in
     let (l2, f2) = compile r2 (f1+1) ctype in
     (Leaf [Fork (fresh+1, f1+1)] @@ l1 @@ Leaf [Jmp f2] @@ l2, f2)

  | Re_quant (nul, qid, quant, r1) when ctype = Progress ->
     (* progress compilation, consuming is allowed *)

     (* there is some code duplication here, but it should help identify the different particular cases that we do linearly *)

     (** particular case of the Non-Nullable +, where the last repetition can be used for the final loop *)
     if (quant.min > 0 && quant.max = None && nul = NonNullable) then
       begin
         (* repeat the body min-1 times *)
         let (min_code, min_fresh) = repeat_min (quant.min-1) qid r1 fresh ctype in
         (* loop on the last iteration *)
         let (body_code, body_fresh) = compile r1 (min_fresh+1) ctype in
         let fork = if quant.greedy then Fork (min_fresh, body_fresh+1)
                    else Fork (body_fresh+1, min_fresh) in
         (min_code @@ Leaf [SetQuantToClock (qid,false)] @@ body_code @@ Leaf [fork], body_fresh+1)
       end

     (** particular case of the greedy CIN + *)
     else if (quant.min > 0 && quant.max = None && nul = CINullable && quant.greedy) then
       (* the body of the plus will have to be compiled separately *)
       begin
         (* repeat the body min-1 times *)
         let (min_code, min_fresh) = repeat_min (quant.min-1) qid r1 fresh ctype in
         (* fork between the non-nullable repetition or the nulled branch *)
         let (body_code, body_fresh) = compile r1 (min_fresh+3) ctype in
         (min_code @@ Leaf [Fork (min_fresh+1, body_fresh+2); SetQuantToClock (qid, false); BeginLoop] @@ body_code @@ Leaf [EndLoop; Fork (min_fresh+1, body_fresh+3); SetQuantToClock (qid, true)],body_fresh+3)
       end

     (** particular case of the greedy CDN + *)
     else if (quant.min > 0 && quant.max = None && nul = CDNullable && quant.greedy) then
       begin
         (* repeat the body min-1 times *)
         let (min_code, min_fresh) = repeat_min (quant.min-1) qid r1 fresh ctype in
         (* fork between the non-nullable repetition or the nulled branch with a CDN test *)
         let (body_code, body_fresh) = compile r1 (min_fresh+3) ctype in
         (min_code @@ Leaf [Fork (min_fresh+1, body_fresh+2); SetQuantToClock (qid, false); BeginLoop] @@ body_code @@ Leaf [EndLoop; Fork (min_fresh+1, body_fresh+4); CheckNullable qid; SetQuantToClock (qid, true)],body_fresh+4)
       end


     (** Generic Case  *)
     else
       begin
         (* first repeat the body min times *)
         let (min_code, min_fresh) = repeat_min quant.min qid r1 fresh ctype in
         begin match quant.max with
         | None ->
            (* create a loop for the following repetitions *)
            (* iterations of this loop cannot match the empty string, hence the BeginLoop/EndLoop instructions *)
            (* TODO possible optimization: in the non-nullable case, I could remove BeginLoop/EndLoop *)
            let (iter_code, iter_fresh) = compile r1 (min_fresh+3) ctype in
            let fork = if quant.greedy then Fork(min_fresh+1, iter_fresh+2)
                       else Fork (iter_fresh+2,min_fresh+1) in
            (min_code @@ Leaf [fork; SetQuantToClock (qid, false); BeginLoop] @@ iter_code @@ Leaf [EndLoop; Jmp min_fresh],iter_fresh+2)
         | Some max ->
            (* repeat the optional repetitions max-min times *)
            let (opt_code, opt_fresh) = repeat_optional (max-quant.min) qid r1 min_fresh ctype quant.greedy in
            (min_code @@ opt_code,opt_fresh)
         end
       end

  (* when ctype = ReconstrutNulled, ie we only want to find the top-priority nullable path *)
  | Re_quant (nul, qid, quant, r1) ->
     if (quant.min = 0) then (Leaf [], fresh)
     (* optional repetitions can't consume the empty string, so skip it *)
     else if (nul = NonNullable) then
       (* you won't be able to null that expression *)
       (Leaf [Fail], fresh+1)
     else if (quant.max = None && nul = CINullable && quant.greedy) then
       (* CIN Plus should not be compiled recursively *)
       (* but we indicate that the inner plus was nulled as well *)
       (Leaf [SetQuantToClock(qid,true)], fresh+1)
     else if (quant.max = None && nul = CDNullable && quant.greedy) then
       (* same for CDN plus *)
       (Leaf [CheckNullable qid; SetQuantToClock(qid,true)], fresh+2)
     else
       (* in all other cases *)
       (* we only have to compile one iteration, since only the last iteration matters and iterations don't consume *)
       let (l1, f1) = compile r1 (fresh+1) ReconstructNulled in
       (Leaf [SetQuantToClock (qid,false)] @@ l1, f1)

  | Re_capture (cid, r1) ->
     let (l1, f1) = compile r1 (fresh+1) ctype in
     (Leaf [SetRegisterToCP (start_reg cid)] @@ l1 @@ Leaf [SetRegisterToCP (end_reg cid)], f1+1)
  | Re_lookaround (lookid, looktype, r1) ->
     (* does not compile the lookarounds it depends on, only the current expression *)
     begin match looktype with
     | Lookahead | Lookbehind -> (Leaf [CheckOracle lookid], fresh+1)
     | NegLookahead | NegLookbehind -> (Leaf [NegCheckOracle lookid], fresh+1)
     end
  | Re_anchor a -> (Leaf [AnchorAssertion a], fresh+1)

(* repeats a [r] regex [min] times when it's inside a quantifier [qid] with minimum repetitions *)
and repeat_min (min:int) (qid:quantid) (r:regex) (fresh:label) (ctype:comp_type): instruction treelist * label =
  (* TODO possible optimization: there's no need to capture/set anything on the first min-1 repetitions *)
  if min = 0 then (Leaf [], fresh)
  else
    let (body_code, new_fresh) = compile r (fresh+1) ctype in
    let (next_code, next_fresh) = repeat_min (min-1) qid r new_fresh ctype in
    (Leaf [SetQuantToClock (qid,false)] @@ body_code @@ next_code, next_fresh)

(* repeats the optional max-min repetitions of a regex inside a bounded quantifier *)
(* those repetitions are not allowed to match the empty string, hence the BeginLoop/EndLoop instructions *)
and repeat_optional (nb:int) (qid:quantid) (r:regex) (fresh:label) (ctype:comp_type) (greedy:bool) : instruction treelist * label =
  if nb = 0 then (Leaf [], fresh)
  else
    let (body_code, new_fresh) = compile r (fresh+3) ctype in
    let (next_code, next_fresh) = repeat_optional (nb-1) qid r (new_fresh+1) ctype greedy in
    let fork = if greedy then Fork(fresh+1,next_fresh)
               else Fork(next_fresh,fresh+1) in
    (Leaf [fork; SetQuantToClock (qid,false); BeginLoop] @@ body_code @@ Leaf [EndLoop] @@ next_code,next_fresh)



(* adds an accept at the end of the bytecode *)
let compile_to_bytecode (r:regex): code =
  let (c,_) = compile r 0 Progress in
  let full_c = tl_flatten c [Accept] in
  Array.of_list full_c

(* same but with a WriteOracle instruction instead of an Accept *)
(* l is the current lookid we are compiling the regex for *)
let compile_to_write (r:regex) (l:lookid): code =
  let (c,_) = compile r 0 Progress in
  let full_c = tl_flatten c [WriteOracle l] in
  Array.of_list full_c

(* compiles the bytecode for reconstructing the missing groups from nulled + *)
(* this recursively compiles the nested + *)
let compile_reconstruct_nulled (r:regex): code =
  let (c,_) = compile r 0 ReconstructNulled in
  let full_c = tl_flatten c [Accept] in
  Array.of_list full_c


(** * Fully Compiled Regexes  *)
(* to do ahead-of-time compilation, we define here the type of a compiled regex *)
(* where everything has already been compiled *)
(* we could decide dynamically which sub-expression to compile (for lookarounds and nulled plus) *)
(* but if we want to benchmark against other engines that do all of the compilation ahead of time *)
(* we should do the same *)
type compiled_regex =
  {
    (* data for the main expression *)
    main_ast: regex;
    main_bc: code;
    reverse_graph: graph_node;
    rv_graph_size: int;
    main_cdns: cdns;
    (* lookaround data *)
    look_types: lookaround Array.t; (* the type of each lookaround *)
    look_ast: regex Array.t; (* the ast of each lookaround *)
    look_cdns: cdns Array.t; (* the cdns restricted to each lookaround *)
    look_build_bc: code Array.t;    (* lookaround bytecodes for building the oracle *)
    look_capture_bc: code Array.t; (* lookarounds bytecodes for constructing capture groups *)
    (* Plus data *)
    plus_bc: code Array.t;      (* CDN & CIN plus bytecode *)
  }

(* the regex used when builing the oracle *)
let oracle_regex (looktype:lookaround) (l:regex): regex =
  match looktype with
  | Lookahead | NegLookahead -> lazy_prefix (reverse_regex (remove_capture l))
  | Lookbehind | NegLookbehind -> lazy_prefix (remove_capture l)

(* the regex used when reconstructing capture groups *)
let capture_regex (looktype:lookaround) (l:regex): regex =
  match looktype with
  | Lookahead -> l
  | Lookbehind -> reverse_regex l
  | _ -> Re_empty               (* no capture groups defined in negative lookarounds *)

(* recursively sets the two kinds of bytecode for each lookaround and nullable plus *)
let rec compile_extra_bytecode (r:regex) (c:compiled_regex): unit =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> ()
  | Re_capture(_,r1) -> compile_extra_bytecode r1 c
  | Re_alt(r1,r2) | Re_con(r1,r2) -> compile_extra_bytecode r1 c; compile_extra_bytecode r2 c
  | Re_quant (nul, qid, quant, r1) ->
     (* only for CIN and CDN *)
     if (quant.min > 0 && quant.max = None && nul <> NonNullable && quant.greedy) then
       begin
         let quant_code = compile_reconstruct_nulled r1 in
         c.plus_bc.(qid) <- quant_code
       end;
     compile_extra_bytecode r1 c
  | Re_lookaround (lid, la, body) ->
     (* both directions for building the oracle and reconstruct capture groups *)
     let build_reg = oracle_regex la body in
     let capture_reg = capture_regex la body in
     let build_code = compile_to_write build_reg lid in
     let capture_code = compile_to_bytecode capture_reg in
     c.look_types.(lid) <- la;
     c.look_cdns.(lid) <- compile_cdns body;
     c.look_ast.(lid) <- body;
     c.look_build_bc.(lid) <- build_code;
     c.look_capture_bc.(lid) <- capture_code;
     compile_extra_bytecode body c

let full_compilation (r:regex) : compiled_regex =
  let maxlook = max_lookaround r in
  let maxquant = max_quant r in
  let empty_code : code = Array.of_list [] in
  let looktypes = Array.make (maxlook+1) Lookahead in
  let lookcdns = Array.make (maxlook+1) [] in
  let lookast = Array.make (maxlook+1) Re_empty in
  let build_look = Array.make (maxlook+1) empty_code in
  let capture_look = Array.make (maxlook+1) empty_code in
  let plus_code = Array.make (maxquant+1) empty_code in
  let main_code = compile_to_bytecode (lazy_prefix r) in
  
  let (reverse_graph, rv_graph_size) = compile_reverse_graph r in
  let main_cdns = compile_cdns r in
  let compiled = {
      main_ast = r; main_bc = main_code;
      main_cdns = main_cdns;
      look_types = looktypes; look_cdns = lookcdns; look_ast = lookast;
      look_build_bc = build_look; look_capture_bc = capture_look;
      plus_bc = plus_code; reverse_graph = reverse_graph; rv_graph_size = rv_graph_size} in
  compile_extra_bytecode r compiled; (* compile lookarounds, CIN & CDN *)
  compiled
