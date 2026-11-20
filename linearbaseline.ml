let linear_scan r s =
  let arr = Array.make_matrix (String.length r) (String.length s) false in
  for ridx = 0 to String.length r - 1 do
    for sidx = 0 to String.length s - 1 do
      arr.(ridx).(sidx) <- (String.get s sidx = String.get r ridx)
    done
  done;
  let sum = ref 0 in
  for ridx = 0 to String.length r - 1 do
    for sidx = 0 to String.length s - 1 do
      sum := !sum + (if arr.(ridx).(sidx) then 1 else 0)
    done
  done;
 !sum

let _ =
  let regex = Sys.argv.(1) in
  let string = Sys.argv.(2) in
  let warmups = int_of_string(Sys.argv.(3)) in
  let repetitions = int_of_string(Sys.argv.(4)) in

  let sum = ref 0 in
  for [@warning "-35"] i=0 to (warmups-1) do
    sum := !sum + linear_scan regex string
  done;

  (* triggering garbage collector *)
  Gc.full_major();
  Gc.set { (Gc.get ()) with minor_heap_size = 1_000_000_000 };

  (* measuring matches *)
  let tstart = Timer.now() in
  for [@warning "-35"] i = 0 to (repetitions - 1) do
    sum := !sum + linear_scan regex string
  done;
  let time = Timer.elapsed tstart in

  Printf.fprintf stderr "%d" !sum;
  Printf.printf ("%Li\n") time
