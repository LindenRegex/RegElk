(** * Measuring With RDTSC *)
let now () = Int64.of_float (Unix.gettimeofday () *. 1_000_000.0)
let elapsed from = Int64.sub (now ()) from

(* all our algorithms are measured with the rdtsc instruction *)
(* to get an estimate as precise as possible *)
(* when we measure V8Linear, we also patch it to return rdtsc *)
(* we use the rdtsc interface provided by the Ocaml_Intrinsics package *)
let now () = Ocaml_intrinsics.Perfmon.rdtsc ()
let elapsed from = Int64.sub (now ()) from
