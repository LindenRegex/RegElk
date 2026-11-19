(lang dune 3.18)
(using menhir 3.0)
(authors "Aurèle Barrière" "Clément Pit-Claudel")
(homepage "https://github.com/LindenRegex/RegElk")
(generate_opam_files)

(package
 (name regelk)
 (synopsis "OCaml Linear Engine for JavaScript Regexes")
 (depends
  (ocaml (= 4.14.5))
  (yojson (= 2.1.0))
  (ocaml_intrinsics (= "v0.15.2"))
  (core_bench (= "v0.15.0"))
  (core (= "v0.15.1"))
  (core_unix (= "v0.15.2"))
  (menhir (= 20220210))
  (ocamlfind (= 1.9.8))))
