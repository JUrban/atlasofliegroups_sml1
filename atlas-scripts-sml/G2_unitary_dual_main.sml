use "atlas-scripts-sml/G2_unitary_dual.sml";

(*
  File: atlas-scripts-sml/G2_unitary_dual_main.sml

  Purpose
  - Simple Poly/ML driver that loads `G2_unitary_dual` and runs its `demo()`.
  - Useful as a smoke test of the induction/parabolic machinery on a small rank.
*)
val () = G2_unitary_dual.demo ();
