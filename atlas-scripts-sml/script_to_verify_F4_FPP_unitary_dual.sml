use "atlas-scripts-sml/VerifyF4FPP.sml";

(*
  File: atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml

  Purpose
  - SML replacement for `atlas-scripts/script_to_verify_F4_FPP_unitary_dual.at`.
  - Entry point intended to be run under Poly/ML:
      poly -q < atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml

  Behavior
  - Loads `VerifyF4FPP` and runs `VerifyF4FPP.run()` immediately.
*)
val () = VerifyF4FPP.run ();

(* Dummy `main` for `polyc`-compiled entry points. *)
fun main () = ();
