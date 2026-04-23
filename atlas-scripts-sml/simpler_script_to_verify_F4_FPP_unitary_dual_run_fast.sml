(* Fast runner for `simpler_script_to_verify_F4_FPP_unitary_dual`.

   This performs only a bounded sample of (x,lambda,gamma) triples. *)

use "atlas-scripts-sml/SimplerVerifyF4FPP.sml";

(* Conservative defaults; adjust as needed. *)
val () = SimplerVerifyF4FPP.runFast (5, 3, 30);

