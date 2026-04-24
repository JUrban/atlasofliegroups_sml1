(* Smoke test: ensure the slow brute-force verifier module loads and typechecks.

   This intentionally does NOT run `SimplerVerifyF4FPP.runSlow` or `runFast`
   (those build the full F4_s unitary hash and can be expensive). *)

use "atlas-scripts-sml/SimplerVerifyF4FPP.sml";

val () = print "OK: simpler_verify_compile_smoke\n";

