use "atlas-scripts-sml/VerifyF4FPP.sml";

(*
  File: atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual_main.sml

  Purpose
  - Alternate entry point suitable for `polyc` compilation, providing an
    explicit `main : unit -> unit`.
*)
fun main () : unit =
  VerifyF4FPP.run ();
