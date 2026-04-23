(* Entry point for the (very slow) brute-force verifier.

   This file is intentionally tiny and does NOT `use` the heavy implementation
   module unless explicitly enabled by environment variables.

   - Full (very slow) run:
       ATLAS_RUN_SLOW_SIMPLER_VERIFY=1 poly -q < atlas-scripts-sml/simpler_script_to_verify_F4_FPP_unitary_dual.sml

   - Fast partial run (bounded sampling; should complete quickly):
       ATLAS_RUN_FAST_SIMPLER_VERIFY=1 poly -q < atlas-scripts-sml/simpler_script_to_verify_F4_FPP_unitary_dual.sml
*)

fun getenvIs1 name =
  case OS.Process.getEnv name of
    SOME "1" => true
  | _ => false;

val runSlow = getenvIs1 "ATLAS_RUN_SLOW_SIMPLER_VERIFY";
val runFast = (not runSlow) andalso getenvIs1 "ATLAS_RUN_FAST_SIMPLER_VERIFY";

val () =
  if runSlow then
    use "atlas-scripts-sml/simpler_script_to_verify_F4_FPP_unitary_dual_run_slow.sml"
  else if runFast then
    use "atlas-scripts-sml/simpler_script_to_verify_F4_FPP_unitary_dual_run_fast.sml"
  else
    TextIO.print
      "simpler_script_to_verify_F4_FPP_unitary_dual: disabled (set ATLAS_RUN_SLOW_SIMPLER_VERIFY=1 or ATLAS_RUN_FAST_SIMPLER_VERIFY=1)\n";

(* Dummy `main` for `polyc`-compiled entry points. *)
fun main () = ();
