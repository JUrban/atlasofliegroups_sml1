(*
  File: formal/hol4/F4FPPVerifySMLBridgeGoalsScript.sml

  Purpose
  - State the “bridge” theorems that will eventually connect the *concrete*
    Poly/ML programs in `atlas-scripts-sml/` to the abstract goal predicates in
    the HOL4 development.

  What this is (and is not)
  - This is intentionally **top-down** and may use `cheat` for now.
  - It does not attempt to model Poly/ML evaluation, exceptions, or the FFI.
    Instead, it introduces abstract predicates representing “the program
    returned successfully” and records the obligations that such success is
    expected to imply.

  Concrete code this corresponds to
  - Fast program: `atlas-scripts-sml/VerifyF4FPP.sml`
      - constructs the set `U_fast` (via `ParamHash`)
      - runs bottom-layer checks (`FPP_globalDirac.FPP_unitary_hash_bottom_layer_param_hash`)
  - Slow program: `atlas-scripts-sml/SimplerVerifyF4FPP.sml`
      - enumerates the domain and counts missing witnesses (may take hours)

  Endgame
  - Ultimately we want to replace the `cheat`s in this theory by:
      (a) a CakeML proof of the algorithmic/data-structure core, and
      (b) a set of FFI specifications (possibly axiomatized) for Atlas calls.

  Status
  - This theory only introduces the abstract “program success” predicates.
  - The (currently `cheat`ed) bridge theorems are isolated in
    `F4FPPVerifySMLBridgeCheatsGoalsTheory`.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFullGoalsTheory;
open F4FPPBottomLayerGoalsTheory;

val _ = new_theory "F4FPPVerifySMLBridgeGoals";

(* Abstract predicates meaning: the SML programs ran and reported success.
   These will later be refined to concrete semantics about I/O, exceptions,
   return values, and flag settings. *)
val _ = new_constant ("fast_program_succeeds", ``:group -> bool -> bool``);
val _ = new_constant ("slow_program_succeeds", ``:group -> bool``);

val _ = export_theory ();
