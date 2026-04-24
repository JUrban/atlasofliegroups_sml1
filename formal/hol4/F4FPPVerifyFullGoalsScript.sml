(*
  File: formal/hol4/F4FPPVerifyFullGoalsScript.sml

  Purpose
  - Combine the previously-separated goal layers into a single “what the fast
    and slow programs together establish” statement.
  - This theory does *not* attempt to connect to concrete SML/FFI execution;
    it just defines the top-level predicates and derives convenient theorems.

  Intended reading
  - `slow_ok g` is the slow script reporting “0 missing witnesses”.
  - `fast_sound g` is the key semantic obligation for the fast set `U_fast g`.
  - `bottom_layer_total_ok g dirac (U_fast g)` captures the additional
    invariants checked in `FPP_globalDirac.sml` (or rho-seeding for compact
    groups).
  - `fast_ok g dirac` packages the fast program’s obligations.
  - `full_ok g dirac` packages both programs’ obligations plus domain
    correctness, giving a clean top-level theorem.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPBottomLayerGoalsTheory;

val _ = new_theory "F4FPPVerifyFullGoals";

Definition fast_checks_ok_def:
  fast_checks_ok g (dirac:bool) <=>
    bottom_layer_total_ok g dirac (U_fast g)
End

Definition fast_ok_def:
  fast_ok g (dirac:bool) <=>
    fast_sound g /\ fast_checks_ok g dirac
End

Definition full_ok_def:
  full_ok g (dirac:bool) <=>
    dom_list_correct g /\ slow_ok g /\ fast_ok g dirac
End

Theorem full_ok_implies_set_equality:
  !g dirac.
    full_ok g dirac ==>
      U_slow g (D_slow g) = U_fast g
Proof
  rw[full_ok_def, fast_ok_def, fast_checks_ok_def]
  \\ metis_tac[slow_ok_and_fast_sound_gives_set_equality]
QED

Theorem full_ok_implies_bottom_layer_total_ok:
  !g dirac.
    full_ok g dirac ==>
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  simp[full_ok_def, fast_ok_def, fast_checks_ok_def]
QED

val _ = export_theory ();

