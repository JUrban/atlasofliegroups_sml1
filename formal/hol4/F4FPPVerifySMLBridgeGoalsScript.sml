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

(* Refinement obligations: success implies the abstract goal predicates. *)
Theorem fast_program_succeeds_imp_fast_ok:
  !g dirac. fast_program_succeeds g dirac ==> fast_ok g dirac
Proof
  (* Top-down obligation: proved later by linking `VerifyF4FPP.sml` to:
       - `fast_sound g` (semantic soundness of `U_fast`)
       - `bottom_layer_total_ok g dirac (U_fast g)` (checked invariants)
     potentially under explicit FFI specs. *)
  cheat
QED

Theorem slow_program_succeeds_imp_dom_and_slow_ok:
  !g. slow_program_succeeds g ==> dom_list_correct g /\ slow_ok g
Proof
  (* Top-down obligation: proved later by linking `SimplerVerifyF4FPP.sml` to:
       - correctness of its enumeration vs `D_slow`
       - correctness of its missing-witness check vs `check_domain_fun` *)
  cheat
QED

Theorem slow_program_succeeds_imp_dom_list_correct:
  !g. slow_program_succeeds g ==> dom_list_correct g
Proof
  (* Convenience projection for later goal statements. *)
  cheat
QED

Theorem slow_program_succeeds_imp_slow_ok:
  !g. slow_program_succeeds g ==> slow_ok g
Proof
  (* Convenience projection for later goal statements. *)
  cheat
QED

Theorem fast_and_slow_programs_succeed_imp_full_ok:
  !g dirac.
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==> full_ok g dirac
Proof
  rpt strip_tac
  \\ pop_assum strip_assume_tac
  \\ simp[full_ok_def]
  \\ rpt conj_tac
  \\ metis_tac
      [ slow_program_succeeds_imp_dom_list_correct
      , slow_program_succeeds_imp_slow_ok
      , fast_program_succeeds_imp_fast_ok
      ]
  \\ metis_tac
      [ slow_program_succeeds_imp_dom_list_correct
      , slow_program_succeeds_imp_slow_ok
      , fast_program_succeeds_imp_fast_ok
      ]
  \\ metis_tac
      [ slow_program_succeeds_imp_dom_list_correct
      , slow_program_succeeds_imp_slow_ok
      , fast_program_succeeds_imp_fast_ok
      ]
QED

(* Once the bridge obligations hold, we can derive the main consequences
   without any further cheating: `full_ok` gives equality and invariants. *)
Theorem fast_and_slow_programs_succeed_gives_equivalence:
  !g dirac.
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==>
      U_slow g (D_slow g) = U_fast g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  (*
    Intended proof sketch (once the bridge lemmas are proved without `cheat`):
    - derive `full_ok g dirac` from slow/fast program success
    - apply `full_ok_implies_set_equality` and `full_ok_implies_bottom_layer_total_ok`
  *)
  cheat
QED

val _ = export_theory ();
