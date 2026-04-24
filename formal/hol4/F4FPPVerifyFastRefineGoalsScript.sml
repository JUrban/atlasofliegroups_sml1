(*
  File: formal/hol4/F4FPPVerifyFastRefineGoalsScript.sml

  Purpose
  - Refine the single high-level obligation `fast_sound g` (from
    `F4FPPVerifyGoalsTheory`) into smaller, compositional obligations that more
    closely match the structure of the fast SML program:

      1) The fast program enumerates some (possibly pruned) domain `D_fast g`
         of triples.
      2) Every element of `U_fast g` is witnessed by a triple in `D_fast g`.
      3) The fast enumeration domain is a subset of the intended slow domain:
           `D_fast g ⊆ D_slow g`.

    From (2) and (3) we can *derive* `fast_sound g` in the sense used by the
    main set-equality argument.

  Status
  - This theory is an “OK” refinement layer: it contains only definitions and
    simple logical lemmas (no `cheat`), and introduces the explicit obligations
    that will later be discharged by connecting to `VerifyF4FPP.sml` and Atlas
    FFI specifications.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastTheory;

val _ = new_theory "F4FPPVerifyFastRefineGoals";

(* Abstract set of triples actually enumerated (or “considered”) by the fast
   program. This may be a strict subset of `D_slow g` due to pruning. *)
val _ = new_constant ("D_fast", ``:group -> triple set``);

Definition fast_witnessed_def:
  fast_witnessed g <=>
    !pi.
      pi IN U_fast g ==>
        ?t.
          t IN D_fast g /\
          first_final_term (mk_param g t) = SOME pi /\
          is_unitary pi
End

Definition fast_domain_subset_def:
  fast_domain_subset g <=>
    D_fast g SUBSET D_slow g
End

Theorem fast_witnessed_and_subset_imp_fast_sound:
  !g.
    fast_witnessed g /\ fast_domain_subset g ==>
      fast_sound g
Proof
  rw[fast_witnessed_def, fast_domain_subset_def, fast_sound_def, sound_wrt_domain_def]
  \\ metis_tac[SUBSET_DEF]
QED

val _ = export_theory ();

