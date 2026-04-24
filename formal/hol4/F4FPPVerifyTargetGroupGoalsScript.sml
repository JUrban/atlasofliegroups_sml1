(*
  File: formal/hol4/F4FPPVerifyTargetGroupGoalsScript.sml

  Purpose
  - Pin down the *concrete instance* we ultimately care about: the split real
    form `F4_s` used throughout the SML scripts.

  Why we need this layer
  - Most theories in `formal/hol4/` are generic in a group `g : group`.
  - However, several parts of the SML code branch on group properties that are
    *specific* to `F4_s`, e.g.:
      - the special lambda-table consistency check is triggered only for the
        `F4_s` KGB-size (229),
      - the overall workflow assumes the group is non-compact (so the bottom
        layer runs checks rather than rho-seeding).
  - For the eventual end-to-end theorem statement, we will instantiate `g` to
    this concrete group and discharge these group facts once.

  Status
  - This theory is mostly declarative: it introduces a constant `F4s` and
    records the key group facts we will assume/prove from the Atlas FFI.
  - For now these facts are packaged as axioms (`new_axiom`) to keep the
    top-level theorem statements usable immediately; later they can be turned
    into proved lemmas once we refine the FFI layer.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifySpecTheory;
open F4FPPBottomLayerGoalsTheory;

val _ = new_theory "F4FPPVerifyTargetGroupGoals";

(* The specific group instance used by `VerifyF4FPP.sml` and friends. *)
val _ = new_constant ("F4s", ``:group``);

(* The fast scripts expect `F4_s` to be non-compact, so bottom-layer checks run. *)
val _ = new_axiom ("F4s_not_compact", ``~group_is_compact F4s``);

(* The slow/fast scripts' “F4 lambda-table check” is enabled for this group. *)
val _ = new_axiom ("F4s_needs_lambda_table_check", ``needs_lambda_table_check F4s``);

val _ = export_theory ();

