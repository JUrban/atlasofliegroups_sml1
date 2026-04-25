(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeGoalsScript.sml

  Purpose
  - Provide a ParamHash-specific bridge goal layer, making explicit the exact
    properties we need from the concrete SML module `atlas-scripts-sml/ParamHash.sml`
    in order to discharge the abstract `fast_param_set_*` obligations.

  Background
  - The fast program stores its computed set in a mutable `ParamHash.t`.
  - The bottom-layer checker consumes this structure through:
      - enumeration (`ParamHash.list`) and
      - membership (`ParamHash.contains`)
    (or equivalently `lookup >= 0`).

  In the HOL4 development we have already introduced:
  - `fast_param_set g : param_set` (abstract),
  - refinements splitting its correctness into:
      - list sound/complete
      - contains sound/complete
    and composition lemmas yielding `fast_param_set_ok g`.

  This theory introduces explicit abstract placeholders for the concrete
  ParamHash observations:
  - `paramhash_list g`
  - `paramhash_contains g`
  along with a “wiring” predicate asserting that `fast_param_set g` is exactly
  the `(list,contains)` pair extracted from ParamHash.

  Status
  - The ParamHash correctness obligations are stated here (definitions + OK
    implications only).
  - Any “execution success ⇒ obligations” bridge theorems are isolated in
    dedicated `*Cheats*` theories, to keep this theory entirely OK.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;
open F4FPPVerifyFastParamSetRefineGoalsTheory;
open F4FPPVerifyFastParamSetContainsRefineGoalsTheory;
open F4FPPVerifyFastParamSetListRefineGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeGoals";

(* Abstract observations of the concrete ParamHash produced by the fast compute
   phase. *)
val _ = new_constant ("paramhash_list", ``:group -> param list``);
val _ = new_constant ("paramhash_contains", ``:group -> param -> bool``);

(* Wiring predicate: `fast_param_set g` is exactly the ParamHash view. *)
Definition fast_param_set_is_paramhash_def:
  fast_param_set_is_paramhash g <=>
    ps_list (fast_param_set g) = paramhash_list g /\
    ps_contains (fast_param_set g) = paramhash_contains g
End

(* ParamHash list/contains obligations, stated directly in terms of `U_fast g`. *)
Definition paramhash_list_sound_def:
  paramhash_list_sound g <=>
    !p. MEM p (paramhash_list g) ==> p IN U_fast g
End

Definition paramhash_list_complete_def:
  paramhash_list_complete g <=>
    !p. p IN U_fast g ==> MEM p (paramhash_list g)
End

Definition paramhash_contains_sound_def:
  paramhash_contains_sound g <=>
    !p. paramhash_contains g p ==> p IN U_fast g
End

Definition paramhash_contains_complete_def:
  paramhash_contains_complete g <=>
    !p. p IN U_fast g ==> paramhash_contains g p
End

(* OK: ParamHash obligations + wiring imply the corresponding `fast_param_set_*`
   obligations introduced earlier. *)
Theorem fast_param_set_is_paramhash_and_paramhash_list_sound_imp_list_sound:
  !g.
    fast_param_set_is_paramhash g /\ paramhash_list_sound g ==>
      fast_param_set_list_sound g
Proof
  rw[fast_param_set_is_paramhash_def, paramhash_list_sound_def,
     fast_param_set_list_sound_def]
QED

Theorem fast_param_set_is_paramhash_and_paramhash_list_complete_imp_list_complete:
  !g.
    fast_param_set_is_paramhash g /\ paramhash_list_complete g ==>
      fast_param_set_list_complete g
Proof
  rw[fast_param_set_is_paramhash_def, paramhash_list_complete_def,
     fast_param_set_list_complete_def]
QED

Theorem fast_param_set_is_paramhash_and_paramhash_contains_sound_imp_contains_sound:
  !g.
    fast_param_set_is_paramhash g /\ paramhash_contains_sound g ==>
      fast_param_set_contains_sound g
Proof
  rw[fast_param_set_is_paramhash_def, paramhash_contains_sound_def,
     fast_param_set_contains_sound_def]
QED

Theorem fast_param_set_is_paramhash_and_paramhash_contains_complete_imp_contains_complete:
  !g.
    fast_param_set_is_paramhash g /\ paramhash_contains_complete g ==>
      fast_param_set_contains_complete g
Proof
  rw[fast_param_set_is_paramhash_def, paramhash_contains_complete_def,
     fast_param_set_contains_complete_def]
QED

(* A convenient single bundle: what we ultimately need from ParamHash. *)
Definition paramhash_ok_def:
  paramhash_ok g <=>
    fast_param_set_is_paramhash g /\
    paramhash_list_sound g /\
    paramhash_list_complete g /\
    paramhash_contains_sound g /\
    paramhash_contains_complete g
End

Theorem paramhash_ok_imp_fast_param_set_ok:
  !g. paramhash_ok g ==> fast_param_set_ok g
Proof
  rpt strip_tac
  \\ fs[paramhash_ok_def]
  \\ match_mp_tac fast_param_set_list_and_contains_obligations_imp_fast_param_set_ok
  \\ metis_tac
       [ fast_param_set_is_paramhash_and_paramhash_list_sound_imp_list_sound
       , fast_param_set_is_paramhash_and_paramhash_list_complete_imp_list_complete
       , fast_param_set_is_paramhash_and_paramhash_contains_sound_imp_contains_sound
       , fast_param_set_is_paramhash_and_paramhash_contains_complete_imp_contains_complete
       ]
QED

(* --- Bridge from compute-phase success (currently CHEATED) --- *)
(* Bridge theorems from execution to these obligations are recorded in
   `F4FPPVerifyParamHashBridgeCheatsGoalsTheory`. *)

val _ = export_theory ();
