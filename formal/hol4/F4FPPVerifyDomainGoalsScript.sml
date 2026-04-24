(*
  File: formal/hol4/F4FPPVerifyDomainGoalsScript.sml

  Purpose
  - Refine the coarse obligation `dom_list_correct` from `F4FPPVerifyGoals` into
    smaller, compositional obligations about the *component enumerations*:
      - KGB element enumeration
      - per-x lambda enumeration
      - barycenter enumeration
  - Provide a canonical “product construction” `dom_list_from_components` and
    prove that if each component enumeration is correct (as a set), then the
    product enumeration is correct for `D_slow`.

  This fits the “top-down refinement” style: we can later link these abstract
  list constants to the concrete SML enumerators (and/or to Atlas C++ oracles).
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;

val _ = new_theory "F4FPPVerifyDomainGoals";

Theorem IN_set[simp]:
  !x xs. x IN set xs <=> MEM x xs
Proof
  Induct_on `xs` \\ simp[]
QED

Theorem MEM_FLAT_MAP[simp]:
  !f xs x.
    MEM x (FLAT (MAP f xs)) <=> ?y. MEM y xs /\ MEM x (f y)
Proof
  simp[MEM_FLAT, MEM_MAP] \\ metis_tac[]
QED

(* Abstract list-valued enumerators to be refined later. *)
val _ = new_constant ("KGB_list", ``:group -> num list``);
val _ = new_constant ("FPP_lambdas_list", ``:group -> num -> ratvec list``);
val _ = new_constant ("AllBarycenters_list", ``:group -> ratvec list``);

Definition dom_list_from_components_def:
  dom_list_from_components (g:group) : triple list =
    FLAT
      (MAP
         (λx.
            FLAT
              (MAP
                 (λlam.
                    MAP (λgam. <| x := x; lambda := lam; gamma := gam |>)
                        (AllBarycenters_list g))
                 (FPP_lambdas_list g x)))
         (KGB_list g))
End

Definition KGB_list_correct_def:
  KGB_list_correct g <=> set (KGB_list g) = KGB g
End

Definition FPP_lambdas_list_correct_def:
  FPP_lambdas_list_correct g <=>
    !x. set (FPP_lambdas_list g x) = FPP_lambdas g x
End

Definition AllBarycenters_list_correct_def:
  AllBarycenters_list_correct g <=> set (AllBarycenters_list g) = AllBarycenters g
End

Theorem dom_list_from_components_correct:
  !g.
    KGB_list_correct g /\
    FPP_lambdas_list_correct g /\
    AllBarycenters_list_correct g ==>
      set (dom_list_from_components g) = D_slow g
Proof
  rw[KGB_list_correct_def, FPP_lambdas_list_correct_def, AllBarycenters_list_correct_def]
  \\ rw[EXTENSION]
  \\ Cases_on `x`
  \\ simp[D_slow_def, dom_list_from_components_def, MEM_MAP]
  \\ simp[GSYM IN_set]
  \\ fs[triple_component_equality]
QED

val _ = export_theory ();
