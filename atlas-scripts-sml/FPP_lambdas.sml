use "atlas-scripts-sml/FPP_lambdas_fold.sml";

(*
  File: atlas-scripts-sml/FPP_lambdas.sml

  Purpose
  - Compatibility wrapper matching the `.at` filename `FPP_lambdas.at`.
  - The current SML implementation is in `atlas-scripts-sml/FPP_lambdas_fold.sml`,
    which is tailored to the folded-FPP workflow used in the F4 verifier code.
*)

structure FPP_lambdas = FPP_lambdas_fold;

