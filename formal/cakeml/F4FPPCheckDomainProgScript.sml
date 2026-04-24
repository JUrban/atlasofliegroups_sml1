(*
  Minimal CakeML-translation checkpoint for VERIFY_ESTIMATE Stage A.

  We start with a *pure* list-model checker that corresponds to the core
  computation in `atlas-scripts-sml/SimplerVerifyF4FPP.sml`'s `check_domain`:

    - given a predicate `missing : 'a -> bool` and a finite domain list `ts`,
      compute how many elements of `ts` are missing (i.e. violate completeness).

  This theory translates the checker into CakeML using the (non-monadic)
  translator and exposes the standard "value behaves like HOL function" theorem.

  Next steps (planned):
    - extend with a more structured checker result (e.g. witness extraction),
    - then switch to the monadic translator for a stateful hash-set model.
*)

Theory F4FPPCheckDomainProg
Ancestors
  arithmetic list combin pair semanticPrimitives ml_translator
Libs
  ml_translatorLib

Definition check_domain_fun_def:
  check_domain_fun missing (ts:'a list) = LENGTH (FILTER missing ts)
End

Theorem check_domain_fun_eq0_iff:
  ∀missing ts. check_domain_fun missing ts = 0 ⇔ ∀t. MEM t ts ⇒ ¬missing t
Proof
  rw[check_domain_fun_def,LENGTH_EQ_0,FILTER_EQ_NIL,EVERY_MEM]
QED

(* Ensure required list primitives are available to the translator. *)
val FILTER_v_thm = translate FILTER;
val LENGTH_v_thm = translate LENGTH;

val check_domain_fun_v_thm = translate check_domain_fun_def;

val Decls_thm =
  get_ml_prog_state ()
  |> ml_progLib.clean_state
  |> ml_progLib.remove_snocs
  |> ml_progLib.get_thm
  |> REWRITE_RULE [ml_progTheory.ML_code_def,ml_progTheory.ML_code_env_def];

Theorem evaluate_prog_thm =
  Decls_thm |> REWRITE_RULE [ml_progTheory.Decls_def]
