# HOL4 formalization (VERIFY_ESTIMATE Stage A)

This directory contains the first HOL4 artifacts for the plan in `VERIFY_ESTIMATE.md`.

For a narrative description of the overall approach and refinement structure, see:

- `formal/STRATEGY.md`
- `formal/hol4/GOALS.md`
- `formal/hol4/CHEATS.md`

**Build**
```sh
export HOLDIR=/project/hol/HOL
export PATH=/project/hol/polyml/bin:$HOLDIR/bin:$PATH
export LD_LIBRARY_PATH=/project/hol/polyml/lib:${LD_LIBRARY_PATH:-}

cd formal/hol4
$HOLDIR/bin/Holmake
```

**Theories**
- `F4FPPVerifySpec` (`F4FPPVerifySpecScript.sml`): abstract set semantics (`U_slow`, `complete_rel`, `missing_witness`, and an abstract `D_slow`).
- `F4FPPVerifyAlg` (`F4FPPVerifyAlgScript.sml`): pure list model of the slow checker (`check_domain_fun`) and lemmas connecting `check_domain_fun = 0` to `complete_rel`.
- `F4FPPBottomLayerGoals` (`F4FPPBottomLayerGoalsScript.sml`): set-level specification of the bottom-layer checks (standard/final, lambda-table, hermitian/unitary, twist, dual closure).
- `F4FPPVerifyRefinedMainGoals` (`F4FPPVerifyRefinedMainGoalsScript.sml`): refinement-bundle predicates and the core “obligations ⇒ set equality” theorem.
- `F4FPPVerifyAtlasFFIContractsGoals` (`F4FPPVerifyAtlasFFIContractsGoalsScript.sml`): explicit inventory of Atlas/FFI contracts (hash/equality, congruence laws).
- `F4FPPVerifyEndToEndObligationStackGoals` (`F4FPPVerifyEndToEndObligationStackGoalsScript.sml`): explicit end-to-end obligation stack theorem.
- `F4FPPVerifyEndToEndProgramSuccessStackGoals` (`F4FPPVerifyEndToEndProgramSuccessStackGoalsScript.sml`): explicit “program success ⇒ obligation stack ⇒ equivalence” roadmap theorem.
