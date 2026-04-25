## CakeML translator playground for F4/FPP verification

This directory is the start of the CakeML side of the plan in `VERIFY_ESTIMATE.md`:
translate small, *pure* list-model checkers into CakeML and prove their basic
correctness theorems (in HOL), before moving on to stateful/hash-table models.

### Build

This assumes you have already built the HOL4 + CakeML toolchain under
`/project/hol/` as described in `HOL_TOOLCHAIN.md`.

```sh
export HOLDIR=/project/hol/HOL
export PATH=/project/hol/polyml/bin:$HOLDIR/bin:$PATH
export LD_LIBRARY_PATH=/project/hol/polyml/lib:${LD_LIBRARY_PATH:-}

cd /project/formal/cakeml
$HOLDIR/bin/Holmake
```

### What is here so far?

- `F4FPPCheckDomainProgScript.sml`: a minimal pure checker (`check_domain_fun`)
  that counts “missing witnesses” in a domain list; translated with the
  (non-monadic) CakeML translator.
- `ParamHashProgScript.sml`: an initial monadic (stateful) model of a
  ParamHash-like bucketed set, translated with the monadic translator.
- `ParamHashGoalsScript.sml`: “top-down” correctness goals for the pure-state
  model (`ph_lookup_state_complete`, `ph_all_present_state_iff_subset`, ...).
- `ParamHashBuildGoalsScript.sml`: a small non-cheated base theory:
  `ph_build_state`, list/nthn helper lemmas, and the pure well-formedness
  preservation lemma `ph_match_state_preserves_ok`.
- `ParamHashRefinementGoalsScript.sml`: refinement lemmas connecting the monadic
  operations (`ph_lookup`, `ph_match`, `ph_insert_all`, `ph_all_present`) to the
  pure-state model (`ph_*_state`) under `ph_ok`. Most are proved; currently
  **OK** (no `cheat`).
- `ParamHashCreateGoalsScript.sml`: initialization goals for the monadic model:
  defines the pure initial state `ph_create_state` and records the refinement
  lemma `ph_create_refines_create_state` (now proved, no `cheat`).
- `ParamHashEndToEndGoalsScript.sml`: composes `create` + `insert_all` into a
  single end-to-end reference model (`ph_build_from_create_state`) and records
  the corresponding monadic refinement goal (proved by composition, but still
  CHEAT-tainted because it depends on cheat-tainted invariant/bridge layers).
- `ParamHashSetGoalsScript.sml`: a set-interface view of the pure-state model
  (derives `contains`-style predicates from `ph_lookup_state` and proves
  `ph_contains_state p s <=> p IN ph_set s` under `ph_invariant`).
- `ParamHashInvariantGoalsScript.sml`: invariant-preservation goals for the
  build process (records “match preserves invariant”; currently `cheat`ed).
