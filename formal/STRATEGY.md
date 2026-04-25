# Formal verification strategy (top-down, refinement-friendly)

This directory collects the *formalisation side* of the F4/FPP verifier effort.
The intent is **not** to immediately prove everything end-to-end, but to:

1. Write down the *right* top-level claims about the fast and slow programs.
2. Refine those claims into smaller obligations (domain enumeration, set
   correctness, hash-table invariants, etc.).
3. Gradually replace assumptions/`cheat`s with proofs and/or justified external
   specs for Atlas/C++ FFI calls.

The key idea is to keep a clean separation:

- **Program-level structure**: lists, folds, hash-table control-flow, and the
  dependency graph between checks.
- **Semantic primitives**: Atlas/C++ operations exposed via Poly/ML FFI
  (`atlas_param_*`, group construction, KGB size, etc.).
- **Refinement glue**: relations tying program outputs to abstract set
  semantics.

## Theories and their roles

### HOL4: abstract set semantics + algorithmic skeletons

These live in `formal/hol4/`.

#### `F4FPPVerifySpecTheory`

File: `formal/hol4/F4FPPVerifySpecScript.sml`

Defines the *abstract semantics* used by both programs (assumption-heavy by
design):

- Abstract types: `group`, `ratvec`, `param`, and record `triple`.
- Uninterpreted semantic primitives:
  - `mk_param : group -> triple -> param`
  - `first_final_term : param -> param option`
  - `is_unitary : param -> bool`
- Abstract domain components (standing in for Atlas computations):
  - `KGB : group -> num set`
  - `FPP_lambdas : group -> num -> ratvec set`
  - `AllBarycenters : group -> ratvec set`
- Derived definitions:
  - `D_slow g` — the slow checker’s search domain as a set of triples.
  - `U_slow g dom` — the set of unitary final terms witnessed by `dom`.
  - `complete_rel g dom U_fast` — “no missing witness” definition of
    completeness of a candidate set `U_fast`.
  - `missing_witness g U_fast t` — the “one triple witnesses a missing element”
    predicate.

Proved (already, fully):

- `complete_rel_iff_no_missing`
- `complete_rel_imp_subset` (i.e. completeness implies `U_slow ⊆ U_fast`)

These are the set-level statements the slow checker is *meant* to justify.

#### `F4FPPVerifyAlgTheory`

File: `formal/hol4/F4FPPVerifyAlgScript.sml`

Models the slow checker’s control-flow *purely over lists*:

- `check_domain_fun g U_fast ts : num`
  counts how many `t ∈ ts` satisfy `missing_witness`.

Proved:

- `check_domain_fun_length_filter`
- `check_domain_fun_eq0_iff`
- `check_domain_fun_eq0_imp_complete_rel_list`

This isolates the “ML loop correctness” from Atlas semantics.

#### `F4FPPVerifyFastTheory`

File: `formal/hol4/F4FPPVerifyFastScript.sml`

Packages the *shape* of the final argument:

- `sound_wrt_domain g dom U_fast` (the soundness direction `U_fast ⊆ U_slow`)
- `sound_and_complete_gives_equality`

This separates “what we want to show about the fast program” from *how* the
fast program constructs `U_fast`.

#### `F4FPPVerifyFastRefineGoalsTheory` (refined fast-soundness obligations)

File: `formal/hol4/F4FPPVerifyFastRefineGoalsScript.sml`

Splits the single obligation `fast_sound g` into two compositional obligations
that match the fast program structure:

- `D_fast g` — the subset of triples actually enumerated/considered by the fast program.
- `fast_witnessed g` — every `pi ∈ U_fast g` is witnessed by some `t ∈ D_fast g`.
- `fast_domain_subset g` — the fast domain is included in the intended domain: `D_fast g ⊆ D_slow g`.

Proves an “OK” lemma:

- `fast_witnessed_and_subset_imp_fast_sound`:
  `fast_witnessed g ∧ fast_domain_subset g ⇒ fast_sound g`.

#### `F4FPPVerifyFastPruneGoalsTheory` (make fast pruning explicit)

File: `formal/hol4/F4FPPVerifyFastPruneGoalsScript.sml`

Introduces an explicit (still abstract) fast-side pruning predicate
`fast_considers g t` and defines the “obvious” pruned domain:

- `D_fast_pruned g = { t ∈ D_slow g | fast_considers g t }`

Then it provides glue lemmas showing that if the fast program’s abstract
`D_fast g` agrees with `D_fast_pruned g`, and every stored `pi ∈ U_fast g` is
witnessed by some `t` satisfying `fast_considers`, then the original obligations
`fast_domain_subset g` and `fast_witnessed g` hold.

#### `F4FPPVerifyFastPruneTraceGoalsTheory` (fast prune via a list witness)

File: `formal/hol4/F4FPPVerifyFastPruneTraceGoalsScript.sml`

Introduces an abstract list witness `fast_domain_trace g` intended to represent
the triples actually iterated by the fast compute phase, and defines its set
view `D_fast_trace g = set (fast_domain_trace g)`.

It then factors the pruning equality goal into more translator-friendly pieces:

- `fast_domain_trace_ok g`: `D_fast g = D_fast_trace g`
- `fast_domain_trace_sound g`: `D_fast_trace g ⊆ D_fast_pruned g`
- `fast_domain_trace_complete g`: `D_fast_pruned g ⊆ D_fast_trace g`

and proves (OK) that these imply the previously introduced pruning obligations
(`fast_domain_sound`, `fast_domain_complete`, hence `fast_domain_is_pruned`).

The corresponding execution bridges from `fast_compute_program_succeeds` are
recorded (currently `cheat`ed) in:

- `formal/hol4/F4FPPVerifyFastPruneTraceCheatsGoalsScript.sml`

#### `F4FPPVerifyFastWitnessTraceGoalsTheory` (fast witness via an insert trace)

File: `formal/hol4/F4FPPVerifyFastWitnessTraceGoalsScript.sml`

Introduces an abstract event trace `fast_insert_trace g : (triple # param) list`
intended to record “witness triple `t` produced param `pi` (and was inserted /
matched)”. It defines:

- `fast_insert_trace_sound g`: every event is justified by `first_final_term (mk_param g t) = SOME pi`
  and `t` satisfies the pruning predicate.
- `fast_insert_trace_covers_U_fast g`: every `pi ∈ U_fast g` appears in the trace.
- `fast_insert_trace_unitary g`: every traced `pi` is unitary.

It proves (OK) that these imply the earlier witness obligations
`fast_witnessed_pruned_exists` and `fast_unitary_set`.

Bridge placeholders from compute success are recorded (currently `cheat`ed) in:

- `formal/hol4/F4FPPVerifyFastWitnessTraceCheatsGoalsScript.sml`

#### `F4FPPVerifyGoalsTheory` (top-level goal layer)

File: `formal/hol4/F4FPPVerifyGoalsScript.sml`

Introduces abstract constants that represent the *actual program outputs*:

- `dom_list : group -> triple list` — the slow enumerator output.
- `fast_list : group -> param list` — the fast program’s accumulated params.
- `U_fast g = set (fast_list g)`
- `Dom g = set (dom_list g)`
- `slow_ok g` — the slow checker returns “no misses” on its list.
- `dom_list_correct g` — `Dom g = D_slow g`.
- `fast_sound g` — `sound_wrt_domain g (D_slow g) (U_fast g)`.

Proved (fully, list/set reasoning only):

- `slow_ok_imp_complete_rel`
- `slow_ok_and_fast_sound_gives_set_equality`
- `fast_ok_implies_correctness_goal`

Interpretation:

> If (i) the slow domain list is correct, (ii) the fast set is sound w.r.t.
> that domain, and (iii) the slow checker reports 0 misses on that domain,
> then the fast set equals the slow semantics.

This makes the main “equivalence” goal *explicitly depend* on three refinement
obligations.

#### `F4FPPVerifyDomainGoalsTheory` (domain refinement layer)

File: `formal/hol4/F4FPPVerifyDomainGoalsScript.sml`

Refines `dom_list_correct` into correctness of three component enumerations:

- `KGB_list : group -> num list`
- `FPP_lambdas_list : group -> num -> ratvec list`
- `AllBarycenters_list : group -> ratvec list`
- `dom_list_from_components g` — canonical nested `MAP/FLAT` product.
- `KGB_list_correct`, `FPP_lambdas_list_correct`,
  `AllBarycenters_list_correct`.
- Goal theorem:
  - `dom_list_from_components_correct` (proved)

This is meant to be discharged by routine list reasoning once we decide what
the concrete enumeration functions are (SML, Atlas FFI, fixtures, etc.).

#### `F4FPPVerifySlowRefineGoalsTheory` (refined slow completeness obligations)

File: `formal/hol4/F4FPPVerifySlowRefineGoalsScript.sml`

Defines a concrete (component-based) slow-success predicate:

- `slow_ok_components g`:
  `check_domain_fun g (U_fast g) (dom_list_from_components g) = 0`.

Proves an “OK” lemma composing the algorithmic and domain-refinement results:

- `slow_ok_components_imp_complete_rel`:
  if the component enumerators are correct and `slow_ok_components g` holds,
  then `complete_rel g (D_slow g) (U_fast g)`.

#### `F4FPPVerifyAlgAtlasEqGoalsTheory` and `F4FPPVerifySlowRefineAtlasEqGoalsTheory` (slow checker modulo `atlas_eq`)

Files:
- `formal/hol4/F4FPPVerifyAlgAtlasEqGoalsScript.sml`
- `formal/hol4/F4FPPVerifySlowRefineAtlasEqGoalsScript.sml`

Motivation:
- The real SML slow checker tests “membership in the fast set” via ParamHash,
  which uses the Atlas C++ semantic equality (`atlas_param_equal`), not HOL `=`.
- Therefore, the slow completeness contract is more naturally stated modulo
  `atlas_eq`.

What these theories introduce/prove (OK):
- `check_domain_fun_atlas_eq`: a list-fold miss counter using
  `missing_witness_atlas_eq` (from `F4FPPVerifySpecAtlasEqGoalsTheory`).
- `slow_ok_components_atlas_eq g`:
  `check_domain_fun_atlas_eq g (U_fast g) (dom_list_from_components g) = 0`.
- `slow_ok_components_atlas_eq_imp_complete_rel_atlas_eq`:
  under component correctness, `slow_ok_components_atlas_eq g` implies
  `complete_rel_atlas_eq g (D_slow g) (U_fast g)`.

#### `F4FPPVerifyComponentsBridgeGoalsTheory` (components → program outputs glue)

File: `formal/hol4/F4FPPVerifyComponentsBridgeGoalsScript.sml`

Introduces the explicit refinement assumption:

- `dom_list_is_components g`:
  `dom_list g = dom_list_from_components g`.

And proves “OK” glue lemmas:

- `dom_list_is_components_imp_dom_list_correct`:
  under component correctness, this implies `dom_list_correct g`.
- `dom_list_is_components_imp_slow_ok_iff`:
  `slow_ok g` is equivalent to the component-based predicate `slow_ok_components g`.

#### `F4FPPVerifyRefinedMainGoalsTheory` (refined main theorem, obligation-level)

File: `formal/hol4/F4FPPVerifyRefinedMainGoalsScript.sml`

Defines small “bundle” predicates and proves a refined top-level theorem that
derives the key equality using *only* the refined obligations:

- `slow_refinement_ok g`:
  `dom_list_is_components g` plus component-list correctness.
- `fast_semantic_ok g`:
  `fast_witnessed g ∧ fast_domain_subset g` (from `F4FPPVerifyFastRefineGoalsTheory`).
- `refined_obligations_imply_equivalence` (proved, OK):
  if `slow_refinement_ok`, `slow_ok_components`, `fast_semantic_ok`, and the
  bottom-layer predicate `bottom_layer_total_ok` hold, then `U_slow = U_fast`
  (and the bottom-layer predicate holds as well).

#### `F4FPPVerifyRefinedMainAtlasEqGoalsTheory` (refined main theorem modulo `atlas_eq`)

File: `formal/hol4/F4FPPVerifyRefinedMainAtlasEqGoalsScript.sml`

Provides the refined-main theorem in the more realistic conclusion form:

- `refined_obligations_imply_set_atlas_eq` (proved, OK):
  under `atlas_eq_equiv`, `slow_refinement_ok`, `slow_ok_components_atlas_eq`,
  `fast_semantic_ok`, and `bottom_layer_total_ok`, it derives
  `set_atlas_eq (U_fast g) (U_slow g (D_slow g))` (plus the bottom-layer
  postcondition).

#### `F4FPPBottomLayerGoalsTheory` (bottom-layer checks spec)

File: `formal/hol4/F4FPPBottomLayerGoalsScript.sml`

Captures, as *set-level predicates*, the suite of “bottom-layer” checks
implemented by `atlas-scripts-sml/FPP_globalDirac.sml`:

- `standard_final_ok U` — every element is standard and final.
- `lambda_table_set_ok g U` — if the group requires it (current code: `F4`),
  every element’s `lambda` is consistent with the precomputed table.
- `twist_equiv_ok U` — every element is equivalent to its twist.
- `hermitian_ok U` — every element is hermitian.
- `unitary_if dirac U` — if the Dirac flag is enabled, every element is unitary.
- `dual_closed U` — closure under contragredient (the “unitary dual symmetry”
  property that the original `.at` script checks).
- `bottom_layer_total_ok g dirac U` — mirrors the SML special-case for compact
  groups: instead of checks, require `rho_set g ⊆ U`.

This theory is intentionally *interface-first*: it introduces the abstract
primitives (`twist`, `param_equiv`, `contragredient`, etc.) that will later be
linked to Atlas FFI calls.

#### `F4FPPBottomLayerGoalsAtlasEqTheory` (bottom-layer checks modulo `atlas_eq`)

File: `formal/hol4/F4FPPBottomLayerGoalsAtlasEqScript.sml`

Adds modulo-`atlas_eq` variants of the bottom-layer predicates, replacing
representative membership `p IN U` by membership in the `atlas_eq`-closure
`mem_set_atlas_eq p U`:

- `standard_final_ok_atlas_eq`, `lambda_table_set_ok_atlas_eq`,
  `twist_equiv_ok_atlas_eq`, `hermitian_ok_atlas_eq`, `unitary_if_atlas_eq`,
  `dual_closed_atlas_eq`,
- `bottom_layer_ok_atlas_eq` and `bottom_layer_total_ok_atlas_eq`.

It also proves “lifting” lemmas showing that the original representative-level
predicates imply their modulo variants under the explicit congruence contracts
from `F4FPPVerifyAtlasFFIContractsGoalsTheory` (notably
`atlas_eq_congruent_bottom_layer`).

#### `F4FPPVerifyAtlasFFIContractsGoalsTheory` (explicit Atlas/FFI contract inventory)

File: `formal/hol4/F4FPPVerifyAtlasFFIContractsGoalsScript.sml`

Collects the *named* assumptions/obligations we will need about Atlas C++ calls
and their Poly/ML FFI wrappers, especially:

- `atlas_eq` / `atlas_hash_mod` / `atlas_clone` and the bundled predicate
  `atlas_hash_eq_ok` (equivalence + hash coherence + range + cloning),
- congruence/stability of semantic predicates (`is_unitary`, `is_final`, …)
  under `atlas_eq` (including `lambda_table_ok` and congruence of `param_equiv`
  in both arguments),
- a minimal algebraic law used by the bottom-layer (`contragredient` is an
  involution),
- a placeholder bundle `atlas_ffi_contracts`.

This theory is meant to keep the “trusted base” explicit: future bridge lemmas
should assume (or derive) `atlas_ffi_contracts` instead of implicitly relying
on FFI coherence.

#### `F4FPPBottomLayerAlgGoalsTheory` (bottom-layer list algorithm skeleton)

File: `formal/hol4/F4FPPBottomLayerAlgGoalsScript.sml`

Defines list-level versions of the bottom-layer checks (mirroring the SML
“iterate a list and filter/bail” control-flow) and proves equivalence with the
set-level predicates from `F4FPPBottomLayerGoalsTheory` when `U = set ps`.

This is the intended attachment point for showing:

- `FPP_globalDirac`’s list-based checks imply `bottom_layer_ok g dirac (U_fast g)`
  once we relate the SML enumeration `ParamHash.list` to `fast_list g`.

#### `F4FPPVerifyBottomLayerRefineGoalsTheory` (bottom-layer: fast list hook)

File: `formal/hol4/F4FPPVerifyBottomLayerRefineGoalsScript.sml`

Adds the tiny but useful lemma rewriting the bottom-layer goal for `U_fast g`
into a list-level goal on `fast_list g`. For non-compact groups it shows:

- `bottom_layer_total_ok g dirac (U_fast g)` is equivalent to
  `bottom_layer_ok_list g dirac (fast_list g)`.

#### `F4FPPBottomLayerParamSetGoalsTheory` (bottom-layer: param_set interface)

File: `formal/hol4/F4FPPBottomLayerParamSetGoalsScript.sml`

Models the `param_set` interface used by `FPP_globalDirac.sml` as a pair
`(ps, contains)` and isolates the extra data-structure obligation needed for
the unitary-dual check:

- `param_set_rep_ok (ps,contains) U`: `U = set ps` and `contains p ⇔ p ∈ U`.

It then defines a `contains`-based dual-closure check (matching the SML code)
and proves that, under `param_set_rep_ok`, it is equivalent to the set-level
predicate `dual_closed U`. This is the intended hook for connecting the
hash-table membership test (`ParamHash.contains`) to the abstract goal
`bottom_layer_ok`.

#### `F4FPPBottomLayerParamSetAtlasEqGoalsTheory` (bottom-layer: param_set modulo `atlas_eq`)

File: `formal/hol4/F4FPPBottomLayerParamSetAtlasEqGoalsScript.sml`

Provides the analogous refinement layer for the realistic membership semantics
of ParamHash/ParamSet:

- `param_set_rep_ok_atlas_eq (ps,contains) U`:
  `U = set ps` and `contains p ⇔ mem_set_atlas_eq p U`.

It proves that the SML dual-closure check based on `contains` implies the
modulo-`atlas_eq` set predicate `dual_closed_atlas_eq`, and that the combined
param_set-based bottom-layer predicate implies `bottom_layer_ok_atlas_eq`.

#### `F4FPPVerifyFastParamSetGoalsTheory` (fast output as a param_set)

File: `formal/hol4/F4FPPVerifyFastParamSetGoalsScript.sml`

Introduces an abstract `fast_param_set g` representing the `ParamHash` (or
similar) value produced by the fast program, viewed through the `param_set`
interface. It defines:

- `fast_param_set_ok g`: `fast_param_set g` represents exactly `U_fast g`.

and provides a simple glue lemma showing that, once `fast_param_set_ok` holds,
proving the SML-style predicate `bottom_layer_ok_param_set g dirac (fast_param_set g)`
is sufficient to conclude `bottom_layer_ok g dirac (U_fast g)`.

#### `F4FPPVerifyFastParamSetAtlasEqGoalsTheory` (fast output as a param_set modulo `atlas_eq`)

File: `formal/hol4/F4FPPVerifyFastParamSetAtlasEqGoalsScript.sml`

Adds the more realistic representation predicate:

- `fast_param_set_ok_atlas_eq g`: `param_set_rep_ok_atlas_eq (fast_param_set g) (U_fast g)`

meaning `contains p ⇔ mem_set_atlas_eq p (U_fast g)` rather than `p IN U_fast g`.
It provides glue lemmas showing that bottom-layer success over the param_set
implies the modulo-`atlas_eq` postconditions `bottom_layer_ok_atlas_eq` and
`bottom_layer_total_ok_atlas_eq`.

#### `F4FPPVerifyFastParamSetRefineGoalsTheory` (split `fast_param_set_ok`)

File: `formal/hol4/F4FPPVerifyFastParamSetRefineGoalsScript.sml`

Refines `fast_param_set_ok g` into two smaller obligations that match how the
SML code is used:

- `fast_param_set_list_ok g`: `ps_list (fast_param_set g) = fast_list g`
- `fast_param_set_contains_ok g`: `ps_contains (fast_param_set g) p ⇔ p ∈ U_fast g`

and provides the “OK” composition lemma:

- `fast_param_set_rep_ok g ⇒ fast_param_set_ok g`

This isolates exactly what we will eventually need to prove about the concrete
`ParamHash.list` and `ParamHash.contains` operations.

#### `F4FPPVerifyFastParamSetRefineAtlasEqGoalsTheory` (split `fast_param_set_ok_atlas_eq`)

File: `formal/hol4/F4FPPVerifyFastParamSetRefineAtlasEqGoalsScript.sml`

Provides the analogous refinement layer, defining:

- `fast_param_set_contains_ok_atlas_eq g`:
  `contains p ⇔ mem_set_atlas_eq p (U_fast g)`,

and bundling it with the existing list obligation `fast_param_set_list_ok g`.

#### `F4FPPVerifyFastParamSetContainsRefineGoalsTheory` (split `contains_ok`)

File: `formal/hol4/F4FPPVerifyFastParamSetContainsRefineGoalsScript.sml`

Splits `fast_param_set_contains_ok g` into the two one-way obligations:

- `fast_param_set_contains_sound g`: `contains p ⇒ p ∈ U_fast g`
- `fast_param_set_contains_complete g`: `p ∈ U_fast g ⇒ contains p`

and provides the “OK” recombination lemma back to the original biconditional.

#### `F4FPPVerifyFastParamSetContainsRefineAtlasEqGoalsTheory` (split `contains_ok_atlas_eq`)

File: `formal/hol4/F4FPPVerifyFastParamSetContainsRefineAtlasEqGoalsScript.sml`

Provides the analogous soundness/completeness split where the abstract set view
is `mem_set_atlas_eq p (U_fast g)`.

#### `F4FPPVerifyFastParamSetListRefineGoalsTheory` (split list enumeration)

File: `formal/hol4/F4FPPVerifyFastParamSetListRefineGoalsScript.sml`

Splits the list-enumeration correctness needed from `ParamHash.list` into:

- `fast_param_set_list_sound g`: every enumerated element is in `U_fast g`
- `fast_param_set_list_complete g`: every element of `U_fast g` is enumerated

From these it derives `set (ps_list (fast_param_set g)) = U_fast g`, and shows
how the list and contains obligations combine to yield `fast_param_set_ok g`.

#### `F4FPPVerifyGlobalDiracBridgeGoalsTheory` (bottom-layer bridge)

File: `formal/hol4/F4FPPVerifyGlobalDiracBridgeGoalsScript.sml`

Introduces the abstract success predicate
`bottom_layer_program_succeeds g dirac` representing successful execution of
the `FPP_globalDirac` pipeline.

The detailed “execution success ⇒ obligations” bridge lemmas (including the
modulo-`atlas_eq` total-postcondition) are isolated in:
- `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoalsScript.sml`

#### `F4FPPVerifyGlobalDiracBridgeDecomposeGoalsTheory` (bottom-layer bridge, per-check)

File: `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeGoalsScript.sml`

Decomposes the bottom-layer bridge into per-check obligations that mirror the
structure of `atlas-scripts-sml/FPP_globalDirac.sml`:

- `bl_standard_final_ok g`
- `bl_lambda_table_ok g`
- `bl_twist_equiv_ok g`
- `bl_hermitian_ok g`
- `bl_unitary_if_ok g dirac`
- `bl_dual_closed_ok g`
- `bl_rho_seeded_ok g` (compact groups: `rho_set g` is present via `contains`)

It provides the pure recombination lemma:
- `bl_checks_imp_bottom_layer_ok_param_set`

The per-check “execution success ⇒ obligation” bridge lemmas and their derived
downstream consequences (including the modulo-`atlas_eq` variants) are
isolated in:
- `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoalsScript.sml`

#### `F4FPPVerifyFastComputeBridgeGoalsTheory` (fast compute-phase bridge)

File: `formal/hol4/F4FPPVerifyFastComputeBridgeGoalsScript.sml`

Introduces a dedicated (currently `cheat`ed) bridge predicate
`fast_compute_program_succeeds g` for the compute phase that builds the fast
hash. It packages the intended compute obligations:

- `fast_domain_is_pruned g` and `fast_witnessed_pruned g` (domain + witness),
- sound/complete obligations for `fast_param_set`’s `list` and `contains`
  (used to derive `fast_param_set_ok g`),

and proves (OK) that these imply `fast_semantic_ok g`.

#### `F4FPPVerifyFastComputeBridgeDecomposeGoalsTheory` (fast compute bridge, factored)

File: `formal/hol4/F4FPPVerifyFastComputeBridgeDecomposeGoalsScript.sml`

Factors the compute-phase bridge into two explicit bundles:

- `fast_compute_domain_ok g`: `fast_domain_is_pruned g ∧ fast_witnessed_pruned g`
- `fast_compute_paramhash_ok g`: `paramhash_obligations_factored g` (the ParamHash bundle in factored form)

and proves (OK) that these together imply `fast_compute_obligations g`.

The “compute success ⇒ factored obligations” bridge lemmas are recorded (for
now, `cheat`ed) in the isolated theory:

- `formal/hol4/F4FPPVerifyFastComputeBridgeDecomposeCheatsGoalsScript.sml`

#### `F4FPPVerifyFastComputeParamHashStateRefineGoalsTheory` (compute ParamHash via state bundle)

File: `formal/hol4/F4FPPVerifyFastComputeParamHashStateRefineGoalsScript.sml`

Records the refinement-friendly route:

- `fast_compute_program_succeeds g ⇒ paramhash_obligations_state_factored g` (cheat, state witness)
- under `atlas_eq_is_hol_eq` + `atlas_hash_range`,
  `paramhash_obligations_state_factored g ⇒ fast_compute_paramhash_ok g` (OK)

This isolates the “state invariant” obligation as a dedicated proof target for
the CakeML/translator path.

#### `F4FPPVerifyParamHashBridgeGoalsTheory` (ParamHash list/contains obligations)

File: `formal/hol4/F4FPPVerifyParamHashBridgeGoalsScript.sml`

Introduces explicit abstract placeholders for the concrete SML `ParamHash`
observations produced by the compute phase:

- `paramhash_list g`
- `paramhash_contains g`

and a wiring predicate `fast_param_set_is_paramhash g` stating that
`fast_param_set g` is exactly the `(list,contains)` pair from ParamHash.

It then states the precise ParamHash obligations we ultimately need:

- list sound/complete w.r.t. `U_fast g`
- contains sound/complete w.r.t. `U_fast g`

and proves (OK) that these imply `fast_param_set_ok g`. A final bridge lemma
`fast_compute_program_succeeds_imp_paramhash_ok` is recorded (currently
`cheat`ed) in the isolated theory
`formal/hol4/F4FPPVerifyParamHashBridgeCheatsGoalsScript.sml`, to connect
concrete execution to these obligations without tainting the core theory.

#### `F4FPPVerifyParamHashBridgeDecomposeGoalsTheory` (ParamHash obligations, factored)

File: `formal/hol4/F4FPPVerifyParamHashBridgeDecomposeGoalsScript.sml`

Factors `paramhash_ok g` into two more re-usable obligations:

- `paramhash_rep_ok g`: `paramhash_contains g p ⇔ MEM p (paramhash_list g)`
  (pure data-structure correctness), and
- `paramhash_stores_U_fast g`: `∀p. p ∈ U_fast g ⇔ MEM p (paramhash_list g)`
  (algorithmic agreement with the goal-layer fast set).

More realistic (Atlas-facing) variants are also introduced:

- `paramhash_rep_ok_atlas_eq g`: `paramhash_contains g p ⇔ mem_atlas_eq p (paramhash_list g)`
- `paramhash_stores_U_fast_atlas_eq g`: `set_atlas_eq (U_fast g) (set (paramhash_list g))`
- `paramhash_obligations_factored_atlas_eq g`: the corresponding bundled form.

These support a top-down story where end-to-end equivalence is stated modulo the
FFI-provided semantic equality `atlas_eq`. For sanity, the theory proves that
under the simplifying assumption `atlas_eq_is_hol_eq`, the modulo-`atlas_eq`
obligations imply the plain ones.

It then proves (OK) that these imply `paramhash_ok g`, and records cheated
“compute success ⇒ obligations” lemmas in the split form in
`formal/hol4/F4FPPVerifyParamHashBridgeDecomposeCheatsGoalsScript.sml`. The
intent is:

- discharge `paramhash_rep_ok` via a CakeML hash-table proof, and
- discharge `paramhash_stores_U_fast` via a fast-compute semantic argument.

#### `F4FPPVerifyParamHashStateGoalsTheory` (ParamHash abstract state invariant)

File: `formal/hol4/F4FPPVerifyParamHashStateGoalsScript.sml`

Introduces an explicit abstract hash-table state `ph_state` (buckets + an
insertion-order list of stored parameters) and a minimal invariant
`ph_invariant`. Under the simplifying contract `atlas_eq_is_hol_eq` (and
`atlas_hash_range` for the bucket index), it proves:

- `ph_contains_state_iff_MEM_elems`: the lookup-derived membership predicate is
  equivalent to list membership in `elems`.

(This key lemma is now proved in HOL4. There is also an analogous proved lemma
for the CakeML pure-state model: `ph_rep_ok_iff_MEM_elems` in
`formal/cakeml/ParamHashSetGoalsScript.sml`.)

More realistic equality:

- In the intended end-to-end story, `atlas_eq` is an FFI-provided semantic
  equality and should not be assumed equal to HOL `=`.
- The corresponding state lemma is therefore stated in the same file as
  `ph_contains_state_iff_mem_atlas_eq_elems`, using `mem_atlas_eq` (membership
  modulo `atlas_eq`), and is now proved (no `cheat`) under `atlas_hash_eq_ok`
  (to get hash-respects-eq and range facts) and the state invariant.
- The supporting vocabulary lives in:
  - `formal/hol4/F4FPPVerifyAtlasEqListGoalsScript.sml` (`mem_atlas_eq`), and
  - `formal/hol4/F4FPPVerifyAtlasEqSetGoalsScript.sml` (`set_atlas_eq`,
    `atlas_eq_closure`) for stating end-to-end correctness modulo `atlas_eq`.

To make the `mem_atlas_eq ⇒ contains` direction more explicit (and to support
gradual proof strengthening), we also introduce a dedicated decomposition layer:

- `formal/hol4/F4FPPVerifyParamHashStateAtlasEqDecomposeGoalsScript.sml`

This file isolates the main hinge points (and now discharges them as ordinary
proofs): `ph_covered`/`ph_ok`/`ph_bucketed` “plumbing” facts, plus a pure list
lemma about `find_in_bucket` completeness modulo `atlas_eq`.

At the spec level (independent of ParamHash), the intended end-to-end equality
modulo `atlas_eq` is spelled out via two directional obligations in:

- `formal/hol4/F4FPPVerifySpecAtlasEqGoalsScript.sml`
  (`complete_rel_atlas_eq`, `sound_wrt_domain_atlas_eq`).

It then defines a wiring predicate `paramhash_observes_state` and derives:

- `paramhash_observes_state_and_invariant_imp_paramhash_rep_ok`

This is the intended attachment point for a future CakeML/translator proof
that the concrete imperative ParamHash state satisfies `ph_invariant`.

#### `F4FPPVerifyParamHashBridgeStateRefineGoalsTheory` (ParamHash state witness obligation)

File: `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineGoalsScript.sml`

Introduces a named intermediate bridge obligation:

- `paramhash_state_ok g`: there exists an abstract `ph_state` witness `s` such
  that `ph_invariant s` holds and `paramhash_list/contains` are exactly the
  observations of `s`.

It proves (modulo the state lemma) that this implies `paramhash_rep_ok g` under
`atlas_eq_is_hol_eq` + `atlas_hash_range`, and records the cheated bridge
`fast_compute_program_succeeds ⇒ paramhash_state_ok` in:

- `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineCheatsGoalsScript.sml`

More realistic equality:

- It also proves a modulo-`atlas_eq` variant:
  `atlas_hash_eq_ok ∧ paramhash_state_ok g ⇒ paramhash_rep_ok_atlas_eq g`,
  using the (now fully proved) state lemma `ph_contains_state_iff_mem_atlas_eq_elems`.

#### `F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory` (compute success ⇒ state-level ParamHash bundle)

File: `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeGoalsScript.sml`

Introduces a “state-factored” ParamHash bundle:

- `paramhash_obligations_state_factored g`:
  `fast_param_set_is_paramhash g ∧ paramhash_state_ok g ∧ paramhash_stores_U_fast g`.

Then proves (OK) that under `atlas_eq_is_hol_eq` + `atlas_hash_range` this
implies the earlier extensional bundle `paramhash_obligations_factored g`, and
records the cheated bridge in:

- `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsScript.sml`
  (`fast_compute_program_succeeds g ⇒ paramhash_obligations_state_factored g`).

Modulo-`atlas_eq` variant:

- It also introduces `paramhash_obligations_state_factored_atlas_eq g` where the
  “stores-U-fast” clause is stated as a set equality modulo `atlas_eq`:
  `paramhash_stores_U_fast_atlas_eq g`.
- Under `atlas_hash_eq_ok` it implies `paramhash_obligations_factored_atlas_eq g`,
  and a separate lemma shows that under `atlas_eq_is_hol_eq` this implies the
  plain `paramhash_obligations_factored g`.

#### `F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory` (canonical build-state witness)

File: `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceGoalsScript.sml`

Introduces an explicit *canonical* witness state for the compute-phase
ParamHash, parameterized by two abstract execution observations:

- `paramhash_build_m g` : bucket count used during allocation,
- `paramhash_build_ps g` : the trace of attempted `match`/insert calls.

It defines the pure witness state:

- `paramhash_build_state g = ph_build_from_create_state (paramhash_build_m g) (paramhash_build_ps g)`

and a compact obligation `paramhash_build_state_ok g` stating that `m ≠ 0` and
the observable `(paramhash_list g, paramhash_contains g)` view agrees with this
pure model via `paramhash_observes_state`.

Related decomposition (OK):
- `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceRefineGoalsScript.sml`
  splits `paramhash_build_state_ok` into smaller “translator-friendly” facts:
  `paramhash_build_m_ok`, `paramhash_build_ps_ok` (currently weak), plus
  separate list/contains observation clauses.

#### `F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsTheory` (stores-U-fast predicates)

File: `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsScript.sml`

Adds two ways to state “the build result stores exactly `U_fast` (modulo
`atlas_eq`)”:

- state-based: `paramhash_build_stores_U_fast_atlas_eq g` phrased against
  `ph_set (paramhash_build_state g)`, and
- trace-based: `paramhash_build_ps_stores_U_fast_atlas_eq g` phrased against
  `set (paramhash_build_ps g)`.

These let the translator/CakeML path pick the most convenient proof target.

Directional decomposition (OK):
- `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceStoreDecomposeGoalsScript.sml`
  factors the trace-based predicate into two “inclusion” obligations
  (`U_fast -> trace` and `trace -> U_fast`) under `atlas_eq_equiv`, and proves
  recombination lemmas. It also provides a second decomposition phrased
  directly in terms of `mem_set_atlas_eq`, which avoids needing any assumptions
  about `atlas_eq` when merely unfolding `set_atlas_eq`.

#### `F4FPPVerifyParamHashStateBuildSetGoalsTheory` (pure build-set lemma)

File: `formal/hol4/F4FPPVerifyParamHashStateBuildSetGoalsScript.sml`

Proves (OK, no `cheat`) the key pure lemma connecting the two store views:

- `ph_build_from_create_state_set_atlas_eq_set_ps`:
  `set_atlas_eq (set ps) (ph_set (ph_build_from_create_state m ps))`
  under `atlas_hash_range` + `atlas_eq_equiv` + `m ≠ 0`.

This is the main tool for upgrading/downgrading between the trace-level stores
predicate and the state-based one.

#### `F4FPPVerifyParamHashBridgeStateBuildTraceStores*` (bundles and bridge targets)

Files:
- `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresDecomposeGoalsScript.sml`
- `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresDecomposeCheatsGoalsScript.sml`
- `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeGoalsScript.sml`
- `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeCheatsGoalsScript.sml`
- `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceBridgeCheatsGoalsScript.sml`

These theories package the wiring (`fast_param_set_is_paramhash`) together with
`paramhash_build_state_ok` and either store predicate into named bundles, and
record the intended translator targets as explicit (currently `cheat`ed) bridge
lemmas, most notably:

- `fast_compute_program_succeeds g ==> paramhash_build_ps_stores_U_fast_atlas_eq g`
  (direct trace-based target), and/or
- `fast_compute_program_succeeds g ==> paramhash_build_stores_U_fast_atlas_eq g`
  (state-based target phrased against `ph_set`).

Adapter (OK):
- `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceToStateGoalsScript.sml`
  shows how to convert the trace-level canonical-build bundle into the existing
  state-level ParamHash bundle required by the end-to-end obligation stack,
  under `atlas_hash_eq_ok`.

#### `F4FPPVerifySlowBridgeDetailedGoalsTheory` (slow bridge, split obligations)

File: `formal/hol4/F4FPPVerifySlowBridgeDetailedGoalsScript.sml`

Splits the slow bridge into two obligations:

- `slow_program_succeeds g` implies `slow_refinement_ok g` (domain refinement),
- `slow_program_succeeds g` implies `slow_ok_components g` (0 misses),

and provides an “OK” convenience lemma bundling them together.

The remaining `cheat`ed bridge for the refinement part lives in:

- `formal/hol4/F4FPPVerifySlowBridgeDetailedCheatsGoalsScript.sml`

#### `F4FPPVerifySlowProgramDecomposeBridgeGoalsTheory` (slow bridge, finer split)

File: `formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeGoalsScript.sml`

Introduces abstract constants for the slow program’s internal structure:

- `slow_domain_list g`: the triple enumeration order (if materialized),
- `slow_missing g t`: the per-triple “missing witness?” predicate,
  and (more realistic)
  - `slow_missing_atlas_eq g t`: missing predicate where membership in the fast
    set is interpreted modulo `atlas_eq`,

and defines three small bridge obligations:

- `slow_missing_ok g`: `slow_missing g t ⇔ missing_witness g (U_fast g) t`
- `slow_domain_list_is_components g`: `slow_domain_list g = dom_list_from_components g`
- `slow_ok_sml g`: `LENGTH (FILTER (slow_missing g) (slow_domain_list g)) = 0`

From these it proves (OK) that `slow_ok_components g` holds, and then records
cheated lemmas (in an isolated theory) stating that `slow_program_succeeds g`
implies each obligation:

- `formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeCheatsGoalsScript.sml`

Modulo-`atlas_eq` variant:

- `slow_missing_ok_atlas_eq g`: `slow_missing_atlas_eq g t ⇔ missing_witness_atlas_eq g (U_fast g) t`
- `slow_ok_sml_atlas_eq g`: `LENGTH (FILTER (slow_missing_atlas_eq g) (slow_domain_list g)) = 0`

From these it similarly derives (OK) that `slow_ok_components_atlas_eq g` holds.

#### `F4FPPVerifySlowRefinementBridgeDecomposeGoalsTheory` (slow refinement: dom_list vs slow loop)

File: `formal/hol4/F4FPPVerifySlowRefinementBridgeDecomposeGoalsScript.sml`

Decomposes the slow-side refinement bundle `slow_refinement_ok g` further by
introducing an explicit intermediate agreement predicate:

- `dom_list_is_slow_domain_list g`: `dom_list g = slow_domain_list g`

Together with `slow_domain_list_is_components g` (from the previous theory),
this yields `dom_list_is_components g`, and therefore (combined with component
list correctness) yields `slow_refinement_ok g`.

#### `F4FPPVerifyTargetGroupGoalsTheory` (fix the concrete `F4_s` instance)

File: `formal/hol4/F4FPPVerifyTargetGroupGoalsScript.sml`

Introduces a constant `F4s : group` intended to denote the split real form
`F4_s` used by the SML scripts, and records key group facts (currently as
axioms) such as `~group_is_compact F4s` and `needs_lambda_table_check F4s`.

#### `F4FPPVerifyFastProgramSplitBridgeGoalsTheory` (fast program = two phases)

File: `formal/hol4/F4FPPVerifyFastProgramSplitBridgeGoalsScript.sml`

Records the split of `fast_program_succeeds` into:

- `fast_compute_program_succeeds` (compute/hash construction), and
- `bottom_layer_program_succeeds` (GlobalDirac bottom-layer checks),

and composes the phase-level bridge theorems to derive refined fast obligations
for the concrete target group `F4s`.

The split lemma itself is isolated (currently `cheat`ed) in:
- `formal/hol4/F4FPPVerifyFastProgramSplitBridgeCheatsGoalsScript.sml`

#### `F4FPPVerifyEndToEndF4sGoalsTheory` (end-to-end theorem for `F4s`)

File: `formal/hol4/F4FPPVerifyEndToEndF4sGoalsScript.sml`

Composes:

- the refined main theorem `refined_obligations_imply_equivalence`,
- the fast phase-split bridge lemma specialized to `F4s`, and
- the detailed slow-bridge lemma,

to obtain the concrete end-user theorem:

- if `fast_program_succeeds F4s dirac` and `slow_program_succeeds F4s` then
  `U_slow F4s (D_slow F4s) = U_fast F4s` and `bottom_layer_total_ok` holds.

#### `F4FPPVerifyEndToEndObligationStackGoalsTheory` (explicit obligation stack)

File: `formal/hol4/F4FPPVerifyEndToEndObligationStackGoalsScript.sml`

Provides a “maximally explicit” end-to-end theorem
`obligations_stack_imply_equivalence` that derives the refined main result from
premises that match the program’s phase structure:

- Atlas/FFI hash/equality contracts (`atlas_eq_is_hol_eq`, `atlas_hash_range`)
- compute-phase semantic obligations (`fast_compute_domain_ok g`)
- compute-phase ParamHash state bundle (`paramhash_obligations_state_factored g`)
- bottom-layer checker predicate at the param_set interface
  (`bottom_layer_ok_param_set g dirac (fast_param_set g)`)
- non-compactness (`~group_is_compact g`)
- slow-side refined obligations (`slow_refinement_ok g`, `slow_ok_components g`)

This gives a single place to see the full list of obligations that still need
to be discharged to obtain end-to-end equivalence.

This file also now records the **fully modulo-`atlas_eq`** obligation-stack
theorem:

- `obligations_stack_imply_set_atlas_eq_fast_atlas_eq_and_bottom_layer_total_ok_atlas_eq`

which avoids `atlas_eq_is_hol_eq` entirely by:

- phrasing the compute phase via `fast_compute_obligations_atlas_eq` (and
  deriving `fast_semantic_ok_atlas_eq` + `fast_param_set_ok_atlas_eq`), and
- phrasing the bottom-layer phase via `bottom_layer_total_ok_param_set_atlas_eq`
  and then lifting to `bottom_layer_total_ok_atlas_eq`.

#### `F4FPPVerifyEndToEndProgramSuccessStackGoalsTheory` (explicit end-to-end from program success)

File: `formal/hol4/F4FPPVerifyEndToEndProgramSuccessStackGoalsScript.sml`

Records a single theorem
`program_success_implies_equivalence_via_obligation_stack` that composes:

- `fast_program_succeeds ⇒ (compute succeeds ∧ bottom-layer succeeds)` (phase split)
- `compute succeeds ⇒ fast_compute_domain_ok` (cheat)
- `compute succeeds ⇒ paramhash_obligations_state_factored` (cheat)
- `bottom-layer succeeds ⇒ bottom_layer_ok_param_set` (via per-check decomposition)
- `slow succeeds ⇒ (slow_refinement_ok ∧ slow_ok_components)` (cheat-tainted)
- `obligations_stack_imply_equivalence` (OK)

This is the most explicit “roadmap theorem” for the full argument: it shows
exactly which bridge lemmas remain to be proved/justified.

It also records an additional ParamHash route that makes the trace-level
approach explicit:

- `program_success_implies_equivalence_via_obligation_stack_paramhash_atlas_eq_via_build_state_trace_stores`

This variant uses the trace-level stores bundle plus the adapter lemma
`atlas_hash_eq_ok_and_build_state_trace_stores_factored_atlas_eq_imp_paramhash_obligations_state_factored_atlas_eq`
to feed into the same obligation stack interface.

It also records the fully modulo-`atlas_eq` program-success theorem:

- `program_success_implies_set_atlas_eq_fast_atlas_eq_and_bottom_layer_total_ok_atlas_eq_via_obligation_stack`

whose conclusion matches the refined-main modulo theory and the modulo bottom-layer
postcondition.

#### `F4FPPVerifyFullGoalsTheory` (packaged top-level statement)

File: `formal/hol4/F4FPPVerifyFullGoalsScript.sml`

Packages the main top-level predicates into a single conjunction:

- `fast_checks_ok g dirac` — the bottom-layer invariants hold for `U_fast g`.
- `fast_ok g dirac` — fast set is sound and passes bottom-layer checks.
- `full_ok g dirac` — domain correct + slow checker reports OK + fast OK.

And derives convenient “end-user” theorems such as:

- `full_ok_implies_set_equality`:
  if both programs meet their obligations, then `U_slow = U_fast`.

#### `F4FPPVerifySMLBridgeGoalsTheory` (connection to concrete SML runs)

File: `formal/hol4/F4FPPVerifySMLBridgeGoalsScript.sml`

Introduces abstract “program success” predicates:

- `fast_program_succeeds g dirac`
- `slow_program_succeeds g`

The (currently `cheat`ed) bridge theorems live in:

- `formal/hol4/F4FPPVerifySMLBridgeCheatsGoalsScript.sml`

They include the key bridge theorems we ultimately want:

- `fast_program_succeeds_imp_fast_ok` (**currently `cheat`ed**)
- `slow_program_succeeds_imp_dom_and_slow_ok` (**currently `cheat`ed**)
- `slow_program_succeeds_imp_dom_list_correct` (**currently `cheat`ed**)
- `slow_program_succeeds_imp_slow_ok` (**currently `cheat`ed**)
- `fast_and_slow_programs_succeed_imp_full_ok` (**CHEAT-tainted**, depends on cheated bridge lemmas)
- `fast_and_slow_programs_succeed_gives_equivalence` (**CHEAT-tainted**, derived by composition)

From these, we get a clean end-user theorem statement (intended to be derivable
once the bridge lemmas are proved without `cheat`):

- `fast_and_slow_programs_succeed_gives_equivalence`:
  if both programs succeed, then `U_slow = U_fast` and the bottom-layer
  invariants hold for `U_fast`.

#### `F4FPPVerifyRefinedBridgeGoalsTheory` (bridge at refined-obligation level)

File: `formal/hol4/F4FPPVerifyRefinedBridgeGoalsScript.sml`

Refines the bridge interface further: instead of “program success implies
`full_ok`”, it states (currently `cheat`ed, in an isolated theory) that:

- `formal/hol4/F4FPPVerifyRefinedBridgeCheatsGoalsScript.sml`

- `fast_program_succeeds g dirac` implies
  `fast_semantic_ok g` and `bottom_layer_total_ok g dirac (U_fast g)`.
- `slow_program_succeeds g` implies
  `slow_refinement_ok g` and `slow_ok_components g`.

Modulo-`atlas_eq` slow variant:

- the slow-side “0 misses” claim is also stated in a more realistic form:
  `slow_program_succeeds g ⇒ slow_ok_components_atlas_eq g`,
  leading to an end-user theorem phrased with `set_atlas_eq`:
  `fast_and_slow_programs_succeed_gives_refined_equivalence_atlas_eq`.
  This theorem additionally assumes `atlas_hash_eq_ok` (to obtain
  `atlas_eq_equiv`).

Modulo-`atlas_eq` fast variant:

- a more realistic fast-side bridge obligation is also recorded:
  `fast_program_succeeds g dirac ⇒ fast_semantic_ok_atlas_eq g`,
  where `fast_witnessed_atlas_eq` only requires semantic equality of witnesses
  (via `atlas_eq`), not HOL equality of representatives.

Then it derives:

- `fast_and_slow_programs_succeed_gives_refined_equivalence`
  (logically OK, but depends on the cheated bridge obligations),
  by composing these with `refined_obligations_imply_equivalence`.

### CakeML/HOL: translator-facing model(s)

These live in `formal/cakeml/`.

#### `ParamHashProgTheory` (translator checkpoint)

File: `formal/cakeml/ParamHashProgScript.sml`

Provides a first CakeML-monadic model of a ParamHash-like structure and
translates it with CakeML’s monadic translator. It also defines a pure “state
spec” (`ph_*_state`) and some invariants (`ph_ok`, `ph_bucketed`, `ph_covered`,
`ph_invariant`) for staged reasoning.

Important: this theory is kept building as **OK** (no `cheat`), because it is a
translator checkpoint and a stable base for later refinement work.

#### `ParamHashGoalsTheory` (top-down goal layer)

File: `formal/cakeml/ParamHashGoalsScript.sml`

Collects the ParamHash correctness obligations we ultimately want:

- `ph_lookup_state_complete`
- `ph_all_present_state_iff_subset`
- `ph_lookup_state_complete_wrt_set`

These are currently `cheat`ed on purpose; the goal is to “freeze” the intended
interfaces/claims before investing in proofs.

#### `ParamHashBuildGoalsTheory` (pure build base)

File: `formal/cakeml/ParamHashBuildGoalsScript.sml`

Provides a small, non-cheated base layer that both refinement and invariant
theories can share without importing cheat-tainted dependencies:

- list/nthn helper lemmas about append (`nthn_append_lt`, `nthn_append_sing_len`)
- `ph_match_state_preserves_ok`: a single pure `match_state` step preserves `ph_ok`
- the pure-state “build-by-repeated-match” function `ph_build_state`

#### `ParamHashRefinementGoalsTheory` (monadic ops ⇔ pure-state model)

File: `formal/cakeml/ParamHashRefinementGoalsScript.sml`

Records the next “glue” layer needed for a CakeML discharge of the HOL4
ParamHash bridges: refinement goals stating that the translated monadic
operations (`ph_lookup`, `ph_match`, `ph_insert_all`, `ph_all_present`) behave
exactly like the pure-state functions (`ph_lookup_state`, `ph_match_state`,
`ph_build_state`, `ph_all_present_state`) under `ph_ok`/`ph_invariant`.

These are proved largely by unfolding the monadic definitions and applying the
array/list bounds consequences of `ph_ok`. However, the theory is currently
**OK**: it relies on the base lemma `ph_match_state_preserves_ok` (proved, no
`cheat`, in `ParamHashBuildGoalsTheory`) to justify the iterative `insert_all`
refinement. The intent is for this layer to remain **OK** (no `cheat`) as a
stable interface between:

- translator evaluation proofs (CakeML semantics), and
- the already-proved extensional lemmas in `ParamHashSetGoalsTheory`.

#### `ParamHashCreateGoalsTheory` (monadic create ⇔ pure initial state)

File: `formal/cakeml/ParamHashCreateGoalsScript.sml`

Pins down the missing initialization layer for the ParamHash story:

- defines the pure initial state `ph_create_state m`, and proves it satisfies
  `ph_invariant` for `m ≠ 0`;
- records the refinement goal `ph_create_refines_create_state`, connecting the
  monadic `ph_create` operation (translator-level) to that pure initial state
  (now proved by unfolding the monadic definitions).

This is the intended starting point for any end-to-end “create; insert_all”
proof, and it complements `ParamHashRefinementGoalsTheory`, which assumes a
well-formed starting state.

#### `ParamHashEndToEndGoalsTheory` (create; insert_all; query)

File: `formal/cakeml/ParamHashEndToEndGoalsScript.sml`

Packages the “create then build” story as an explicit pure reference model:

- `ph_build_from_create_state m ps = ph_build_state ps (ph_create_state m)`

and records a single top-level refinement goal:

- `ph_build_into_new_refines_build_from_create_state`

This is the layer we eventually want to point at when discharging HOL4-side
ParamHash state obligations from a CakeML evaluation proof: it avoids repeatedly
recomposing `create`/`insert_all` facts at every callsite.

## How the overall proof will be staged

This matches the intent of `VERIFY_ESTIMATE.md`, but with the current theory
stack as concrete artefacts.

## Mapping: SML modules → formal obligations (current best guess)

This section is a “wiring diagram” for where future proofs/specs should land.
It is intentionally redundant with the theory summaries above, but grouped by
code location rather than by theory name.

- `atlas-scripts-sml/VerifyF4FPP.sml`:
  should discharge the bridge obligations in
  `formal/hol4/F4FPPVerifyRefinedBridgeGoalsScript.sml` for the fast side, i.e.
  produce `fast_semantic_ok g` and `bottom_layer_total_ok g dirac (U_fast g)`
  upon success.
- `atlas-scripts-sml/F4_FPP_points_compute.sml`:
  is the main source of the fast-semantic obligations:
  `fast_witnessed g` and `fast_domain_subset g` (hence `fast_semantic_ok g`).
  It will likely require additional intermediate lemmas about:
  - what triples are *considered* (`D_fast g`), and
  - how each stored parameter in `U_fast g` relates to a witness triple.
- `atlas-scripts-sml/FPP_globalDirac.sml`:
  should discharge `bottom_layer_total_ok g dirac (U_fast g)` by linking each
  check (standard/final, lambda-table, twist-equivalence, hermitian, unitary
  if Dirac, contragredient closure) to the corresponding predicates in
  `formal/hol4/F4FPPBottomLayerGoalsScript.sml`.
- `atlas-scripts-sml/SimplerVerifyF4FPP.sml`:
  should discharge the slow-side bridge obligations:
  `slow_refinement_ok g` and `slow_ok_components g`.
  Concretely, this means linking:
  - the `for_domain` loop to `dom_list_from_components`,
  - the “missing witness” predicate to `missing_witness`,
  - and the counter to `check_domain_fun`.
- `atlas-scripts-sml/ParamHash.sml` (plus `formal/cakeml/ParamHashProgScript.sml`):
  is expected to justify that the imperative hash structure implements the set
  view used in the HOL4 developments (`U_fast g`), under suitable specs for the
  Atlas equality/hash FFI.

### Stage 1: “assumption-heavy” program correctness (structure)

Goal: reduce the main statement to small, explicit obligations about:

1. **Domain enumeration** (what triples are checked by the slow program?)
2. **Fast set semantics** (what `U_fast` is represented by the fast program’s
   `ParamHash`?)
3. **Soundness** (every `pi ∈ U_fast` is a unitary final term of some domain
   triple).
4. **Completeness** (if the slow program finds no missing witness, then
   `U_slow ⊆ U_fast`).

This is exactly what `F4FPPVerifyGoalsTheory` makes explicit.

### Stage 2: refine “sets of params” to an imperative hash-table

Goal: connect `U_fast` (a set) to the concrete `ParamHash` implementation used
in `atlas-scripts-sml/ParamHash.sml`:

- Define a set view of `ParamHash.t` (extensional, modulo `atlas_param_equal`).
- Specify/assume properties of `AtlasParam.eq` and `AtlasParam.hash_mod`.
- Prove that `ParamHash.match` implements insertion into that set view, and
  `ParamHash.contains` implements membership (no false negatives/positives).

The CakeML side models this as:

- `ParamHashProgTheory` (monadic translator model),
- `ParamHashGoalsTheory` (lookup/all-present completeness goals), and
- `ParamHashBuildGoalsTheory` (pure build base: `ph_build_state`, nthn lemmas),
- `ParamHashCreateGoalsTheory` (create/init refinement + initial invariants),
- `ParamHashRefinementGoalsTheory` (monadic ops ⇔ pure-state refinement goals),
- `ParamHashEndToEndGoalsTheory` (composed create+build refinement goal),
- `ParamHashSetGoalsTheory` (set-interface view: `contains` ↔ membership).

### Stage 3: attach Atlas/C++ FFI semantics

Goal: treat Atlas calls as external specs initially, then tighten over time.

Examples of external specs we will need (in some form):

- `atlas_param_equal` is an equivalence relation consistent with
  `atlas_param_hash`.
- `atlas_param_clone` preserves extensional equality.
- `atlas_param_twist`, `atlas_param_contragredient` preserve semantic meaning.
- `atlas_param_is_unitary` matches the abstract predicate `is_unitary`.
- Construction/finalisation (`atlas_param_new_*`, `atlas_param_finals`) match
  `mk_param` and `first_final_term`.

Initially, these are assumptions connecting the “abstract HOL4 world” to the
concrete SML+FFI world. Later stages can validate or partially prove them.
The current inventory of such assumptions is centralized in
`formal/hol4/F4FPPVerifyAtlasFFIContractsGoalsScript.sml`.

## Current “what to prove next” checklist (spec-first)

If we continue in the same top-down style, the next useful artefacts are:

1. A HOL4 goal theory that characterises the **fast program output**:
   connect `U_fast` (from `F4FPPVerifyGoalsTheory`) to
   `bottom_layer_total_ok` (from `F4FPPBottomLayerGoalsTheory`), and introduce a
   `fast_ok g`/`fast_result g` wrapper that matches the control-flow of
   `VerifyF4FPP.sml`.
2. A HOL4 goal theory that links the **concrete SML enumerators** for
   `SimplerVerifyF4FPP` to `dom_list_from_components`.
3. A CakeML goal theory that expresses the **state/array/ref semantics** needed
   to connect the translated monadic operations to the pure-state invariants.

As we add these, the “main theorem” stays stable; we just refine its
dependencies into smaller obligations.
