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

#### `F4FPPVerifyAtlasFFIContractsGoalsTheory` (explicit Atlas/FFI contract inventory)

File: `formal/hol4/F4FPPVerifyAtlasFFIContractsGoalsScript.sml`

Collects the *named* assumptions/obligations we will need about Atlas C++ calls
and their Poly/ML FFI wrappers, especially:

- `atlas_eq` / `atlas_hash_mod` / `atlas_clone` and the bundled predicate
  `atlas_hash_eq_ok` (equivalence + hash coherence + range + cloning),
- congruence/stability of semantic predicates (`is_unitary`, `is_final`, …)
  under `atlas_eq`,
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

#### `F4FPPVerifyFastParamSetGoalsTheory` (fast output as a param_set)

File: `formal/hol4/F4FPPVerifyFastParamSetGoalsScript.sml`

Introduces an abstract `fast_param_set g` representing the `ParamHash` (or
similar) value produced by the fast program, viewed through the `param_set`
interface. It defines:

- `fast_param_set_ok g`: `fast_param_set g` represents exactly `U_fast g`.

and provides a simple glue lemma showing that, once `fast_param_set_ok` holds,
proving the SML-style predicate `bottom_layer_ok_param_set g dirac (fast_param_set g)`
is sufficient to conclude `bottom_layer_ok g dirac (U_fast g)`.

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

#### `F4FPPVerifyFastParamSetContainsRefineGoalsTheory` (split `contains_ok`)

File: `formal/hol4/F4FPPVerifyFastParamSetContainsRefineGoalsScript.sml`

Splits `fast_param_set_contains_ok g` into the two one-way obligations:

- `fast_param_set_contains_sound g`: `contains p ⇒ p ∈ U_fast g`
- `fast_param_set_contains_complete g`: `p ∈ U_fast g ⇒ contains p`

and provides the “OK” recombination lemma back to the original biconditional.

#### `F4FPPVerifyFastParamSetListRefineGoalsTheory` (split list enumeration)

File: `formal/hol4/F4FPPVerifyFastParamSetListRefineGoalsScript.sml`

Splits the list-enumeration correctness needed from `ParamHash.list` into:

- `fast_param_set_list_sound g`: every enumerated element is in `U_fast g`
- `fast_param_set_list_complete g`: every element of `U_fast g` is enumerated

From these it derives `set (ps_list (fast_param_set g)) = U_fast g`, and shows
how the list and contains obligations combine to yield `fast_param_set_ok g`.

#### `F4FPPVerifyGlobalDiracBridgeGoalsTheory` (bottom-layer bridge)

File: `formal/hol4/F4FPPVerifyGlobalDiracBridgeGoalsScript.sml`

Introduces a dedicated (currently `cheat`ed) bridge predicate
`bottom_layer_program_succeeds g dirac` representing successful execution of
the `FPP_globalDirac` pipeline. It records the intended bridge theorem:

- success implies `bottom_layer_ok_param_set g dirac (fast_param_set g)`

and then derives (without further cheating) the set-level consequences under
`fast_param_set_ok`, including `bottom_layer_total_ok` for non-compact groups.

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

It then records cheated lemmas stating that `fast_compute_program_succeeds g`
implies each bundle, and derives a factored “success ⇒ obligations” theorem
from those.

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
`cheat`ed) to connect concrete execution to these obligations.

#### `F4FPPVerifyParamHashBridgeDecomposeGoalsTheory` (ParamHash obligations, factored)

File: `formal/hol4/F4FPPVerifyParamHashBridgeDecomposeGoalsScript.sml`

Factors `paramhash_ok g` into two more re-usable obligations:

- `paramhash_rep_ok g`: `paramhash_contains g p ⇔ MEM p (paramhash_list g)`
  (pure data-structure correctness), and
- `paramhash_stores_U_fast g`: `∀p. p ∈ U_fast g ⇔ MEM p (paramhash_list g)`
  (algorithmic agreement with the goal-layer fast set).

It then proves (OK) that these imply `paramhash_ok g`, and records cheated
“compute success ⇒ obligations” lemmas in the split form. The intent is:

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

(Currently this key lemma is `cheat`ed; the surrounding definitions are meant
to make it straightforward to replace the `cheat` by a real invariant proof.)

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
`fast_compute_program_succeeds ⇒ paramhash_state_ok`.

#### `F4FPPVerifySlowBridgeDetailedGoalsTheory` (slow bridge, split obligations)

File: `formal/hol4/F4FPPVerifySlowBridgeDetailedGoalsScript.sml`

Splits the slow bridge into two explicit (currently `cheat`ed) obligations:

- `slow_program_succeeds g` implies `slow_refinement_ok g` (domain refinement),
- `slow_program_succeeds g` implies `slow_ok_components g` (0 misses),

and provides an “OK” convenience lemma bundling them together.

#### `F4FPPVerifySlowProgramDecomposeBridgeGoalsTheory` (slow bridge, finer split)

File: `formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeGoalsScript.sml`

Introduces abstract constants for the slow program’s internal structure:

- `slow_domain_list g`: the triple enumeration order (if materialized),
- `slow_missing g t`: the per-triple “missing witness?” predicate,

and defines three small bridge obligations:

- `slow_missing_ok g`: `slow_missing g t ⇔ missing_witness g (U_fast g) t`
- `slow_domain_list_is_components g`: `slow_domain_list g = dom_list_from_components g`
- `slow_ok_sml g`: `LENGTH (FILTER (slow_missing g) (slow_domain_list g)) = 0`

From these it proves (OK) that `slow_ok_components g` holds, and then records
cheated lemmas stating that `slow_program_succeeds g` implies each obligation.

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

#### `F4FPPVerifyEndToEndF4sGoalsTheory` (end-to-end theorem for `F4s`)

File: `formal/hol4/F4FPPVerifyEndToEndF4sGoalsScript.sml`

Composes:

- the refined main theorem `refined_obligations_imply_equivalence`,
- the fast phase-split bridge lemma specialized to `F4s`, and
- the detailed slow-bridge lemma,

to obtain the concrete end-user theorem:

- if `fast_program_succeeds F4s dirac` and `slow_program_succeeds F4s` then
  `U_slow F4s (D_slow F4s) = U_fast F4s` and `bottom_layer_total_ok` holds.

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

and records the key bridge theorems we ultimately want:

- `fast_program_succeeds_imp_fast_ok` (**currently `cheat`ed**)
- `slow_program_succeeds_imp_dom_and_slow_ok` (**currently `cheat`ed**)
- `slow_program_succeeds_imp_dom_list_correct` (**currently `cheat`ed**)
- `slow_program_succeeds_imp_slow_ok` (**currently `cheat`ed**)
- `fast_and_slow_programs_succeed_imp_full_ok` (**CHEAT-tainted**, depends on cheated bridge lemmas)
- `fast_and_slow_programs_succeed_gives_equivalence` (**currently `cheat`ed**)

From these, we get a clean end-user theorem statement (intended to be derivable
once the bridge lemmas are proved without `cheat`):

- `fast_and_slow_programs_succeed_gives_equivalence`:
  if both programs succeed, then `U_slow = U_fast` and the bottom-layer
  invariants hold for `U_fast`.

#### `F4FPPVerifyRefinedBridgeGoalsTheory` (bridge at refined-obligation level)

File: `formal/hol4/F4FPPVerifyRefinedBridgeGoalsScript.sml`

Refines the bridge interface further: instead of “program success implies
`full_ok`”, it states (currently `cheat`ed) that:

- `fast_program_succeeds g dirac` implies
  `fast_semantic_ok g` and `bottom_layer_total_ok g dirac (U_fast g)`.
- `slow_program_succeeds g` implies
  `slow_refinement_ok g` and `slow_ok_components g`.

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
