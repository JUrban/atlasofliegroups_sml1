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
  - `dom_list_from_components_correct` (**currently `cheat`ed**)

This is meant to be discharged by routine list reasoning once we decide what
the concrete enumeration functions are (SML, Atlas FFI, fixtures, etc.).

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

The CakeML side models this as `ParamHashProgTheory` + goal lemmas in
`ParamHashGoalsTheory`.

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
