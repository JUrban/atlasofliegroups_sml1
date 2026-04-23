# VERIFY_ESTIMATE.md — Formal equivalence plan and cost estimate

## Goal (what “equivalence” should mean here)

We have two SML programs:

- `atlas-scripts-sml/VerifyF4FPP.sml` (fast): constructs a parameter hash for `F4_s`, then runs a suite of checks on the resulting set (standard/final, lambda-table consistency, hermitian/unitary, twist-equivalence, contragredient closure).
- `atlas-scripts-sml/SimplerVerifyF4FPP.sml` (slow): brute-force enumerates many triples `(x,lambda,gamma)` and checks that any unitary final-term discovered is already present in the known unitary set.

These are not “equivalent” in the sense of same runtime behavior:

- The fast program is primarily a **soundness + consistency** checker for a computed set `U_fast`.
- The slow program is primarily a **completeness** cross-check: it tries to show there is no “missing” unitary object outside `U_fast` (relative to a specific enumeration domain).

So the formal statement we actually want is more like:

> For the specific real form `F4_s`, the fast-generated set `U_fast` is
> (1) **sound**: every element of `U_fast` is unitary (and passes the other invariants), and
> (2) **complete** relative to the slow program’s search domain `D_slow`:
>     every unitary final term produced by the slow domain is in `U_fast`.

In symbols, if we define:

- `U_fast` = the set of parameters stored in the `ParamHash` computed by `F4_FPP_points_compute.computeAllIntoParamHash` (with the flags used by `VerifyF4FPP.run()`), possibly closed under whatever equivalence `atlas_param_equal` implements.
- `D_slow` = `{ (x,lambda,gamma) | x ∈ KGB(F4_s), lambda ∈ FPP_lambdas(x), gamma ∈ AllBarycenters }`.
- `U_slow` = `{ pi | ∃(x,lambda,gamma)∈D_slow. pi = first_final_term(normalise(param(x,lambda,gamma))) ∧ is_unitary(pi) }`.

Then the target is:

1. `U_fast ⊆ U_slow` (soundness w.r.t. brute semantics), plus the fast program’s extra invariants.
2. `U_slow ⊆ U_fast` (completeness), i.e. what the slow script is trying to establish empirically.

The remainder of this document estimates what it would take to make such statements precise and prove them in HOL4 or CakeML, and proposes staged “proof strengthening”.

---

## Reality check: the biggest obstacle is the Atlas C++ library

Both programs crucially depend on the Atlas C++ implementation via FFI (`atlas-scripts-sml/ffi/AtlasFFI.sml` + `atlas-scripts-sml/ffi/atlas_smlffi.cpp`), for:

- group data (`KGB` size, involution matrices, root datum, etc.),
- parameter construction (`atlas_param_new_from_lambda_nu_text`),
- normalization/finalization (`atlas_param_normalise`, `atlas_param_finals`),
- semantic predicates (`atlas_param_is_unitary`, `atlas_param_is_hermitian`, etc.),
- equality/hash (`atlas_param_equal`, `atlas_param_hash`).

Formally verifying “equivalence” end-to-end would therefore require either:

1. **Axiomatizing** the behavior of these FFI calls (treating them as trusted oracles with specifications), or
2. **Proving** enough of the Atlas C++ library to justify those specs, or
3. **Re-implementing** the needed parts of Atlas in a verified setting (usually infeasible at this scale).

Practically, any near-term HOL4/CakeML effort will start with (1), then gradually reduce the trusted base where possible (typically for the purely arithmetic/data-structure parts).

---

## Recommended verification strategy: refinement + staged assumptions

### Stage A: prove the *SML algorithmic skeletons* correct under abstract specs

Prove the fast and slow programs implement their intended set-transformations, assuming:

- Abstract type `param`, with abstract functions:
  - `mk_param : x × lambda × gamma → param` (or `nu` if you prefer),
  - `normalise : param → param`,
  - `final_terms : param → (param × mult) list`,
  - `is_unitary : param → bool`,
  - `is_hermitian : param → bool`,
  - `equal : param → param → bool` and `hash_code : param → int → int`,
  - and similarly for group-level structure used by the code.
- Determinism/totality properties where needed (or explicit option/error behavior).

Then:

- Define a mathematical `U_fast_spec` (what the fast pipeline is *supposed* to compute).
- Define `U_slow_spec` (what the brute-force check enumerates/tests).
- Prove `VerifyF4FPP.run()` succeeds ⇒ `U_fast` satisfies the claimed invariants (soundness).
- Prove `SimplerVerifyF4FPP.runSlow()` prints no errors ⇒ `U_slow ⊆ U_fast` (completeness *relative to the shared domain definition*).

This gives a very useful theorem *even with large axioms* because it pins down precisely what would need to be assumed about Atlas.

### Stage B: reduce trusted assumptions for the “pure” parts (ratvec/matrix/hash/bucketing)

Much of the speedup in `VerifyF4FPP` comes from algebraic filtering and bucketing:

- rational vector normalization and arithmetic (`atlas-scripts-sml/Lattice.sml`),
- keying by `(I+theta)*gamma` and `(I+theta)*lambda` (`F4_FPP_points_compute.sml` and `FPP_localDirac.sml`),
- hash-table behavior (`atlas-scripts-sml/hash.sml`, `atlas-scripts-sml/ParamHash.sml`),
- combinatorics (indexing into arrays/lists).

These are far more amenable to direct formalization and proof (either in HOL4 directly or in CakeML).

So the stage-B goal is:

- Prove that the *bucket selection logic* returns exactly the same gamma set as the brute-force “scan all barycenters and test the constraint” would.
- Prove the table-loading variants (fixtures in `atlas-scripts-sml/data/*`) are extensionally equal to their computed definitions, under a “fixtures match generator” assumption (or by proving the generator and then proving the parser returns the same list).

This reduces the trusted base from “Atlas decides everything” to “Atlas decides the representation theory primitives; the SML code around it is proven”.

### Stage C: connect the two specs (fast ⊆ slow and slow ⊆ fast)

Once you have precise specs and the bucketing lemma, the remaining “hard” part is the link:

> Every triple `(x,lambda,gamma)` that the slow program considers corresponds to exactly one parameter that the fast program either inserts or safely rejects.

This breaks down into smaller lemmas:

- The slow program’s set of barycenters matches the fast program’s barycenter enumeration.
- The slow program’s lambda enumeration matches the fast program’s lambda enumeration.
- The “parameter from `(x,lambda,gamma)`” construction is aligned (same `nu` formula, same normalization, same choice of representative final term).
- The fast program’s additional filters (standard/final/hermitian/unitary flags) match the slow program’s predicate (or the slow program must be strengthened to test the same predicate).

At this stage you can prove:

- `U_fast ⊆ U_slow` (if the fast enumerates only those triples and uses the same acceptance predicate),
- and/or `U_slow ⊆ U_fast` (if the fast is complete relative to the slow’s domain).

In practice you will likely choose one direction first (usually soundness), then completeness.

---

## HOL4 vs CakeML: tradeoffs

### HOL4 (deep embedding or shallow reasoning)

Pros:
- Excellent for algebraic proofs and refinement arguments.
- Flexible axiomatization of FFI calls and abstract datatypes.

Cons:
- Proving properties “about SML code” is non-trivial unless you:
  - translate the relevant code into HOL functions, or
  - reimplement the core algorithms directly in HOL and prove the SML matches by inspection/refactoring.

Typical approach in HOL4 here:
- Define a *mathematical model* of the computation (sets of triples → sets of params).
- Prove theorems about that model.
- Then refactor the SML so that the core functions correspond closely to the model (making the final “code-to-model” argument feasible).

### CakeML (extraction/compilation + functional correctness)

Pros:
- Designed for end-to-end verified compilation of ML-like programs.
- Supports a verified story for “program implements spec”, including imperative features (refs/arrays).
- Has an FFI story where external calls can be given abstract specs.

Cons:
- The codebase currently targets Poly/ML + `Foreign.*` FFI; CakeML’s FFI interface is different.
- You would likely need:
  - a CakeML-friendly wrapper layer for Atlas calls, and
  - to isolate the verified core into a CakeML-compilable subset.

Typical approach in CakeML here:
- Extract/port the *pure* and *algorithmic* core (bucketing, hashing, traversal) into CakeML.
- Treat Atlas calls as FFI primitives with specs.
- Prove that the compiled program satisfies a top-level spec.

Given the heavy reliance on Atlas C++ internals, a realistic first deliverable is:

> CakeML proof that the **fast** program’s computed set equals the set defined by its *abstract spec*, assuming the Atlas FFI functions satisfy their specs.

Then gradually tighten the FFI specs or validate them empirically.

---

## A staged “proof strengthening” roadmap

Below is a pragmatic sequence that yields meaningful results early.

### Step 0: refactor to make both programs “spec-friendly”

Recommended code changes (not strictly required, but they lower proof cost dramatically):

1. **Separate computation from I/O**
   - Have functions return structured results (`bool`/`string list`/`ParamHash.t`) rather than printing directly.
2. **Expose explicit specs as SML functions**
   - For example, define `SimplerVerifyF4FPP.domain : group -> triple list` (or an iterator) and `SimplerVerifyF4FPP.step : triple -> outcome`.
3. **Make “equivalence relation” explicit**
   - Wrap `atlas_param_equal` in a named predicate and document intended semantics.
4. **Factor the common “param construction from (x,lambda,gamma)”**
   - Ensure slow and fast share exactly the same function for `nu = gamma - (I+theta)lambda/2`.

This is mostly mechanical but pays off.

### Step 1 (assumption-heavy): formalize the domain and set semantics

In HOL4 or CakeML:

- Define `U_slow_spec` and `U_fast_spec` as pure set functions using abstract `param` operations.
- Prove each SML program implements its spec (or a refined version with caching/hashing).

Trusted base: essentially all Atlas semantics.

Expected effort: ~2–4 weeks if the code is refactored as above; longer if done “as-is”.

### Step 2 (reduce trust): verify bucketing and key-matching logic

Prove that:

- The fast program’s gamma-bucket selection is equivalent to a brute-force predicate on `gamma`.
- The use of fixtures is equivalent to the computed enumerators (either by proof, or by making fixtures part of the trusted assumptions).

Trusted base: still Atlas param semantics, but not the arithmetic/hashing logic.

Expected effort: ~3–6 weeks, depending on how much of `Lattice.sml` and `hash.sml` you bring into the formal world.

### Step 3 (completeness argument): connect slow enumeration to fast insertion

This is the “real equivalence” stage:

- Show that for every triple in the slow domain, the fast code either:
  - constructs an equivalent parameter and inserts it, or
  - rejects it for a reason that implies it cannot contribute a new unitary final term.

This stage requires the clearest specification of:
- what “finalize/first final term” means,
- what “unitary” means,
- and what set the fast program claims to represent.

Trusted base: the “representation theory” pieces in Atlas remain axioms.

Expected effort: ~1–3 months (this is where most time goes).

### Step 4 (optional, advanced): shrink the trusted base further

Candidates:

- Validate/verify parts of `atlas_param_equal`/`atlas_param_hash` coherence properties (e.g. if equal then hashes match mod m).
- Prove invariants about `ParamHash` correctness (no false negatives under equality) and memory/ownership doesn’t affect extensional results.
- Potentially verify a small standalone “parameter model” for the subset of operations needed (very ambitious).

Expected effort: open-ended; months to years if you try to verify deep Atlas semantics.

---

## Concrete proof obligations (what you will actually need)

Even with axiomatized Atlas calls, you will need the following kinds of lemmas:

1. **Hash-set correctness**
   - `ParamHash.match` implements set insertion under `atlas_param_equal`.
   - `ParamHash.lookup` is correct w.r.t. the same equality.
2. **Bucketing correctness**
   - The computed key function is stable under normalization.
   - The gamma selection criterion is equivalent to the predicate used by the slow domain (or to the intended mathematical condition).
3. **Final-term selection alignment**
   - The slow program’s `first_final_term` matches the `.at` notion used in the original brute-force script.
4. **Unitary predicate alignment**
   - `is_unitary` in the slow check must match the unitarity filter used in the fast generator (or the theorem must quantify the difference).
5. **Fixture equivalence**
   - If fixtures are used, you either:
     - prove fixtures equal generated lists, or
     - state an explicit assumption “fixtures match generator”.

---

## “5 minute” practical check (non-formal, but useful)

You already have a built-in fast partial checker in:

- `atlas-scripts-sml/simpler_script_to_verify_F4_FPP_unitary_dual.sml`

with:

- `ATLAS_RUN_FAST_SIMPLER_VERIFY=1` (bounded sampling).

This is not a proof, but it is a good regression tool while refactoring for verification.

If we want a more meaningful fast check without going full brute-force, a good next step is:

- Use the **graph-based class propagation** pipelines in `atlas-scripts-sml/FPP_localDirac.sml`
  (especially the ToHT-then-exact variant) to drastically reduce the number of exact unitarity calls,
  and then compare against `U_fast` for sampled `(x,lambda)` pairs.

This would likely stay under minutes and gives high confidence.

---

## Estimated effort summary (very rough)

Assuming a researcher/engineer familiar with HOL4/CakeML and willing to refactor code:

- **Stage A (assumption-heavy correctness of both programs):** 1–2 months
- **Stage B (verify arithmetic/hashing/bucketing):** 1–2 months
- **Stage C (prove the real completeness/soundness connection):** 2–4 months
- **Stage D (shrink Atlas trusted base):** not realistically bounded

If code is not refactored into spec-friendly modules, multiply by ~2.

---

## Recommended next actions (to make formalization feasible)

1. Refactor `VerifyF4FPP.run()` and `SimplerVerifyF4FPP.runSlow()` to return structured results instead of printing.
2. Introduce a shared module for:
   - `(x,lambda,gamma) ↦ param`,
   - final-term selection policy,
   - equality semantics (naming `atlas_param_equal` as `ParamEq` and documenting intent).
3. Add a “spec driver” that computes `U_fast_spec` and `U_slow_spec` as pure sets given abstract operations.
4. Decide early whether the formal target is:
   - “equivalence under axiomatized Atlas semantics” (recommended), or
   - “verify a significant part of Atlas” (likely too large).

---

## A more precise (and achievable) equivalence statement

If the immediate objective is “the fast and slow programs agree”, a good first theorem is:

> Assuming Atlas FFI functions are deterministic and satisfy basic coherence properties (listed below),
> if `SimplerVerifyF4FPP.runSlow()` emits no “IT'S ALL WRONG!!!” line, then
> every unitary final term discovered in its enumeration is already present in the hash computed by
> `F4_FPP_points_compute.computeAllIntoParamHash`.

This matches what the slow program is operationally doing today and avoids over-claiming.

The stronger theorem (the one people usually *mean*) is:

> For the same group `F4_s` and the same domain `D_slow`, the set computed by the fast generator
> is exactly the set of unitary final terms obtainable from `D_slow` (modulo `atlas_param_equal`).

To reach that, you need to verify (or assume) that the fast generator’s pruning/bucketing does not
exclude any unitary objects in `D_slow`.

---

## “FFI axioms” checklist (what you must assume, or prove about Atlas)

Even for an assumption-heavy Stage A proof, you will need to write down explicit assumptions about
FFI calls, or you will not be able to state theorems precisely enough to be useful.

Minimum useful assumptions (informal):

1. **Determinism**
   - All functions used by the scripts are deterministic (no hidden global state affects results).
2. **Equality is a congruence**
   - If `atlas_param_equal p q` then:
     - `atlas_param_is_unitary p = atlas_param_is_unitary q`
     - `atlas_param_is_final p = atlas_param_is_final q`
     - `atlas_param_finals p` enumerates a list equivalent to `atlas_param_finals q` (up to `equal`).
3. **Hash coherence**
   - `atlas_param_equal p q` implies `atlas_param_hash p m = atlas_param_hash q m`
     (or at least that the `ParamHash` implementation remains extensionally correct under collisions).
4. **Normalization/finalization coherence**
   - `atlas_param_normalise` preserves the represented object up to `equal`.
   - `ParamFinals.finals` returns exactly the (multi)set of final terms (with multiplicities) associated
     to its input, up to `equal`.
   - The “first final term” policy used in `SimplerVerifyF4FPP.first_final_term` is consistent with the
     policy assumed by the fast pipeline (or else the theorem must quantify the difference explicitly).
5. **Group structure coherence**
   - `atlas_group_kgb_involution_matrix_text` and parsing (`AllParameters.parseInvolutionMatrixText`)
     agree on the intended involution matrix; similarly for ranks, KGB sizes, etc.

In HOL4/CakeML you would encode these as abstract function specs or axioms; later you can try to
weaken them or validate them by cross-checking.

---

## Code-centric refactoring checklist (reduces proof effort a lot)

The two main code points that should become shared, named definitions are:

- `(x,lambda,gamma) ↦ param`:
  - Currently implemented in `atlas-scripts-sml/SimplerVerifyF4FPP.sml` as
    `SimplerVerifyF4FPP.param_of_x_lambda_gamma`.
  - The fast pipeline builds the same `nu` via `nu = gamma - (I+theta)lambda/2` inside
    `atlas-scripts-sml/F4_FPP_points_compute.sml` (and related helpers).
- “Final-term selection policy”:
  - Currently implemented in `atlas-scripts-sml/SimplerVerifyF4FPP.sml` as
    `SimplerVerifyF4FPP.first_final_term`.

For formal equivalence, it is extremely helpful if both programs call the same shared functions for
these two pieces, so the proof does not have to reason about “two almost-identical definitions”.

Also recommended:

- Make `VerifyF4FPP.run` and `SimplerVerifyF4FPP.runSlow` return values (errors as data) instead of
  printing and raising exceptions. This is not required, but it makes formal semantics and testing
  cleaner.

---

## Suggested incremental theorem set (what to prove first)

1. **ParamHash correctness** (pure SML)
   - Prove that `ParamHash` implements a finite set w.r.t. `atlas_param_equal` and does not produce
     false negatives on `lookup` after `match`/insert.
2. **Slow checker meaning** (assumption-heavy but tight)
   - Prove that the slow checker is exactly a subset check:
     `no_error_output ⇒ U_slow ⊆ U_fast`, where `U_fast` is the precomputed hash.
3. **Fast checker meaning** (assumption-heavy but tight)
   - Prove that `VerifyF4FPP.run()` implies the computed hash satisfies its claimed invariants
     (closure properties, invariants about normalization/finality, etc.).
4. **Refinement/equivalence** (the real result)
   - Prove `U_fast ⊆ U_slow` and `U_slow ⊆ U_fast` relative to the chosen domain and equality notion.

These deliver usable “confidence theorems” early, before the most expensive completeness arguments.
