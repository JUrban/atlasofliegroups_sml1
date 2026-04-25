use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/K_type_formula.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/K_type_formula.at`.
  - The `.at` file re-implements and extends Atlas’s built-in
    `K_type_formula@KType` at the *parameter-polynomial* level, with additional
    warnings and intended generalizations to Hodge filtrations.

  Why this is currently a stub
  - The `.at` implementation depends on a broad collection of higher-level
    interpreter features that are not yet ported to SML in this repo:
      - `K_types_plus.at`, `is_normal.at`, `nilrad_roots`, root classification
        predicates (`is_complex`, `is_noncompact_imaginary`, ...),
      - `theta_induce_standard` and related parabolic/Levi construction,
      - character formula infrastructure (`character_formula_trivial`,
        `character_formula_one_dimensional`) requiring blocks across dual
        groups, one-dimensional tests, and dimensions.
  - The C++ library already provides the core built-in `K_type_formula@KType`
    (exposed in SML as `KType.K_type_formula : KType.ktype * int -> KTypePol`),
    so the immediate need for this `.at` script’s higher-level wrapper is low
    for the current verifier-focused workload.

  Plan for eventual completion
  - Once `K_types_plus`, Levi/parabolic support, and the required character
    formula and dimension primitives are ported (or exposed via FFI), this file
    can be upgraded to:
      - compute `character_formula_one_dimensional`,
      - implement `K_type_formula_plus`,
      - and provide the warning-enabled induction path.
*)

structure K_type_formula = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  val K_type_formula_verbose : bool ref = ref true

  fun character_formula_trivial (_: group) : unit =
    raise Fail "K_type_formula.character_formula_trivial: not yet ported"

  fun character_formula_one_dimensional (_: param) : unit =
    raise Fail "K_type_formula.character_formula_one_dimensional: not yet ported"

  fun K_type_formula_plus (_: AtlasFFI.ktype) : unit =
    raise Fail "K_type_formula.K_type_formula_plus: not yet ported"
end

