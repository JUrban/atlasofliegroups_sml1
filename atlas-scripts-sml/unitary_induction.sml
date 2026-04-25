use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/unitary_induction.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/unitary_induction.at`.
  - The `.at` script attempts to recognize parameters as (real) parabolically
    induced from one-dimensional unitary representations.

  Status
  - Not yet implemented. The `.at` code relies on a substantial API surface
    that is not currently available in the SML port:
      - enumeration of real parabolics (`all_real_parabolics`)
      - construction of Levi-trivial parameters (`P.Levi.trivial`)
      - one-dimensional-unitary enumeration via `all_lambda_differential_0`
      - `real_induce_irreducible` and `monomials`
      - compact radical basis and related center computations

  Placeholder policy
  - The definitions are present (with documented intended meaning) so that other
    SML translations can refer to them, but they raise `Fail` until the missing
    induction/center infrastructure is ported.
*)

structure Unitary_induction = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  (* Intended: all 1-dimensional unitary reps of `G` (finite set under assumptions). *)
  fun all_one_dimensional_unitary (G: group) : param list =
    let
      val _ = G
    in
    raise Fail "Unitary_induction.all_one_dimensional_unitary: not yet ported"
    end

  (* Intended: enumerate parabolics and induce 1-dimensional Levi unitary reps. *)
  fun all_real_induced_one_dimensional (G: group) : (param * string) list =
    let
      val _ = G
    in
    raise Fail "Unitary_induction.all_real_induced_one_dimensional: not yet ported"
    end

  (* Intended: find Levi characters whose induced module contains `p`. *)
  fun realize_as_real_induced_from_one_dimensional (p: param) : param list =
    let
      val _ = p
    in
    raise Fail "Unitary_induction.realize_as_real_induced_from_one_dimensional: not yet ported"
    end
end
