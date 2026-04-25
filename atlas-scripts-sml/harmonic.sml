use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/harmonic.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/harmonic.at`.
  - The `.at` script computes multiplicities of Weyl-group irreps in the
    harmonic polynomials on the reflection representation, using character
    tables and symmetric powers.

  Current SML support gap
  - The existing SML `CharacterTables` port is deliberately minimal and does
    not provide:
      - `sym_power_refl(deg)` (symmetric powers of reflection character),
      - access to the complex root datum / number of positive roots,
      - the “degree” metadata computed from invariants.
  - As a result, the numerical content here is not yet ported.

  What is provided anyway
  - The combinatorial inclusion–exclusion skeleton (`partial_harm`) is included
    as an SML function parameterized by a multiplicity oracle `mult`.
  - All script-level entry points raise `Fail` for now.
*)

structure Harmonic = struct
  (* Inclusion–exclusion helper mirroring `partial_harm` from the `.at` script. *)
  fun partial_harm
    ( mult: int * int -> int
    , rep: int
    , deg: int
    , inv_degrees: int list
    ) : int =
    let
      fun is_even k = k mod 2 = 0
      fun sum xs = List.foldl (op +) 0 xs
      val n = length inv_degrees
      fun loopK k acc =
        if k > n then acc
        else
          let
            val subsets = Basic.choices_from (inv_degrees, k)
            fun addOne (S, a) =
              let
                val term = mult (rep, deg - sum S)
              in
                if is_even k then a + term else a - term
              end
            val acc' = List.foldl addOne acc subsets
          in
            loopK (k + 1) acc'
          end
    in
      loopK 0 0
    end

  (* Script-level API placeholders. *)
  fun invariants (ct: unit) : int list =
    let
      val _ = ct
    in
    raise Fail "Harmonic.invariants: not yet ported (requires sym_power_refl)"
    end

  fun harm (ct: unit, rep: int, deg: int) : int =
    let
      val _ = (ct, rep, deg)
    in
    raise Fail "Harmonic.harm: not yet ported (requires sym_power_refl)"
    end
end
