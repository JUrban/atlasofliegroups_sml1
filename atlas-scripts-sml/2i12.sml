use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/2i12.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/2i12.at`.
  - The `.at` script is an interactive example that exercises:
      - extended parameters (`extended.at`, `extended_misc.at`)
      - Vogan duality (`Vogan-dual.at`)
      - Hecke algebra routines (`hecke.at`)
      - constructing quotient root data via `root_datum(Lie_type(...),P)` etc.

  Status
  - Not yet implemented in SML. The required “extended parameter” and Hecke
    layers are currently not ported to `atlas-scripts-sml/`.
  - This file exists so the repository has an `.sml` counterpart for the `.at`
    script and so `polyc`/Poly/ML can at least load/compile the module.
*)

structure Script2i12 = struct
  fun run () : unit =
    raise Fail "2i12: not yet ported (depends on extended/Vogan-dual/hecke infrastructure)"
end

