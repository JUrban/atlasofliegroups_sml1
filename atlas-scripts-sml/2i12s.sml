use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/2i12s.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/2i12s.at`.
  - The `.at` script constructs a quotient root datum and an inner class using
    explicit matrices, then builds an extended parameter/block example.

  Status
  - Not yet implemented in SML because the required root-datum constructors
    (quotient basis / invert / root_datum-from-LieType+quotient) and extended
    parameter/block operations are not ported.
*)

structure Script2i12s = struct
  fun run () : unit =
    raise Fail "2i12s: not yet ported (depends on extended/root-datum quotient constructors)"
end

