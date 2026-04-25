use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/aql.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/aql.at`.
  - The `.at` script relates annihilator/associated variety computations.

  Status
  - Not yet ported: depends on primitive ideals, cells, and associated variety
    infrastructure not currently implemented in SML.
*)

structure Aql = struct
  fun TODO (_: string) : 'a =
    raise Fail "Aql: not yet ported"
end

