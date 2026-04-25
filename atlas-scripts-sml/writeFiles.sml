use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/writeFiles.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/writeFiles.at`.
  - The `.at` script prints Atlas-readable `.at` snippets for large data
    structures (vectors, KTypePol lists, ParamPol lists, etc.) to reduce memory
    pressure when reloading.

  Status
  - Not yet ported: implementing Atlas-readable writers in SML is feasible but
    needs careful agreement on formats and on which objects (RealForm/KTypePol/
    ParamPol) are supported without relying on the Atlas interpreter.
*)

structure WriteFiles = struct
  fun TODO (_: string) : 'a =
    raise Fail "WriteFiles: not yet ported"
end

