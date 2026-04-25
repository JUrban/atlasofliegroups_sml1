use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/by_dim.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/by_dim.at`.
  - The `.at` script organizes parameter computations “by dimension” and is
    used as a driver/report in some workflows.

  Status
  - Not yet ported: depends on unipotent/packet enumeration and report tooling.
*)

structure By_dim = struct
  fun TODO (_: string) : 'a =
    raise Fail "By_dim: not yet ported"
end

