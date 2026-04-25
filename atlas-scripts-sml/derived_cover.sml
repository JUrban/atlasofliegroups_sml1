use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/derived_cover.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/derived_cover.at`.
  - The `.at` script constructs derived simply-connected covers, provides KGB
    embedding/inverse-embedding helpers, and lifts/descends parameters between
    groups.

  Status
  - Not yet ported: this depends on `InnerClass`-level simply-connected cover
    data (`group_operations.at`), duality (`Vogan-dual.at`), and several
    matrix/torus-factor manipulations not currently exposed via the SML API.
*)

structure Derived_cover = struct
  fun TODO (_: string) : 'a =
    raise Fail "Derived_cover: not yet ported"
end

