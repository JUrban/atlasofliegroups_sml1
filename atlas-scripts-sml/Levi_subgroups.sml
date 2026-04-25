use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/Levi_subgroups.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/Levi_subgroups.at`.
  - The `.at` script classifies standard Levi subgroups up to Weyl conjugacy and
    provides lookup tables (`orbits_table`, `Levi_lookup`, etc.) used by
    `W_classes.at`, `conjugate.at`, and other Weyl-group conjugacy modules.

  Status
  - Not yet implemented in SML. A proper port will likely require:
      - a clear SML-side representation of Levi data (subsets of simple roots),
      - Weyl-group orbit enumeration on subsets,
      - and/or direct reuse of existing hard-coded tables where appropriate.
*)

structure Levi_subgroups = struct
  type rootdatum = RootDatum.t

  fun orbits_table (_: rootdatum) : unit =
    raise Fail "Levi_subgroups.orbits_table: not yet ported"
end
