use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/nci_nilrad_roots.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/nci_nilrad_roots.at`.
  - The `.at` script computes certain nilradical root sets used in
    non-compact-imaginary (NCI) situations.

  Status
  - Not yet ported: this depends on nilpotent/orbit and parabolic root-set
    utilities not currently present in the SML port.
*)

structure Nci_nilrad_roots = struct
  type group = AtlasFFI.group
  type rootdatum = AtlasFFI.rootdatum

  fun TODO (_: string) : 'a =
    raise Fail "Nci_nilrad_roots: not yet ported"
end

