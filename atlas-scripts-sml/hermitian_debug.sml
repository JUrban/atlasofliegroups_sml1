use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hermitian_debug.sml

  Purpose
  - Translation scaffold for `atlas-scripts/hermitian_debug.at`.
  - See the `.at` file for intended Hermitian-form debugging utilities.

  Status
  - Not yet ported: this file currently defines only a placeholder structure.
*)

structure Hermitian_debug = struct
  fun TODO (_: string) : 'a =
    raise Fail "Hermitian_debug: not yet ported"
end

