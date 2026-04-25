use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/arthur_parameters.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/arthur_parameters.at`.
  - The `.at` script provides utilities around Arthur parameters and related
    packet constructions.

  Status
  - Not yet ported: this functionality relies on large parts of the Atlas
    interpreter’s packet/unipotent/Arthur-parameter libraries which are not
    currently available through the SML FFI or SML ports.
*)

structure ArthurParameters = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun TODO (_: string) : 'a =
    raise Fail "ArthurParameters: not yet ported"
end

