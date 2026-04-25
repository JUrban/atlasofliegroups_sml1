use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/DiracCoh.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/DiracCoh.at`.
  - The `.at` script develops coherent continuation + Dirac related tools.

  Status
  - Not yet implemented in SML. This likely depends on coherent continuation
    infrastructure and KL/Hecke layers not yet ported.
*)

structure DiracCoh = struct
  fun run (_: AtlasFFI.group) : unit =
    raise Fail "DiracCoh.run: not yet ported"
end

