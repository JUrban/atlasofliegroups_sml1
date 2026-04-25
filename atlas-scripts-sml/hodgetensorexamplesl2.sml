use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/hodgetensorexamplesl2.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/hodgetensorexamplesl2.at`.
  - The `.at` script is a small worked example involving:
      - `hodge_tensor.at` and `hodge_function_std`
      - K-type enumerators (`K_parameters_norm_upto`)
      - various Hodge/K-type polynomial helpers.

  Status
  - Not yet implemented in SML because the Hodge-tensor library scripts are not
    yet ported to `atlas-scripts-sml/`.
*)

structure HodgeTensorExampleSL2 = struct
  fun run () : unit =
    raise Fail "hodgetensorexamplesl2: not yet ported (depends on hodge_tensor infrastructure)"
end

