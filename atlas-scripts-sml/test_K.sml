use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/test_K.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/test_K.at`.
  - The `.at` script is an internal consistency test for R-group/K-type
    counting identities using:
      - `R_K_dom`, `R_K_dom_mu`, `highest_weights`, `K_types`, and dimensions.

  Status
  - Not yet implemented in SML. It depends on unported parts of the `K.at`
    ecosystem (R-group computations and K-type dimension functions).
*)

structure TestK = struct
  fun run () : unit =
    raise Fail "test_K: not yet ported (depends on R-group + K-type dimension infrastructure)"
end

