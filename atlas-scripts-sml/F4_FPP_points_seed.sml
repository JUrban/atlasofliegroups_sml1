use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/F4_FPP_points.sml";
use "atlas-scripts-sml/BigUnitaryHashStore.sml";

(*
  File: atlas-scripts-sml/F4_FPP_points_seed.sml

  Purpose
  - SML analogue of `atlas-scripts/F4_FPP_points.at`.
  - The `.at` script seeds Atlas’ global `big_unitary_hash` with the precomputed
    F4 folded-FPP point set by calling `big_unitary_hash.long_match(parameter(...), j)`
    repeatedly.

  What this module provides
  - `seed_F4_s_store()`:
      constructs `F4_s`, creates a `BigUnitaryHashStore`, and loads all points
      from `atlas-scripts-sml/data/F4_FPP_points.txt` into the store’s unitary
      hash for that group.

  Notes
  - This does not depend on the `.at` interpreter; it uses the same fixture file
    that other SML code uses for regression checks.
*)

structure F4_FPP_points_seed = struct
  type group = AtlasFFI.group
  type store = BigUnitaryHashStore.t

  fun seed_F4_s_store () : {g: group, store: store, rf: int} =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
      val () = if g = Foreign.Memory.null then raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ()) else ()
      val store = BigUnitaryHashStore.create ()
      val rf = BigUnitaryHashStore.rf_number store g
      val () =
        F4_FPP_points.foreachParam
          ( g
          , fn p => (ignore (BigUnitaryHashStore.long_match store (p, rf)); ())
          )
    in
      {g = g, store = store, rf = rf}
    end
end

