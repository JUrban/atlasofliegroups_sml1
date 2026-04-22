use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamFinals.sml";

(*
  File: atlas-scripts-sml/ParamReduce.sml

  Purpose
  - Utilities for deduplicating parameter lists up to Atlas equivalence, modeled
    after the `.at` helper `reduce([Param])` used in `K_highest_weights.at`.

  Ownership
  - Many functions here *consume* parameters on duplicate paths by freeing them.
  - Returned lists contain owned parameter handles; caller must free them (use `freeAll`).
*)
structure ParamReduce = struct
  type param = AtlasFFI.param

  (* Add `p` to `acc` unless it is equivalent to an existing element.
     Frees `p` on the duplicate path (ownership is consumed either way). *)
  fun addUniqueByEquivalent (p: param, acc: param list) : param list =
    if List.exists (fn q => AtlasFFI.atlas_param_equivalent (p, q) = 1) acc then
      (AtlasFFI.atlas_param_free p; acc)
    else
      p :: acc

  (* `.at`-style `reduce([Param])` from `atlas-scripts/K_highest_weights.at`:
     keep one representative per equivalence class (after normalisation).
     The returned params are freshly allocated and must be freed by caller. *)
  (* Normalize each parameter and keep one per equivalence class. *)
  fun reduce (ps: param list) : param list =
    let
      fun reduceOne (p: param, acc: param list) : param list =
        let
          val q = AtlasFFI.atlas_param_normalise p
          val () =
            if q = Foreign.Memory.null then
              raise Fail ("ParamReduce.reduce: normalise failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
        in
          addUniqueByEquivalent (q, acc)
        end
    in
      List.rev (List.foldl reduceOne [] ps)
    end

  (* Variant sometimes useful in script ports: expand each input into `finals_for`
     and then deduplicate by equivalence (after normalisation). *)
  (* Reduce after expanding each input via `finals_for`. *)
  fun reduceFinals (ps: param list) : param list =
    let
      fun reduceOne (p: param, acc: param list) : param list =
        let
          val q = AtlasFFI.atlas_param_normalise p
          val () =
            if q = Foreign.Memory.null then
              raise Fail ("ParamReduce.reduceFinals: normalise failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
          val finals = ParamFinals.finals q
          val () = AtlasFFI.atlas_param_free q

          fun addFinal ((r, mult), acc2) =
            if mult = 0 then
              (AtlasFFI.atlas_param_free r; acc2)
            else
              let
                val r2 = AtlasFFI.atlas_param_normalise r
                val () = AtlasFFI.atlas_param_free r
                val () =
                  if r2 = Foreign.Memory.null then
                    raise Fail ("ParamReduce.reduceFinals: normalise(final) failed: " ^ AtlasFFI.atlas_last_error ())
                  else
                    ()
              in
                addUniqueByEquivalent (r2, acc2)
              end
        in
          List.foldl addFinal acc finals
        end
    in
      List.rev (List.foldl reduceOne [] ps)
    end

  (* Free a list of parameter handles. *)
  fun freeAll (ps: param list) : unit =
    List.app AtlasFFI.atlas_param_free ps
end
