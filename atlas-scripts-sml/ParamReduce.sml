use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamFinals.sml";

structure ParamReduce = struct
  type param = AtlasFFI.param

  fun addUniqueByEquivalent (p: param, acc: param list) : param list =
    if List.exists (fn q => AtlasFFI.atlas_param_equivalent (p, q) = 1) acc then
      (AtlasFFI.atlas_param_free p; acc)
    else
      p :: acc

  (* `.at`-style `reduce([Param])`:
     - normalise each parameter
     - expand into final parameters (`finals_for`)
     - keep one representative per equivalence class
     The returned params are freshly allocated and must be freed by caller. *)
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
                    raise Fail ("ParamReduce.reduce: normalise(final) failed: " ^ AtlasFFI.atlas_last_error ())
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

  fun freeAll (ps: param list) : unit =
    List.app AtlasFFI.atlas_param_free ps
end

