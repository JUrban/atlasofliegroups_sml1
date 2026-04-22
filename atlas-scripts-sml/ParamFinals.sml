use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/ParamFinals.sml

  Purpose
  - Convenience wrappers for the Atlas “finals_for” operation exposed via the
    FFI (`atlas_param_finals`).

  Ownership
  - `finals` returns cloned parameter handles (caller must free them).
  - `freeTerms` frees the parameter handles in a term list.
*)
structure ParamFinals = struct
  (* Compute the list of final parameters (with multiplicities) associated to `p`.
     Returns fresh clones; caller owns them. *)
  fun finals (p: AtlasFFI.param) : (AtlasFFI.param * int) list =
    let
      val h = AtlasFFI.atlas_param_finals p
      val () =
        if h = Foreign.Memory.null then
          raise Fail ("ParamFinals.finals: atlas_param_finals failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val n = AtlasFFI.atlas_paramlist_size h
      val () =
        if n < 0 then
          (AtlasFFI.atlas_paramlist_free h;
           raise Fail ("ParamFinals.finals: atlas_paramlist_size failed: " ^ AtlasFFI.atlas_last_error ()))
        else
          ()

      fun term i =
        let
          val mult = AtlasFFI.atlas_paramlist_mult (h, i)
          val q = AtlasFFI.atlas_paramlist_get_param_clone (h, i)
          val () =
            if q = Foreign.Memory.null then
              raise Fail
                ("ParamFinals.finals: atlas_paramlist_get_param_clone failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
        in
          (q, mult)
        end

      val terms =
        (List.tabulate (n, term)
         handle e => (AtlasFFI.atlas_paramlist_free h; raise e))
    in
      AtlasFFI.atlas_paramlist_free h;
      terms
    end

  (* Free the parameter handles returned by `finals`. *)
  fun freeTerms (terms: (AtlasFFI.param * int) list) : unit =
    List.app (fn (p, _) => AtlasFFI.atlas_param_free p) terms
end
