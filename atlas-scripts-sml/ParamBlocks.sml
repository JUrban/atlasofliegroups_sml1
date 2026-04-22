use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/ParamBlocks.sml

  Purpose
  - Convenience wrappers for Atlas “block” computations exposed via the FFI.

  Atlas correspondence
  - In the Atlas interpreter, `block(p)` returns a pair:
      ( [Param] survivors_in_block(p), int start_pos )
    where `start_pos` is the index of `p` in the returned survivor list (or -1).
  - `basic.at` defines `block_of(p)` as the first component:
      `let (params,)=block(p) in params`.

  FFI surface
  - `atlas_param_block_survivors(p)` returns a `ParamListHandle` storing the
    survivor parameter list; the handle also stores `start_pos`.
  - `atlas_paramlist_start_pos(handle)` extracts that index.

  Ownership
  - `block_survivors` returns fresh cloned parameter handles (caller must free them).
  - `freeTerms` frees the parameter handles in a term list.
*)

structure ParamBlocks = struct
  type param = AtlasFFI.param

  (* Compute the survivor list and the start position for `p`’s full block.
     Returns fresh clones; caller owns them.

     The returned list entries have multiplicity 1. *)
  fun block_survivors (p: param) : (param * int) list * int =
    let
      val h = AtlasFFI.atlas_param_block_survivors p
      val () =
        if h = Foreign.Memory.null then
          raise Fail ("ParamBlocks.block_survivors: atlas_param_block_survivors failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val startPos = AtlasFFI.atlas_paramlist_start_pos h
      val n = AtlasFFI.atlas_paramlist_size h
      val () =
        if n < 0 orelse startPos < ~1 then
          (AtlasFFI.atlas_paramlist_free h;
           raise Fail ("ParamBlocks.block_survivors: bad sizes: " ^ AtlasFFI.atlas_last_error ()))
        else
          ()

      fun term i =
        let
          val mult = AtlasFFI.atlas_paramlist_mult (h, i)
          val q = AtlasFFI.atlas_paramlist_get_param_clone (h, i)
          val () =
            if q = Foreign.Memory.null then
              raise Fail ("ParamBlocks.block_survivors: get_param_clone failed: " ^ AtlasFFI.atlas_last_error ())
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
      (terms, startPos)
    end

  (* Free the parameter handles returned by `block_survivors`. *)
  fun freeTerms (terms: (param * int) list) : unit =
    List.app (fn (q, _) => AtlasFFI.atlas_param_free q) terms
end

