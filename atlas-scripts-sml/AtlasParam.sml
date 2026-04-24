use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/AtlasParam.sml

  Purpose
  - Thin wrapper around the Atlas C++ `Param` operations that are treated as
    *semantic primitives* by many scripts:
      - equality (Atlas' internal parameter equivalence),
      - hashing (compatible with equality).

  Why this exists
  - Keeping these operations behind a named API makes it easier to:
      - state “equivalence relation” assumptions explicitly (for HOL/CakeML),
      - swap in mock models for testing,
      - avoid sprinkling raw FFI calls throughout data structures like
        `ParamHash`.

  Notes
  - `eq` is the equality used by the Atlas interpreter and by `ParamHash`.
    It is not SML pointer equality.
*)

structure AtlasParam = struct
  type t = AtlasFFI.param

  fun eq (p: t, q: t) : bool = AtlasFFI.atlas_param_equal (p, q) = 1

  (* Hash a parameter into the range `0..m-1`. *)
  fun hash_mod (p: t, m: int) : int =
    let
      val h = AtlasFFI.atlas_param_hash (p, m)
    in
      if h < 0 then
        raise Fail ("AtlasParam.hash_mod failed: " ^ AtlasFFI.atlas_last_error ())
      else
        h
    end
end

