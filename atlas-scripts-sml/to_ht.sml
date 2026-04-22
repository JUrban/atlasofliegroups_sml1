use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/to_ht.sml

  Purpose
  - Minimal SML analogue of `atlas-scripts/to_ht.at`.
  - The full Atlas `.at` implementation includes many optimized variants for
    truncating hermitian/c-forms and KL sums to a given “height” bound.
  - For the SML port we start with the essential primitive:
      truncate a `KTypePol` by term height.

  API
  - `ToHT.to_ht(pol, HT)`:
      returns a NEW `KTypePol` handle containing only terms with `height <= HT`.
      For `HT < 0`, returns a full copy (implemented via `Int.maxInt` cutoff).

  Ownership
  - The returned `ktypepol` is a fresh handle and must be freed with
    `KTypePol.free` by the caller.
*)

structure ToHT = struct
  type ktypepol = AtlasFFI.ktypepol
  type param = AtlasFFI.param

  (* `Foreign.cInt` marshaling requires the argument fit in a C `int`, so we
     use an explicit large cutoff rather than `Int.maxInt` (which can be 63-bit
     on Poly/ML builds). *)
  val hugeCutoff : int = 2147483647

  fun to_ht (pol: ktypepol, ht: int) : ktypepol =
    if ht < 0 then
      KTypePol.toHT (pol, hugeCutoff)
    else
      KTypePol.toHT (pol, ht)

  (*
    Unitarity “to height” predicates

    The original `.at` code uses truncated hermitian/c-forms (and related
    heuristics) to quickly DISPROVE unitarity at low height bounds.

    In the SML port we currently expose these predicates as exact checks via
    `AtlasFFI.atlas_param_is_unitary`, ignoring the height bounds. This is:
    - sound: never discards a truly unitary parameter
    - complete: returns the true Atlas unitarity answer
    - potentially slower than the `.at`-side optimized truncation heuristics
  *)

  fun is_unitary_to_ht (p: param, ht: int) : bool =
    let
      val _ = ht
    in
      AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1
    end

  fun is_unitary_to_hts (p: param, hts: int list) : bool =
    let
      val _ = hts
    in
      AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1
    end
end
