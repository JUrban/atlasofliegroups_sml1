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

  (* `Foreign.cInt` marshaling requires the argument fit in a C `int`, so we
     use an explicit large cutoff rather than `Int.maxInt` (which can be 63-bit
     on Poly/ML builds). *)
  val hugeCutoff : int = 2147483647

  fun to_ht (pol: ktypepol, ht: int) : ktypepol =
    if ht < 0 then
      KTypePol.toHT (pol, hugeCutoff)
    else
      KTypePol.toHT (pol, ht)
end
