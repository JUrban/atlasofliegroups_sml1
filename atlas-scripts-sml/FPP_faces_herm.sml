use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryCache.sml";

(*
  File: atlas-scripts-sml/FPP_faces_herm.sml

  Purpose
  - Minimal helper for face-based unitary checks: cache `is_unitary` queries but
    only after confirming the parameter is hermitian.

  Notes
  - The original `.at` code has more structure around face enumeration; in this
    SML port we mostly need the cached predicate.
*)
structure FPP_faces_herm = struct
  type param = AtlasFFI.param

  type unitary_cache = BigUnitaryCache.t

  (* Allocate a fresh unitary cache. *)
  fun make_unitary_cache () : unitary_cache =
    BigUnitaryCache.create 4096

  (* Cached unitary predicate with a fast hermitian short-circuit. *)
  fun is_unitary_hash_big_SIMPLE (cache: unitary_cache) (p: param) : bool =
    if AtlasFFI.atlas_param_is_hermitian p <> 1 then
      false
    else
      BigUnitaryCache.check_unitary cache p
end
