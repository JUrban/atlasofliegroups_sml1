use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryCache.sml";

structure FPP_faces_herm = struct
  type param = AtlasFFI.param

  type unitary_cache = BigUnitaryCache.t

  fun make_unitary_cache () : unitary_cache =
    BigUnitaryCache.create 4096

  fun is_unitary_hash_big_SIMPLE (cache: unitary_cache) (p: param) : bool =
    if AtlasFFI.atlas_param_is_hermitian p <> 1 then
      false
    else
      BigUnitaryCache.check_unitary cache p
end
