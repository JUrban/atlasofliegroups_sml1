use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryHash.sml";
use "atlas-scripts-sml/FPPFlags.sml";

structure FPP_globalDirac = struct
  fun FPP_unitary_hash_bottom_layer (_: AtlasFFI.group, _: BigUnitaryHash.t) : unit =
    if !FPPFlags.final_verbose then
      TextIO.print "FPP_unitary_hash_bottom_layer: TODO (bindings + port)\n"
    else
      ()
end
