use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_faces_geom_fold.sml";

fun checkRatvecLen (n: int, u: {den: int, nums: int list}) =
  if length (#nums u) = n then () else raise Fail "bad ratvec length";

fun checkVecLen (n: int, v: int list) =
  if length v = n then () else raise Fail "bad vec length";

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);

val lines = FPP_faces_geom_fold.FPP_lines g;
val () = if length lines = 4 then () else raise Fail "FPP_lines: wrong count";
val () = List.app (fn u => checkRatvecLen (4, u)) lines;

val coroots = FPP_faces_geom_fold.FPP_coroots g;
val () = if length coroots = 4 then () else raise Fail "FPP_coroots: wrong count";
val () = List.app (fn v => checkVecLen (4, v)) coroots;

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

