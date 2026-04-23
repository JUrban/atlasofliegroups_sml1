use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/KTypePolHash.sml";
use "atlas-scripts-sml/FPP_localDirac.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

(* Smoke test for the `localGraphK_FDKH*` wrappers.

   We build a synthetic `[[FaceVertsKHash]]`-style table `fd`:
   - dim0 entries are [v, charIdx, langlandsCount]
   - dim1 entries are [v0, v1, charIdx, langlandsCount]

   Then we check that `localGraphK_FDKH_kpol` groups a 1-face with its vertex
   when the truncated KTypePols match at the requested height. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val mu = KType.ofParam p;

val polBig = KType.K_type_formula (mu, 30);
val polSmall = KTypePol.toHT (polBig, 10);
val polOther = KTypePol.scaleSplit (polSmall, 2, 0);

val polHash = KTypePolHash.create 64;
val idxSmall = KTypePolHash.match polHash polSmall;
val idxBig = KTypePolHash.match polHash polBig;
val idxOther = KTypePolHash.match polHash polOther;

val fd : int list list list =
  [ [[0, idxSmall, 1], [1, idxSmall, 1], [2, idxOther, 2]]
  , [[0, 1, idxBig, 1], [1, 2, idxOther, 2]]
  ];

val (gd, _, _) = FPP_localDirac.localGraphK_FDKH_kpol (fd, polHash, 10, 0);
val comps = FaceClasses.class_of gd;
val idx = FaceClasses.index_f fd;

val v0 = idx (0, 0);
val v1 = idx (0, 1);
val v2 = idx (0, 2);
val e01 = idx (1, 0);
val e12 = idx (1, 1);

val c1 = Array.sub (comps, v0);
val _ = assert "v1 in same SCC as v0" (Array.sub (comps, v1) = c1);
val _ = assert "e01 in same SCC as v0 (trunc eq)" (Array.sub (comps, e01) = c1);

val c2 = Array.sub (comps, v2);
val _ = assert "e12 in same SCC as v2" (Array.sub (comps, e12) = c2);
val _ = assert "SCCs distinct" (c1 <> c2);

val () = KTypePolHash.freeAll polHash;
val () = KTypePol.free polBig;
val () = KTypePol.free polSmall;
val () = KTypePol.free polOther;
val () = KType.free mu;
val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK\n";
