use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/F4_FPP_lambdas.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/KTypePolHash.sml";
use "atlas-scripts-sml/FPP_localDirac.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

(* Smoke test for seeding graph classes from a known-unitary `ParamHash`.

   We:
   - compute a tiny list of *known unitary* parameters using `local_test_GEO_hash2_exact_limit`
   - insert one into a `ParamHash`
   - run `local_testK_hash_graph_exact_limit_known` and check that it executes and
     returns a well-formed `[[FaceVertsKHash]]` table. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val rank = AtlasFFI.atlas_group_rank g;
val kgbSize = AtlasFFI.atlas_group_kgb_size g;
val lambdasByX = F4_FPP_lambdas.loadRatvecs kgbSize;

fun findX i =
  if i >= kgbSize then raise Fail "no x with lambdas"
  else if null (Array.sub (lambdasByX, i)) then findX (i + 1)
  else i;

val x = findX 0;
val lambda = hd (Array.sub (lambdasByX, x));

val ps = FPP_localDirac.local_test_GEO_hash2_exact_limit (g, x, lambda, 1);
val _ = assert "need at least one known unitary" (not (null ps));
val p0 = hd ps;

val known = ParamHash.create 64;
val _ = ParamHash.match known p0;

val polHash = KTypePolHash.create 256;
val fd = FPP_localDirac.local_testK_hash_graph_exact_limit_known known (g, x, lambda, 1, polHash, ~1);

val _ = assert "dims" (length fd = rank + 1);

fun checkDim d =
  let
    val row = List.nth (fd, d)
    fun checkEntry e =
      let
        val _ = assert "entry length" (length e = d + 3)
        val idx = List.nth (e, d + 1)
        val lq = List.nth (e, d + 2)
        val _ = assert "idx >= 0" (idx >= 0)
        val _ = assert "idx in range" (idx < KTypePolHash.size polHash)
        val _ = assert "lq >= 0" (lq >= 0)
      in
        ()
      end
  in
    List.app checkEntry row
  end;

val () = List.app checkDim (List.tabulate (rank + 1, fn i => i));

val () = List.app AtlasFFI.atlas_param_free ps;
val () = ParamHash.freeAll known;
val () = KTypePolHash.freeAll polHash;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK\n";

